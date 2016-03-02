-module(exmpp_core).

-export([create/1,
         open_stream/2,
         close_stream/2,
         parse/2,
         event/2
        ]).

-export([get_state_for/2,
         set_state_for/3,
         iq_ns/4]).

% Implemented in this module
-define(VERSION, <<"1.0">>).
-define(NS, <<"jabber:client">>).
-define(NS_STREAM, <<"http://etherx.jabber.org/streams">>).

% Implemented elsewhere
-define(NS_SASL, <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
-define(NS_BIND, <<"urn:ietf:params:xml:ns:xmpp-bind">>).
-define(NS_SESS, <<"urn:ietf:params:xml:ns:xmpp-session">>).

-type stream_state() :: pending | open | wait_stanza.

-record(stream, {
          id :: binary()|undefined,
          parser :: exmpp_xml:parser(),
          ref :: reference()|undefined,
          to :: binary(),
          state :: stream_state(),
          ns_state = #{} :: #{ns() => ns_state()},
          iq_pending = #{},
          pending :: boolean(),
          stanza :: [exmpp_xml:el()]
         }).

-record(state, {
          astream :: #stream{}|undefined,
          streams = [] :: [#stream{}]
         }).

-opaque xmpp() :: #state{}.
-type open_req() :: #{to=>binary()}.
-type iq_req() :: #{id=>binary(),
                    type=>binary()}|
                  #{id=>binary(),
                    type=>binary(),
                    attrs=>[#{binary()=>binary()}]}.
-type ns() :: binary().
-type ns_state() :: any().

-export_type([xmpp/0,
              open_req/0]).

create(#{}) -> #state{}.

-spec get_state_for(NS, State) -> NSState when
      NS :: ns(),
      State :: xmpp(),
      NSState :: ns_state().
get_state_for(NS, #state{astream=Stream}) ->
    maps:get(NS, Stream#stream.ns_state, undefined).

-spec set_state_for(NS, NSState, State) -> State when
      NS :: ns(),
      NSState :: ns_state(),
      State :: xmpp().
set_state_for(NS, NSState, #state{astream=Stream}=State) ->
    NsState1 = (Stream#stream.ns_state)#{NS => NSState},
    State#state{astream=Stream#stream{ns_state=NsState1}}.

-spec iq_ns(Req, Childs, Ns, State) -> Result when
      Req :: iq_req(),
      Childs :: exmpp_xml:els(),
      Ns :: ns(),
      Result :: Ok,
      Ok :: {Element, State},
      Element :: exmpp_xml:els(),
      State :: xmpp().
iq_ns(Req, Childs, Ns, #state{astream=Stream}=State) ->
    {Id, El} = iq_el(Req, Childs),
    IqPending = State#state.astream#stream.iq_pending,
    IqPending1 = IqPending#{Id => Ns},
    {El, State#state{astream=Stream#stream{iq_pending=IqPending1}}}.

iq_el(#{id:=Id,type:=Type}=Req, Childs) ->
    DefaultAttrs = #{<<"type">> => Type,<<"id">> => Id},
    Attrs = maps:merge(maps:get(attrs, Req, #{}), DefaultAttrs),
    {Id,[#{name => <<"iq">>,attrs => Attrs,childs => Childs}]}.

-spec open_stream(Open, State) -> Result when
      Open :: open_req(),
      State :: xmpp(),
      Result :: {Element, Ref, State},
      Element :: iolist(),
      Ref :: reference().
open_stream(Open,#state{astream=#stream{}=Stream,streams=Streams}=S) ->
    open_stream(Open, S#state{astream=undefined,streams=[Stream|Streams]});
open_stream(#{to:=To},State) ->
    S = [#{name => <<"stream:stream">>, type => stream_start,
           attrs => #{<<"to">> => To,
                      <<"version">> => ?VERSION,
                      <<"xmlns">> => ?NS,
                      <<"xmlns:stream">> => ?NS_STREAM}}],
    Ref = erlang:make_ref(),
    lager:debug([{ref,Ref},{to,To}], "Opening stream"),
    Stream = #stream{state=pending,parser=exmpp_xml:parser(),
                     to=To,ref=Ref},
    {exmpp_xml:encode(S), Ref, State#state{astream=Stream}}.

close_stream(#{},State) ->
    S = [#{name=><<"stream:stream">>,type=>stream_end}],
    State1 = maybe_replace_astream(State),
    {exmpp_xml:encode(S), State1}.

maybe_replace_astream(#state{streams=[]}=State) ->
    State#state{astream=undefined};
maybe_replace_astream(#state{streams=[Stream|Streams]}=State) ->
    State#state{astream=Stream,streams=Streams}.

-spec parse(Data, State) -> Result when
      Data :: binary(),
      State :: #state{},
      Result :: Ok | Error,
      Ok :: {Events, State},
      Events :: exmpp_xml:els()|heartbeat,
      Error :: {error, term()}.
parse(<<" ">>, State) -> {heartbeat, State};
parse(Data, #state{astream=#stream{parser=Parser}=Stream}=S) ->
    case exmpp_xml:parse(Data, Parser) of
        {ok, Events, Parser1} ->
            Stream1 = Stream#stream{parser=Parser1},
            {Events, S#state{astream=Stream1}};
        {error, Reason} ->
            lager:info([{reason,Reason}], "Bad Data Received"),
            {error, Reason}
    end.

-spec event(Event, State) -> Result when
      Event :: term(),
      State :: xmpp(),
      Result :: Ok | Error,
      Ok :: {ok, State} | {ImplMod, Reply, State},
      ImplMod :: module(),
      Reply :: term(),
      Error :: {error, term()}.
event(Event, S) -> incoming_(Event, S).

handle_iq(#{attrs:=#{<<"id">>:=Id}}=Iq,#state{astream=Stream}=S) ->
    IqPending = Stream#stream.iq_pending,
    case maps:get(Id, IqPending, undefined) of
        undefined ->
            lager:info([{id,Id}], "Unknown IQ Received"),
            S;
        NS when is_binary(NS) ->
            Module = lookup_ns(NS),
            Stream1 = Stream#stream{iq_pending=maps:remove(Id, IqPending)},
            S1 = S#state{astream=Stream1},
            lager:info([{id,Id},{ns,NS}], "IQ for NS Received"),
            Module:handle(Iq, S1)
    end.

incoming_(#{attrs:=#{<<"xmlns">>:=?NS,<<"id">>:=Id,
                     <<"xmlns:stream">>:=?NS_STREAM,
                     <<"from">>:=From},
            name:=<<"stream:stream">>,type:=stream_start},
          #state{astream=#stream{to=From,state=pending}=Stream}=S) ->
    Stream1 = Stream#stream{id=Id},
    lager:debug([{id,Id},{from,From}], "Stream Opening"),
    {ok, S#state{astream=Stream1}};
incoming_(#{name:=<<"stream:features">>,childs:=Childs},
          #state{astream=#stream{state=pending,ref=Ref,id=Id}}=S) ->
    S1 = lists:foldl(fun(Child, S0) -> incoming_(Child, S0) end, S, Childs),
    Stream = S1#state.astream,
    Stream1 = Stream#stream{state=open,ref=undefined},
    lager:debug([{id,Id}], "Stream Features Received"),
    {stream, Ref, S1#state{astream=Stream1}};
incoming_(#{type:=stream_start}=El,#state{astream=Stream}=State) ->
    Stream1 = Stream#stream{state=wait_stanza,stanza=[El]},
    State#state{astream=Stream1};
incoming_(stream_end, #state{astream=#stream{state=wait_stanza,
                                             stanza=Stanza}=Stream}=State) ->
    [Parent|Childs] = lists:reverse(Stanza),
    Stanza1 = maps:remove(type, Parent#{childs=>Childs}),
    Stream1 = Stream#stream{state=open,stanza=[]},
    incoming_(Stanza1, State#state{astream=Stream1});
incoming_(El, #state{astream=#stream{state=wait_stanza,stanza=Stanza}=Stream}=State) ->
    Stream1 = Stream#stream{stanza=[El|Stanza]},
    State#state{astream=Stream1};
incoming_(#{name:=<<"iq">>}=El, State) ->
    handle_iq(El, State);
incoming_(#{name:=<<"message">>}=El, State) ->
    {message, El, State};
incoming_(#{attrs:=#{<<"xmlns">>:=NS}}=Event, State) ->
    Module = lookup_ns(NS),
    Module:handle(Event, State);
incoming_(stream_end, #state{astream=#stream{id=Id}, streams=Streams}=S) ->
    lager:debug([{id,Id}], "Stream Ending"),
    case Streams of
        [Stream|Streams] -> S#state{astream=Stream,streams=Streams};
        [] -> S#state{astream=undefined,streams=[]}
    end.

lookup_ns(?NS_SASL) -> exmpp_sasl;
lookup_ns(?NS_SESS) -> exmpp_session;
lookup_ns(?NS_BIND) -> exmpp_bind;
lookup_ns(?NS) -> ?MODULE;
lookup_ns(?NS_STREAM) -> ?MODULE.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lookup_ns_test_() ->
    [?_assertEqual(exmpp_sasl,lookup_ns(?NS_SASL)),
     ?_assertEqual(exmpp_session,lookup_ns(?NS_SESS)),
     ?_assertEqual(exmpp_bind,lookup_ns(?NS_BIND)),
     ?_assertEqual(?MODULE,lookup_ns(?NS)),
     ?_assertEqual(?MODULE,lookup_ns(?NS_STREAM))].

open_stream_test_() ->
    [?_assertMatch(
        {<<"<stream:stream to='test@example.com' version='1.0'"
           " xmlns='jabber:client'"
           " xmlns:stream='http://etherx.jabber.org/streams'>">>,
         Ref, #state{astream=#stream{state=pending,
                                     ref=Ref}}} when is_reference(Ref),
        open_stream(#{to=><<"test@example.com">>}, #state{})),
     ?_assertMatch(
        {<<"<stream:stream to='test@example.com' version='1.0'"
           " xmlns='jabber:client'"
           " xmlns:stream='http://etherx.jabber.org/streams'>">>,
         Ref,
         #state{astream=#stream{state=pending,
                                ref=Ref},
                streams=[#stream{}]}} when is_reference(Ref),
        open_stream(#{to=><<"test@example.com">>},
                    #state{streams=[#stream{}]}))].

-endif.
