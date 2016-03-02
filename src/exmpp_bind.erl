-module(exmpp_bind).

-export([handle/2,
         bind/2]).

-define(NS, <<"urn:ietf:params:xml:ns:xmpp-bind">>).

-record(bind, {
          id :: binary(),
          to :: binary(),
          jid :: binary(),
          ref :: reference()|undefined,
          state = unbound :: bound | unbound | pending
         }).

-type bind_details() :: #{to=>binary()}.

-spec bind(Details, State) -> Retval when
      Details :: bind_details(),
      State :: exmpp_core:xmpp(),
      Retval :: Ok | Error,
      Ok :: {iolist(), reference(), State},
      Error :: {error, no_bind_support}.
bind(Details, State) ->
    case exmpp_core:get_state_for(?NS, State) of
        undefined -> {error, no_bind_support};
        #bind{} ->
            {El, Ref, State1} = bind_(Details, State),
            {exmpp_xml:encode(El), Ref, State1}
    end.

-spec handle(Element, State) -> Retval when
      Element :: exmpp_xml:el(),
      State :: term(),
      Retval :: Ok | Error,
      Ok :: State | {bind, Reply, State},
      Reply :: authorized,
      Error :: {error, term()}.
handle(Msg, State) ->
    BindState =
        case exmpp_core:get_state_for(?NS, State) of
            undefined -> #bind{};
            S -> S
        end,
    case handle_(Msg, BindState) of
        {Status, BindState1} ->
            State1 = exmpp_core:set_state_for(?NS, BindState1, State),
            {bind, Status, State1};
        BindState1 ->
            exmpp_core:set_state_for(?NS, BindState1, State)
    end.

% Internal
handle_(#{name:=<<"bind">>}, BindState) ->
    BindState;
handle_(#{name:=<<"iq">>,
          attrs:=#{<<"from">>:=To,<<"id">>:=Id,<<"type">>:=<<"result">>},
          childs:=[#{name:=<<"bind">>,
                     childs:=[#{name:=<<"jid">>, data:=Jid}]}]},
        #bind{to=To,id=Id,ref=Ref}=Bind) ->
    lager:debug([{id,Id},{from,To}], "Stream Bound"),
    {Ref, Bind#bind{ref=undefined,jid=Jid,state=bound}}.

bind_(#{to:=To}, State) ->
    Id = <<"bind-1">>,
    Child = #{name => <<"bind">>,
              attrs => maps:from_list([{<<"xmlns">>, ?NS}])},
    {El, State1} = exmpp_core:iq_ns(#{id => Id, type => <<"set">>,
                                      attrs => #{<<"to">> => To}}, [Child],
                                    ?NS, State),
    BindState = exmpp_core:get_state_for(?NS, State1),
    Ref = erlang:make_ref(),
    BindState1 = BindState#bind{ref=Ref,id=Id,to=To},
    State2 = exmpp_core:set_state_for(?NS, BindState1, State1),
    lager:debug([{id,Id},{to,To}], "Binding Stream"),
    {El, Ref, State2}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_test_() ->
    Ref = erlang:make_ref(),
    Id = <<"bind-1">>,
    To = <<"example.com">>,
    [?_assertMatch(
        #bind{state=unbound},
        handle_(#{name=><<"bind">>}, #bind{})
       ),
     ?_assertMatch(
        {Ref, #bind{state=bound,ref=undefined,jid= <<"test@example.com">>}} when is_reference(Ref),
        handle_(#{name=><<"iq">>,
                  attrs=>#{<<"from">>=>To,
                           <<"id">>=>Id,
                           <<"type">>=><<"result">>},
                  childs=>[#{name=><<"bind">>,
                             childs=>[#{name=><<"jid">>,
                                        data=><<"test@example.com">>}]}]
                 }, #bind{to=To,id=Id,ref=Ref}))].

-endif.
