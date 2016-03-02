-module(exmpp_session).

-export([handle/2,
         create/2]).

-define(NS, <<"urn:ietf:params:xml:ns:xmpp-session">>).

-record(session, {
          id :: binary(),
          to :: binary(),
          ref :: reference() | undefined,
          state = no_session :: session | no_session | pending
         }).

-type details() :: #{to => binary()}.

-spec create(Details, State) -> Retval when
      Details :: details(),
      State :: exmpp_core:xmpp(),
      Retval :: Ok | Error,
      Ok :: {El, reference(), State},
      El :: iolist(),
      Error :: {error, no_session_support}.
create(Details, State) ->
    case exmpp_core:get_state_for(?NS, State) of
        undefined -> {error, no_session_support};
        SessionState ->
            {El, Ref, State1} = create_(Details, SessionState, State),
            {exmpp_xml:encode(El), Ref, State1}
    end.

-spec handle(Element, State) -> Retval when
      Element :: exmpp_xml:el(),
      State :: exmpp_core:xmpp(),
      Retval :: Ok | Error,
      Ok :: State | {session, Reply, State},
      Reply :: authorized,
      Error :: {error, term()}.
handle(Msg, State) ->
    SessionState =
        case exmpp_core:get_state_for(?NS, State) of
            undefined -> #session{};
            S -> S
        end,
    case handle_(Msg, SessionState) of
        {Status, SessionState1} ->
            State1 = exmpp_core:set_state_for(?NS, SessionState1, State),
            {session, Status, State1};
        SessionState1 ->
            exmpp_core:set_state_for(?NS, SessionState1, State)
    end.

handle_(#{name:=<<"session">>}, Session) ->
    Session;
handle_(#{name:=<<"iq">>,
          attrs:=#{<<"type">>:=<<"result">>,
                   <<"id">>:=Id,
                   <<"from">>:=From}},
        #session{to=From,id=Id,ref=Ref,state=pending}=Session) ->
    lager:debug([{id,Id}], "Session Started"),
    {{Ref, created}, Session#session{state=session,
                                     ref=undefined}}.

create_(#{to:=To}, SessionState, State) ->
    Id = <<"session-1">>,
    Child = #{name => <<"session">>,
              attrs => maps:from_list([{<<"xmlns">>, ?NS}])},
    {El, State1} = exmpp_core:iq_ns(#{id => Id,type => <<"set">>,
                                      attrs => #{<<"to">> => To}},
                                    [Child],
                                    ?NS, State),
    Ref = erlang:make_ref(),
    SessionState1 = SessionState#session{state=pending,to=To,
                                         ref=Ref,
                                         id=Id},
    State2 = exmpp_core:set_state_for(?NS, SessionState1, State1),
    lager:debug([{id,Id}], "Starting Session"),
    {El, Ref, State2}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_test_() ->
    Ref = erlang:make_ref(),
    Id = <<"session">>,
    From = <<"test@example.com">>,
    [?_assertMatch(
        #session{state=no_session},
        handle_(#{name=><<"session">>}, #session{})
       ),
     ?_assertMatch(
        {{Ref, created}, #session{state=session,ref=undefined}}
        when is_reference(Ref),
        handle_(#{name=><<"iq">>,
                  attrs=>#{
                    <<"type">>=><<"result">>,
                    <<"id">>=>Id,
                    <<"from">>=>From}}, #session{state=pending,
                                                 ref=Ref,
                                                 id=Id,
                                                 to=From})
       )
    ].

-endif.
