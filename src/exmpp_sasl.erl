-module(exmpp_sasl).

-export([handle/2,
         authenticate/2]).

-define(NS, <<"urn:ietf:params:xml:ns:xmpp-sasl">>).

-record(sasl, {
          mechanisms :: [binary()],
          ref :: reference() | undefined,
          state = unauthorized :: unauthorized | authorized | pending
         }).

-type authenticate_req() :: #{type => plain,
                              user => binary(),
                              password => binary()}.

-spec authenticate(Details, State) -> Result when
      Details :: authenticate_req(),
      State :: term(),
      Result :: Ok | Error,
      Ok :: {iolist(), reference(), State},
      Error :: {error, ErrorReason},
      ErrorReason :: no_sasl_state | unsupported_type.
authenticate(Details, State) ->
    case exmpp_core:get_state_for(?NS, State) of
        undefined -> {error, no_sasl_state};
        SaslState ->
            case authenticate_(Details, SaslState) of
                {El, Ref, SaslState1} ->
                    State1 = exmpp_core:set_state_for(?NS, SaslState1, State),
                    {exmpp_xml:encode(El), Ref, State1};
                Error -> Error
            end
    end.

-spec handle(Element, State) -> Retval when
      Element :: exmpp_xml:el(),
      State :: term(),
      Retval :: Ok | Error,
      Ok :: State | {sasl, Reply, State},
      Reply :: authorized,
      Error :: {error, term()}.
handle(Msg, State) ->
    SaslState =
        case exmpp_core:get_state_for(?NS, State) of
            undefined -> #sasl{};
            S -> S
        end,
    case handle_(Msg, SaslState) of
        {Status, SaslState1} ->
            State1 = exmpp_core:set_state_for(?NS, SaslState1, State),
            {sasl, Status, State1};
        SaslState1 ->
            exmpp_core:set_state_for(?NS, SaslState1, State)
    end.

% Internal
handle_(#{name:=<<"mechanisms">>, childs:=Childs}, Sasl) ->
    Mechanisms = [M || #{data:=M, name:=<<"mechanism">>} <- Childs],
    Sasl#sasl{mechanisms=Mechanisms};
handle_(#{name:=<<"success">>}, #sasl{ref=Ref,state=pending}=Sasl) ->
    lager:debug([{ref,Ref}], "User Authorized"),
    {{Ref, authorized}, Sasl#sasl{state=authorized,ref=undefined}};
handle_(#{name:=<<"failure">>,childs:=[#{name:=Reason}]},
        #sasl{ref=Ref,state=pending}=Sasl) ->
    lager:debug([{ref,Ref}], "User Unauthorized"),
    {{Ref, {unauthorized, Reason}}, Sasl#sasl{state=unauthorized,
                                              ref=undefined}}.

authenticate_(#{type:=plain,user:=User,password:=Pw},
              #sasl{mechanisms=M}=SaslState) ->
    case lists:member(<<"PLAIN">>, M) of
        true ->
            S = [#{name => <<"auth">>,
                   data => base64:encode(<<0, User/binary, 0, Pw/binary>>),
                   attrs => #{<<"xmlns">> => ?NS,
                              <<"mechanism">> => <<"PLAIN">>}}],
            Ref = erlang:make_ref(),
            lager:debug([{user,User},{ref,Ref}], "Authenticating User"),
            {S, Ref, SaslState#sasl{state=pending,ref=Ref}};
        _ -> {error, unsupported_type}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_test_() ->
    Ref = erlang:make_ref(),
    [?_assertEqual(
        #sasl{mechanisms=[<<"A">>,<<"B">>]},
        handle_(#{name=><<"mechanisms">>,
                  childs=>[#{name=><<"mechanism">>,data=><<"A">>},
                           #{name=><<"mechanism">>,data=><<"B">>}]}, #sasl{})
       ),
     ?_assertEqual(
        {{Ref, authorized}, #sasl{ref=undefined,state=authorized}},
        handle_(#{name=><<"success">>}, #sasl{ref=Ref,state=pending})
       ),
     ?_assertEqual(
        {{Ref, {unauthorized, <<"reason">>}},
         #sasl{ref=undefined,state=unauthorized}},
        handle_(#{name=><<"failure">>,
                  childs=>[#{name=><<"reason">>}]}, #sasl{ref=Ref,state=pending})
       )].

authenticate__test_() ->
    [?_assertMatch(
        {[#{name:=<<"auth">>,
            data:=<<"AHRlc3RAZXhhbXBsZS5jb20AdGVzdA==">>,
            attrs:=#{<<"xmlns">>:=?NS,
                     <<"mechanism">>:=<<"PLAIN">>}}],
         Ref,
         #sasl{state=pending,ref=Ref}
        } when is_reference(Ref),
               authenticate_(#{type=>plain,user=><<"test@example.com">>,
                               password=><<"test">>},
                             #sasl{mechanisms=[<<"PLAIN">>]})
       ),
     ?_assertMatch(
        {error, unsupported_type},
        authenticate_(#{type=>plain,user=><<"test@example.com">>,
                        password=><<"test">>},
                      #sasl{mechanisms=[]})
       )
    ].

-endif.
