-module(exmpp_client).
% A simple exmpp client behaviour implementation that makes it just a little
% bit easier to use the client
-behaviour(gen_server).

-callback(init(Args) ->
                 {ok, State} when
      Args :: term(),
      State :: term()).
-callback(handle_call(Call, From, State) ->
                 {reply, Reply, State} |
                 {send, Msg, Reply, State} |
                 {noreply, State} |
                 {stop, Reason, State} |
                 {send, Msg, Reply} when
      Call :: term(),
      From :: term(),
      State :: term(),
      Reply :: term(),
      Reason :: term(),
      Msg :: exmpp_xml:els()).
-callback(handle_cast(Cast, State) ->
                 {noreply, State} |
                 {stop, Reason, State} |
                 {send, Msg, State} when
      Cast :: term(),
      State :: term(),
      Reason :: term(),
      Msg :: exmpp_xml:els()).
-callback(handle_info(Info, State) ->
                 {noreply, State} |
                 {stop, Reason, State} |
                 {send, Msg, State} when
      Info :: term(),
      State :: term(),
      Reason :: term(),
      Msg :: exmpp_xml:els()).
-callback(terminate(Reason, State) ->
                 term() when
      Reason :: term(),
      State :: term()).
-callback(message(Msg, State) ->
                 {send, Msg, State} |
                 {stop, Reason, State} |
                 {ok, State} when
      Msg :: exmpp_xml:els(),
      State :: term(),
      Reason :: term()).


-export([start_link/4]).

% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(conn, {
          host :: inet:ip_address() | inet:hostname(),
          port :: inet:port_number(),
          sock :: ssl:socket() | gen_tcp:socket() | undefined,
          proto :: ssl|tcp,
          timeout :: non_neg_integer()|infinity
         }).

-record(state, {
          cb :: {module(), term()},
          conn :: #conn{},
          xmpp :: xmpp_core:xmpp(),
          s :: init | starting | authenticating | starting_inner | binding |
               session_init | active,
          ref :: reference() | undefined,
          to :: binary(),
          waiter :: term(),
          username :: binary(),
          password :: binary()
         }).

start_link(Module, Args, Options, Timeout) ->
    case gen_server:start_link(?MODULE, [Module, Args, Timeout|Options],
                               []) of
        {ok, Pid} ->
            try gen_server:call(Pid, connect) of
                ok -> {ok, Pid};
                {error, _Reason} = Error -> Error
            catch
                exit:_ ->
                    erlang:exit(Pid, kill),
                    {error, timeout}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Callbacks
init([Module, Args, Timeout|#{username:=Username,password:=Password,
                              hostname:=Hostname,port:=Port,
                              proto:=Proto}=Opts]) ->
    Xmpp = exmpp_core:create(#{}),
    Conn = #conn{host=Hostname,port=Port,proto=Proto,timeout=Timeout},
    To = maps:get(to,Opts,list_to_binary(Hostname)),
    {ok, #state{conn=Conn,xmpp=Xmpp,cb={Module,Args},to=To,
                username=Username,password=Password,s=init}}.

handle_call(connect, From, #state{conn=Conn,xmpp=Xmpp,s=init,to=To}=State) ->
    case connect_(Conn) of
        {ok, Conn1} ->
            {El, Ref, Xmpp1} = exmpp_core:open_stream(#{to=>To},Xmpp),
            send_(El, Conn1),
            {noreply, State#state{conn=Conn1,xmpp=Xmpp1,ref=Ref,s=starting,
                                  waiter=From}};
        {error, Reply} ->
            {stop, normal, Reply, State}
    end;
handle_call(Call, From, #state{cb={Module, CbState},conn=Conn}=State) ->
    case Module:handle_call(Call, From, CbState) of
        {reply, Reply, CbState1} ->
            {reply, Reply, State#state{cb={Module, CbState1}}};
        {send, Msg, Reply, CbState1} ->
            send_(exmpp_xml:encode(Msg), Conn),
            {reply, Reply, State#state{cb={Module, CbState1}}};
        {noreply, CbState1} ->
            {noreply, State#state{cb={Module, CbState1}}};
        {send, Msg, CbState1} ->
            send_(exmpp_xml:encode(Msg), Conn),
            {noreply, State#state{cb={Module, CbState1}}};
        {stop, Reason, CbState1} ->
            {stop, Reason, State#state{cb={Module,CbState1}}}
    end.

handle_cast(Cast, #state{cb={Module, CbState},conn=Conn}=State) ->
    case Module:handle_cast(Cast, CbState) of
        {noreply, CbState1} ->
            {noreply, State#state{cb={Module, CbState1}}};
        {stop, Reason, CbState1} ->
            {stop, Reason, State#state{cb={Module,CbState1}}};
        {send, Msg, CbState1} ->
            send_(exmpp_xml:encode(Msg), Conn),
            {noreply, State#state{cb={Module, CbState1}}}
    end.

handle_info({Type, _Socket, Data}, State) when Type =:= ssl orelse
                                               Type =:= tcp ->
    State1 = incoming_(Data, State),
    {noreply, State1};
handle_info({Close, _Socket}, State) when Close =:= ssl_closed orelse
                                          Close =:= tcp_closed ->
    {stop, normal, State};
handle_info({Error, _Socket, Reason}, State) when Error =:= ssl_error orelse
                                                  Error =:= tcp_error ->
    {stop, Reason, State};
handle_info(Info, #state{cb={Module, CbState},conn=Conn}=State) ->
    case Module:handle_info(Info, CbState) of
        {noreply, CbState1} ->
            {noreply, State#state{cb={Module, CbState1}}};
        {send, Msg, CbState1} ->
            send_(exmpp_xml:encode(Msg), Conn),
            {noreply, State#state{cb={Module, CbState1}}};
        {stop, Reason, CbState1} ->
            {stop, Reason, State#state{cb={Module,CbState1}}}
    end.

code_change(_, _, _) -> erlang:error(function_clause).

terminate(Reason, #state{cb={Module, CbState},conn=Conn,xmpp=Xmpp}) ->
    {El, _} = exmpp_core:close_stream(#{}, Xmpp),
    send_(El, Conn),
    close_(Conn),
    Module:terminate(Reason, CbState),
    ok.

% Internal
incoming_(Data, #state{xmpp=Xmpp,conn=Conn}=S) ->
    case exmpp_core:parse(Data, Xmpp) of
        {error, Reason} -> {stop, Reason, S};
        {heartbeat, Xmpp1} ->
            send_(<<" ">>, Conn),
            S#state{xmpp=Xmpp1};
        {Events, Xmpp1} -> event_(Events, S#state{xmpp=Xmpp1})
    end.

event_([], #state{conn=Conn}=State) ->
    set_sockopts(Conn),
    State;
event_([Event|Events], #state{xmpp=Xmpp}=State) ->
    Res = exmpp_core:event(Event, Xmpp),
    case event_res(Res, State) of
        {stop, Reason, State1} ->
            {stop, Reason, State1};
        State1 ->
            event_(Events, State1)
    end.

event_res({stream, Reply, Xmpp1}, State) ->
    stream_event(Reply, State#state{xmpp=Xmpp1});
event_res({sasl, Reply, Xmpp1}, State) ->
    sasl_event(Reply, State#state{xmpp=Xmpp1});
event_res({bind, Reply, Xmpp1}, State) ->
    bind_event(Reply, State#state{xmpp=Xmpp1});
event_res({session, Reply, Xmpp1}, State) ->
    session_event(Reply, State#state{xmpp=Xmpp1});
event_res({message, Message, Xmpp1}, State) ->
    message_event(Message, State#state{xmpp=Xmpp1});
event_res({ok, Xmpp1}, State) ->
    State#state{xmpp=Xmpp1}.

message_event(Message, #state{cb={Module, CbState},conn=Conn}=State) ->
    case Module:message(Message, CbState) of
        {send, Msg, CbState1} ->
            send_(Msg, Conn),
            State#state{cb={Module,CbState1}};
        {stop, Reason, CbState1} ->
            {stop, Reason, State#state{cb={Module,CbState1}}};
        {ok, CbState1} ->
            State#state{cb={Module,CbState1}}
    end.

 % CHANGE THIS SIGNATURE TO {stream, {Ref, Status}, Xmpp}
stream_event(Ref, #state{ref=Ref,s=starting,username=Username,
                         password=Password,xmpp=Xmpp,conn=Conn}=State) ->
    Auth = #{type => plain,user => Username,password => Password},
    {El, Ref1, Xmpp1} = exmpp_sasl:authenticate(Auth, Xmpp),
    send_(El, Conn),
    State#state{ref=Ref1,xmpp=Xmpp1,s=authenticating};
stream_event(Ref, #state{ref=Ref,s=starting_inner,xmpp=Xmpp,conn=Conn,
                         to=To}=State) ->
    {El, Ref1, Xmpp1} = exmpp_bind:bind(#{to=>To}, Xmpp),
    send_(El, Conn),
    State#state{ref=Ref1,xmpp=Xmpp1,s=binding}.

sasl_event({Ref, authorized},#state{ref=Ref,s=authenticating,xmpp=Xmpp,to=To,
                                    conn=Conn}=State) ->
    {El, Ref1, Xmpp1} = exmpp_core:open_stream(#{to=>To},Xmpp),
    send_(El, Conn),
    State#state{ref=Ref1,xmpp=Xmpp1,s=starting_inner};
sasl_event({Ref, {unauthorized,_}}, #state{ref=Ref,s=authenticating,
                                           waiter=Waiter}=State) ->
    gen_server:reply(Waiter, {error, unauthorized}),
    {stop, unauthorized, State}.

bind_event(Ref, #state{ref=Ref,s=binding,xmpp=Xmpp,conn=Conn,to=To}=State) ->
    {El, Ref1, Xmpp1} = exmpp_session:create(#{to=>To}, Xmpp),
    send_(El, Conn),
    State#state{ref=Ref1,xmpp=Xmpp1,s=session_init}.

session_event({Ref, created},#state{ref=Ref,s=session_init,cb=Cb,
                                    waiter=Waiter}=State) ->
    gen_server:reply(Waiter, ok),
    State#state{cb=cb_init(Cb),ref=undefined,s=active}.

cb_init({Module, Args}) ->
    {ok, CbState} = Module:init(Args),
    {Module, CbState}.

connect_(#conn{host=Host,port=Port,proto=Proto,timeout=Timeout}=Conn) ->
    case Proto:connect(Host, Port, [{active, false},{mode,binary},
                                    {packet, 0},{header, 0}], Timeout) of
        {ok, Sock} -> {ok, Conn#conn{sock=Sock}};
        {error, _Reason} = Error -> Error
    end.

send_(Message, #conn{sock=Sock,proto=Proto}=C) ->
    lager:debug([], "send: ~p", [iolist_to_binary(Message)]),
    ok = Proto:send(Sock, Message),
    set_sockopts(C),
    Proto:setopts(Sock, [{active, once}]),
    ok.

close_(#conn{sock=Sock,proto=Proto}) -> catch Proto:close(Sock).


set_sockopts(#conn{proto=ssl,sock=Sock}) ->
    ssl:setopts(Sock, [{active, once}]);
set_sockopts(#conn{proto=tcp,sock=Sock}) ->
    inet:setopts(Sock, [{active, once}]).
