-module(es_client).

-export([start/1]).
-export([start_link/1, init/2]).
-export([system_continue/3, system_terminate/4,
         system_get_state/1, system_replace_state/2]).

-record(s, {socket = none :: none | gen_tcp:socket()}).

start(ListenSocket) ->
    es_client_sup:start_acceptor(ListenSocket).

start_link(ListenSocket) ->
    proc_lib:start_link(?MODULE, init, [self(), ListenSocket]).

init(Parent, ListenSocket) ->
    ok = io:format("~p Listening.~n", [self()]),
    Debug = sys:debug_options([]),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    listen(Parent, Debug, ListenSocket).

listen(Parent, Debug, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            {ok, _} = start(ListenSocket),
            {ok, Peer} = inet:peername(Socket),
            ok = io:format("~p Connection accepted from: ~p~n", [self(), Peer]),
            State = #s{socket = Socket},
            loop(Parent, Debug, State);
        {error, closed} ->
            ok = io:format("~p Retiring: Listen socket closed.~n", [self()]),
            exit(normal)
     end.

loop(Parent, Debug, State = #s{socket = Socket}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"bye\r\n">>} ->
            ok = io:format("~p Client saying goodbye. Bye!~n", [self()]),
            ok = gen_tcp:send(Socket, "Bye!\r\n"),
            ok = gen_tcp:shutdown(Socket, read_write),
            exit(normal);
        {tcp, Socket, Message} ->
            ok = io:format("~p received: ~tp~n", [self(), Message]),
            ok = gen_tcp:send(Socket, ["You sent: ", Message]),
            loop(Parent, Debug, State);
        {tcp_closed, Socket} ->
            ok = io:format("~p Socket closed, retiring.~n", [self()]),
            exit(normal);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        Unexpected ->
            ok = io:format("~p Unexpected message: ~tp", [self(), Unexpected]),
            loop(Parent, Debug, State)
    end.

system_continue(Parent, Debug, State) ->
    loop(Parent, Debug, State).


system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).


system_get_state(Misc) -> {ok, Misc}.


system_replace_state(StateFun, Misc) ->
    {ok, StateFun(Misc), Misc}.
