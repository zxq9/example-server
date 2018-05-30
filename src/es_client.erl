%%% @doc
%%% Example Server Client
%%%
%%% An extremely naive Telnet client handler.
%%% Unlike other modules that represent discrete processes, this one does not adhere
%%% to any OTP behavior. It does, however, adhere to OTP.
%%%
%%% In some cases it is more comfortable to write socket handlers or a certain
%%% category of state machines as "pure" Erlang processes. This approach is made
%%% OTP-able by use of the proc_lib module, which is the underlying library used
%%% to write the stdlib's behaviors like gen_server, gen_statem, gen_fsm, etc.
%%%
%%% http://erlang.org/doc/design_principles/spec_proc.html
%%% @end

-module(es_client).
-vsn("1.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("MIT").

-export([start/1]).
-export([start_link/1, init/2]).
-export([system_continue/3, system_terminate/4,
         system_get_state/1, system_replace_state/2]).


%%% Type and Record Definitions


-record(s, {socket = none :: none | gen_tcp:socket()}).


%% An alias for the state record above. Aliasing state can smooth out annoyances
%% that can arise from using the record directly as its own type all over the code.

-type state() :: #s{}.


%%% Service Interface


-spec start(ListenSocket) -> Result
    when ListenSocket :: gen_tcp:socket(),
         Result       :: {ok, pid()}
                       | {error, Reason},
         Reason       :: {already_started, pid()}
                       | {shutdown, term()}
                       | term().
%% @private
%% How the es_client_man or a prior es_client kicks things off.
%% This is called in the context of es_client_man or the prior es_client.

start(ListenSocket) ->
    es_client_sup:start_acceptor(ListenSocket).


-spec start_link(ListenSocket) -> Result
    when ListenSocket :: gen_tcp:socket(),
         Result       :: {ok, pid()}
                       | {error, Reason},
         Reason       :: {already_started, pid()}
                       | {shutdown, term()}
                       | term().
%% @private
%% This is called by the es_client_sup. While start/1 is called to iniate a startup
%% (essentially requesting a new worker be started by the supervisor), this is
%% actually called in the context of the supervisor.

start_link(ListenSocket) ->
    proc_lib:start_link(?MODULE, init, [self(), ListenSocket]).


-spec init(Parent, ListenSocket) -> no_return()
    when Parent       :: pid(),
         ListenSocket :: gen_tcp:socket().
%% @private
%% This is the first code executed in the context of the new worker itself.
%% This function does not have any return value, as the startup return is
%% passed back to the supervisor by calling proc_lib:init_ack/2.
%% We see the initial form of the typical arity-3 service loop form here in the
%% call to listen/3.

init(Parent, ListenSocket) ->
    ok = io:format("~p Listening.~n", [self()]),
    Debug = sys:debug_options([]),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    listen(Parent, Debug, ListenSocket).


-spec listen(Parent, Debug, ListenSocket) -> no_return()
    when Parent       :: pid(),
         Debug        :: [sys:dbg_opt()],
         ListenSocket :: gen_tcp:socket().
%% @private
%% This function waits for a TCP connection. The owner of the socket is still
%% the es_client_man (so it can still close it on a call to es_client_man:ignore/0),
%% but the only one calling gen_tcp:accept/1 on it is this process. Closing the socket
%% is one way a manager process can gracefully unblock child workers that are blocking
%% on a network accept.
%%
%% Once it makes a TCP connection it will call start/1 to spawn its successor.

listen(Parent, Debug, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            {ok, _} = start(ListenSocket),
            {ok, Peer} = inet:peername(Socket),
            ok = io:format("~p Connection accepted from: ~p~n", [self(), Peer]),
            ok = es_client_man:enroll(),
            State = #s{socket = Socket},
            loop(Parent, Debug, State);
        {error, closed} ->
            ok = io:format("~p Retiring: Listen socket closed.~n", [self()]),
            exit(normal)
     end.


-spec loop(Parent, Debug, State) -> no_return()
    when Parent :: pid(),
         Debug  :: [sys:dbg_opt()],
         State  :: state().
%% @private
%% The service loop itself. This is the service state. The process blocks on receive
%% of Erlang messages, TCP segments being received themselves as Erlang messages.

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
            ok = es_client_man:echo(Message),
            loop(Parent, Debug, State);
        {relay, Sender, Message} when Sender == self() ->
            ok = gen_tcp:send(Socket, ["Message from YOU: ", Message]),
            loop(Parent, Debug, State);
        {relay, Sender, Message} ->
            From = io_lib:format("Message from ~tp: ", [Sender]),
            ok = gen_tcp:send(Socket, [From, Message]),
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


-spec system_continue(Parent, Debug, State) -> no_return()
    when Parent :: pid(),
         Debug  :: [sys:dbg_opt()],
         State  :: state().
%% @private
%% The function called by the OTP internal functions after a system message has been
%% handled. If the worker process has several possible states this is one place
%% resumption of a specific state can be specified and dispatched.

system_continue(Parent, Debug, State) ->
    loop(Parent, Debug, State).


-spec system_terminate(Reason, Parent, Debug, State) -> no_return()
    when Reason :: term(),
         Parent :: pid(),
         Debug  :: [sys:dbg_opt()],
         State  :: state().
%% @private
%% Called by the OTP inner bits to allow the process to terminate gracefully.
%% Exactly when and if this is callback gets called is specified in the docs:
%% See: http://erlang.org/doc/design_principles/spec_proc.html#msg

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).



-spec system_get_state(State) -> {ok, State}
    when State :: state().
%% @private
%% This function allows the runtime (or anything else) to inspect the running state
%% of the worker process at any arbitrary time.

system_get_state(State) -> {ok, State}.


-spec system_replace_state(StateFun, State) -> {ok, NewState, State}
    when StateFun :: fun(),
         State    :: state(),
         NewState :: term().
%% @private
%% This function allows the system to update the process state in-place. This is most
%% useful for state transitions between code types, like when performing a hot update
%% (very cool, but sort of hard) or hot patching a running system (living on the edge!).

system_replace_state(StateFun, State) ->
    {ok, StateFun(State), State}.
