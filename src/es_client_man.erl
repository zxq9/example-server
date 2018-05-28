%%% @doc
%%% Example Server Client Manager
%%%
%%% This is the "manager" part of the service->worker pattern.
%%% It keeps track of who is connected and can act as a router among the workers.
%%% Having this process allows us to abstract and customize service-level concepts
%%% (the high-level ideas we care about in terms of solving an external problem in the
%%% real world) and keep them separate from the lower-level details of supervision that
%%% OTP should take care of for us.
%%% @end

-module(es_client_man).
-behavior(gen_server).
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("MIT").

-export([listen/1, ignore/0]).
-export([enroll/0, echo/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


%%% Type and Record Definitions


-record(s, {port_num = none :: none | inet:port_number(),
            listener = none :: none | gen_tcp:socket(),
            clients  = []   :: [pid()]}).


-type state()   :: #s{}.



%%% Service Interface


-spec listen(PortNum) -> Result
    when PortNum :: inet:port_number(),
         Result  :: ok
                  | {error, Reason},
         Reason  :: {listening, inet:port_number()}.
%% @doc
%% Tell the service to start listening on a given port.
%% Only one port can be listened on at a time in the current implementation, so
%% an error is returned if the service is already listening.

listen(PortNum) ->
    gen_server:call(?MODULE, {listen, PortNum}).


-spec ignore() -> ok.
%% @doc
%% Tell the service to stop listening.
%% It is not an error to call this function when the service is not listening.

ignore() ->
    gen_server:cast(?MODULE, ignore).



%%% Client Process Interface


-spec enroll() -> ok.
%% @doc
%% Clients register here when they establish a connection.
%% Other processes can enroll as well.

enroll() ->
    gen_server:cast(?MODULE, {enroll, self()}).


-spec echo(Message) -> ok
    when Message :: string().
%% @doc
%% The function that tells the manager to broadcast a message to all clients.
%% This can broadcast arbitrary strings to clients from non-clients as well.

echo(Message) ->
    gen_server:cast(?MODULE, {echo, Message, self()}).



%%% Startup Functions


-spec start_link() -> Result
    when Result :: {ok, pid()}
                 | {error, Reason :: term()}.
%% @private
%% This should only ever be called by es_clients (the service-level supervisor).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).


-spec init(none) -> {ok, state()}.
%% @private
%% Called by the supervisor process to give the process a chance to perform any
%% preparatory work necessary for proper function.

init(none) ->
    ok = io:format("Starting.~n"),
    State = #s{},
    {ok, State}.



%%% gen_server Message Handling Callbacks


-spec handle_call(Message, From, State) -> Result
    when Message  :: term(),
         From     :: {pid(), reference()},
         State    :: state(),
         Result   :: {reply, Response, NewState}
                   | {noreply, State},
         Response :: ok
                   | {error, {listening, inet:port_number()}},
         NewState :: state().
%% @private
%% The gen_server:handle_call/3 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_call-3

handle_call({listen, PortNum}, _, State) ->
    {Response, NewState} = do_listen(PortNum, State),
    {reply, Response, NewState};
handle_call(Unexpected, From, State) ->
    ok = io:format("~p Unexpected call from ~tp: ~tp~n", [self(), From, Unexpected]),
    {noreply, State}.


-spec handle_cast(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_cast/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2

handle_cast({enroll, Pid}, State) ->
    NewState = do_enroll(Pid, State),
    {noreply, NewState};
handle_cast({echo, Message, Sender}, State) ->
    ok = do_echo(Message, Sender, State),
    {noreply, State};
handle_cast(ignore, State) ->
    NewState = do_ignore(State),
    {noreply, NewState};
handle_cast(Unexpected, State) ->
    ok = io:format("~p Unexpected cast: ~tp~n", [self(), Unexpected]),
    {noreply, State}.


-spec handle_info(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_info/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_info-2

handle_info({'DOWN', Mon, process, Pid, Reason}, State) ->
    NewState = handle_down(Mon, Pid, Reason, State),
    {noreply, NewState};
handle_info(Unexpected, State) ->
    ok = io:format("~p Unexpected info: ~tp~n", [self(), Unexpected]),
    {noreply, State}.



%%% OTP Service Functions


-spec code_change(OldVersion, State, Extra) -> Result
    when OldVersion :: {down, Version} | Version,
         Version    :: term(),
         State      :: state(),
         Extra      :: term(),
         Result     :: {ok, NewState}
                     | {error, Reason :: term()},
         NewState   :: state().
%% @private
%% The gen_server:code_change/3 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:code_change-3

code_change(_, State, _) ->
    {ok, State}.


-spec terminate(Reason, State) -> no_return()
    when Reason :: normal
                 | shutdown
                 | {shutdown, term()}
                 | term(),
         State  :: state().
%% @private
%% The gen_server:terminate/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:terminate-2

terminate(_, _) ->
    ok.



%%% Doer Functions


-spec do_listen(PortNum, State) -> {Result, NewState}
    when PortNum  :: inet:port_number(),
         State    :: state(),
         Result   :: ok
                   | {error, Reason :: {listening, inet:port_number()}},
         NewState :: state().
%% @private
%% The "doer" procedure called when a "listen" message is received.

do_listen(PortNum, State = #s{port_num = none}) ->
    SocketOptions =
        [inet6,
         {active,    once},
         {mode,      binary},
         {keepalive, true},
         {reuseaddr, true}],
    {ok, Listener} = gen_tcp:listen(PortNum, SocketOptions),
    {ok, _} = es_client:start(Listener),
    {ok, State#s{port_num = PortNum, listener = Listener}};
do_listen(_, State = #s{port_num = PortNum}) ->
    ok = io:format("~p Already listening on ~p~n", [self(), PortNum]),
    {{error, {listening, PortNum}}, State}.


-spec do_ignore(State) -> NewState
    when State    :: state(),
         NewState :: state().
%% @private
%% The "doer" procedure called when an "ignore" message is received.

do_ignore(State = #s{listener = none}) ->
    State;
do_ignore(State = #s{listener = Listener}) ->
    ok = gen_tcp:close(Listener),
    State#s{listener = none}.


-spec do_enroll(Pid, State) -> NewState
    when Pid      :: pid(),
         State    :: state(),
         NewState :: state().

do_enroll(Pid, State = #s{clients = Clients}) ->
    case lists:member(Pid, Clients) of
        false ->
            Mon = monitor(process, Pid),
            ok = io:format("Monitoring ~tp @ ~tp~n", [Pid, Mon]),
            State#s{clients = [Pid | Clients]};
        true ->
            State
    end.


-spec do_echo(Message, Sender, State) -> ok
    when Message :: string(),
         Sender  :: pid(),
         State   :: state().
%% @private
%% The "doer" procedure called when an "echo" message is received.

do_echo(Message, Sender, #s{clients = Clients}) ->
    Send = fun(Client) -> Client ! {relay, Sender, Message} end,
    lists:foreach(Send, Clients).


-spec handle_down(Mon, Pid, Reason, State) -> NewState
    when Mon      :: reference(),
         Pid      :: pid(),
         Reason   :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% Deal with monitors. When a new process enrolls as a client a monitor is set and
%% the process is added to the client list. When the process terminates we receive
%% a 'DOWN' message from the monitor. More sophisticated work managers typically have
%% an "unenroll" function, but this echo service doesn't need one.

handle_down(Mon, Pid, Reason, State = #s{clients = Clients}) ->
    case lists:member(Pid, Clients) of
        true ->
            NewClients = lists:delete(Pid, Clients),
            State#s{clients = NewClients};
        false ->
            Unexpected = {'DOWN', Mon, process, Pid, Reason},
            ok = io:format("~p Unexpected info: ~tp~n", [self(), Unexpected]),
            State
    end.
