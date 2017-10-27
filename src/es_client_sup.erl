-module(es_client_sup).
-behaviour(supervisor).

-export([start_acceptor/1]).
-export([start_link/0]).
-export([init/1]).

start_acceptor(ListenSocket) ->
    supervisor:start_child(?MODULE, [ListenSocket]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, none).

init(none) ->
    RestartStrategy = {simple_one_for_one, 1, 60},
    Client    = {es_client,
                 {es_client, start_link, []},
                 temporary,
                 brutal_kill,
                 worker,
                 [es_client]},
    {ok, {RestartStrategy, [Client]}}.
