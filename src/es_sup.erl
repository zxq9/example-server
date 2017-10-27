-module(es_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = {one_for_one, 1, 60},
    Clients   = {es_clients,
                 {es_clients, start_link, []},
                 permanent,
                 5000,
                 supervisor,
                 [es_clients]},
    Children  = [Clients],
    {ok, {RestartStrategy, Children}}.
