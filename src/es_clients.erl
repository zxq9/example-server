-module(es_clients).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, none).

init(none) ->
    RestartStrategy = {rest_for_one, 1, 60},
    ClientSup = {es_client_sup,
                 {es_client_sup, start_link, []},
                 permanent,
                 5000,
                 supervisor,
                 [es_client_sup]},
    ClientMan = {es_client_man,
                 {es_client_man, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [es_client_man]},
    Children  = [ClientSup, ClientMan],
    {ok, {RestartStrategy, Children}}.
