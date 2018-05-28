%%% @doc
%%% Example Server Client Service Supervisor
%%%
%%% This is the service-level supervisor of the system. It is the parent of both the
%%% client connection handlers and the client manager (which manages the client
%%% connection handlers). This is the child of es_sup.
%%%
%%% See: http://erlang.org/doc/apps/kernel/application.html
%%% @end

-module(es_clients).
-behavior(supervisor).
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("MIT").

-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, none).

-spec init(none) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

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
