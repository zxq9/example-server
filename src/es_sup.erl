%%% @doc
%%% Example Server Supervisor
%%%
%%% The very top level supervisor in the system. It only has one service branch: the
%%% client handling service. In a more complex system the client handling service would
%%% only be one part of a larger system. Were this a game system, for example, the
%%% item data management service would be a peer, as would a login credential provision
%%% service, game world event handling, and so on.
%%%
%%% See: http://erlang.org/doc/design_principles/applications.html
%%% See: http://zxq9.com/archives/1311
%%% @end

-module(es_sup).
-vsn("1.1.0").
-behaviour(supervisor).
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("MIT").

-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

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
