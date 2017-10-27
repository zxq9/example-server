-module(es).
-behaviour(application).

-export([listen/1, ignore/0]).
-export([start/0, start/1]).
-export([start/2, stop/1]).

listen(PortNum) ->
    es_client_man:listen(PortNum).

ignore() ->
    es_client_man:ignore().

start() ->
    ok = application:ensure_started(sasl),
    ok = application:start(?MODULE),
    io:format("Starting es...").


start(Port) ->
    ok = start(),
    ok = es_client_man:listen(Port),
    io:format("Startup complete, listening on ~w~n", [Port]).

start(normal, _Args) ->
    es_sup:start_link().

stop(_State) ->
    ok.
