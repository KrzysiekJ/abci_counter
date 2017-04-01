-module(abci_counter_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    abci_counter_sup:start_link().

stop(_State) ->
    ok.
