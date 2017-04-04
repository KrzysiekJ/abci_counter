-module(abci_counter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs =
        [#{id => counter,
           start => {abci_counter, start_link, []}},
         #{id => abci_server,
           start => {abci_server_sup, start_link, [{abci_counter, 46658}]},
           type => supervisor}],
    {ok, {{one_for_all, 1, 5}, Procs}}.
