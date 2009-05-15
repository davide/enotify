-module(enotify_sup).
-behavior(supervisor).

%% External exports
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

start_link(ExtProg, PortModule) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ExtProg, PortModule]).

init([ExtProg, PortModule]) ->
    {ok, {{one_for_one, 3, 10},
          [{enotify, {enotify, start_link, [ExtProg, PortModule]},
            permanent, 10, worker, [enotify]}]}}.

