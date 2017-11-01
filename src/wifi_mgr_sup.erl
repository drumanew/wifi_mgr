-module(wifi_mgr_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [{wifi_mgr_fsm,
            {wifi_mgr_fsm, start_link, []},
            permanent,
            5000,
            worker,
            [wifi_mgr_fsm]}],
  {ok, {{one_for_one, 1, 5}, Procs}}.
