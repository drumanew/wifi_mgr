-module(wifi_mgr_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch =
    cowboy_router:compile([{'_', [{'_', wifi_mgr_handler, []}]}]),
  cowboy:start_http(wifi_mgr_http_listener,
                    100,
                    [{port, 8080}],
                    [{env, [{dispatch, Dispatch}]}]),
  wifi_mgr_sup:start_link().

stop(_State) ->
  ok.
