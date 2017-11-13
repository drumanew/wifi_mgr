-module(wifi_mgr_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req0, State = #state{}) ->
  {Path, Req1} = cowboy_req:path(Req0),
  {QS, Req2} = cowboy_req:qs_vals(Req1),
  {ok, BodyQS, Req3} = cowboy_req:body_qs(Req2),
  handle(Req3, Path, QS, BodyQS, State).

handle(Req, <<"/reconnect">>, _, _, State) ->
  wifi_mgr_fsm:reconnect(),
  redirect(<<"/">>, Req, State);
handle(Req, <<"/set_config">>, QS, BodyQS, State) ->
  Config = parse_config(QS, BodyQS),
  wifi_mgr_fsm:reconfigure(Config),
  redirect(<<"/">>, Req, State);
handle(Req, <<"/">>, _, _, State) ->
  response(Req, State);
handle(Req, _, _, _, State) ->
  redirect(<<"/">>, Req, State).

response(Req, State) ->
  {ok, Body} = test_dtl:render(#{ssid_list => scan_ssids(),
                                 state => wifi_state() }),
  {ok, Req2} = cowboy_req:reply(200,
                                [{<<"content-type">>, <<"text/html">>}],
                                Body,
                                Req),
  {ok, Req2, State}.

redirect(Path, Req, State) ->
  {ok, Req2} = cowboy_req:reply(302,
                                [{<<"Location">>, Path}],
                                <<>>,
                                Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

% 8 800 775 25 35

%% private

scan_ssids () ->
  RawData = os:cmd("sudo iwlist wlan0 scan"),
  parse_scan_ssids(RawData).

parse_scan_ssids (RawData) ->
  Res = re:run(RawData, "SSID:\"(?<SSID>.*)\"", [global, {capture, ["SSID"], list}]),
  case Res of
    {match, CaptureList} ->
      [ #{ name => SSID } || [SSID] <- CaptureList ];
    _ ->
      []
  end.

wifi_state () ->
  case wifi_mgr_fsm:status() of
    Status = #{ state := State } ->
      SSID = maps:get(ssid, Status, "-"),
      case State of
        checking ->
          #{ ssid => SSID, string => "NO", color => "grey", reconnect_visibility => "hidden" };
        connecting ->
          #{ ssid => SSID, string => "NO", color => "grey", reconnect_visibility => "hidden" };
        connected ->
          #{ ssid => SSID, string => "OK", color => "green", reconnect_visibility => "hidden" };
        access_point ->
          #{ ssid => SSID, string => "ERR", color => "red", reconnect_visibility => "visible" }
      end;
    _ ->
      #{ ssid => "-",
         string => "ERR",
         color => "red",
         reconnect_visibility => "hidden" }
  end.

parse_config (QS, BodyQS) ->
  io:format("QS = ~p~n", [QS]),
  io:format("BodyQS = ~p~n", [BodyQS]),
  parse_config(QS ++ BodyQS).

parse_config (KV_list) ->
  parse_config2(KV_list, #{}).

parse_config2 ([], Acc) ->
  Acc;
parse_config2 ([{<<"ssid">>, SSID_bin} | KV_list], Acc) ->
  SSID = binary_to_list(SSID_bin),
  case valid_ssid(SSID) of
    true ->
      parse_config2(KV_list, Acc#{ ssid => SSID });
    _ ->
      parse_config2(KV_list, Acc)
  end;
parse_config2 ([{<<"password">>, Password_bin} | KV_list], Acc) ->
  Password = binary_to_list(Password_bin),
  case valid_password(Password) of
    true ->
      parse_config2(KV_list, Acc#{ password => Password });
    _ ->
      parse_config2(KV_list, Acc)
  end;
parse_config2 ([_ | KV_list], Acc) ->
  parse_config2(KV_list, Acc).


valid_ssid (SSID) when is_list(SSID) andalso
                       length(SSID) > 0 andalso
                       length(SSID) =< 32 ->
  true;
valid_ssid (_) ->
  false.

valid_password (Password) when is_list(Password) andalso
                               length(Password) >= 8 ->
  true;
valid_password (_) ->
  false.
