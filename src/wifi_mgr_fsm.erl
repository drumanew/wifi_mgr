-module(wifi_mgr_fsm).
-behaviour(gen_fsm).

-compile(export_all).

%% API.
-export([start_link/0,
         status/0,
         reconfigure/1,
         reconnect/0]).

%% gen_fsm.
-export([init/1]).

-export([checking/2]).
-export([connecting/2]).
-export([connected/2]).
-export([access_point/2]).

-export([handle_event/3]).

-export([checking/3]).
-export([connecting/3]).
-export([connected/3]).
-export([access_point/3]).

-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-define (POLL_INT, 10000).
-define (FAILS_REMAINS, 3).

-record(state, { config, timer, fails_remains = ?FAILS_REMAINS }).

-define (DEBUG_LOG, true).

-ifdef (DEBUG_LOG).
  -define (LOG(Format, Args), io:format(Format, Args)).
-else.
  -define (LOG(Format, Args), (fun (F, A) -> F, A, ok end)(Format, Args)).
-endif.

-define (LOG(IoList), ?LOG(IoList, [])).


%% API.

-spec start_link() -> {ok, pid()}.
start_link () ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], [{debug, [trace]}]).

status () ->
  gen_fsm:sync_send_all_state_event(?MODULE, status).

reconfigure (Config) ->
  event({reconfigure, Config}).

reconnect () ->
  event(reconnect).

%% gen_fsm.

init([]) ->
  stop_access_point(),
  State0 = #state{ timer = poll_timer() },
  {next_state, NextState, State} = switch(checking, State0),
  {ok, NextState, State}.

checking({configuration, {ok, Config}}, StateData) ->
  switch(connecting, StateData#state{ config = Config });
checking({configuration, {error, _Reason}}, StateData) ->
  switch(access_point, StateData);
checking(_Event, StateData) ->
  {next_state, checking, StateData}.

connecting({connection, ok}, StateData) ->
  switch(connected, StateData);
connecting({connection, {error, _Reason}}, StateData) ->
  switch(access_point, StateData);
connecting(_Event, StateData) ->
  {next_state, connecting, StateData}.

connected({connection, closed}, StateData = #state{ fails_remains = 0 }) ->
  switch(access_point, StateData);
connected({connection, closed}, StateData = #state{ fails_remains = FR }) ->
  switch(checking, StateData#state{ fails_remains = FR - 1 });
connected({reconfigure, Config}, StateData) ->
  case update_config(Config) of
    ok ->
      switch(checking, StateData#state{ fails_remains = ?FAILS_REMAINS });
    {error, _Reason} ->
      {next_state, connected, StateData}
  end;
connected(_Event, StateData) ->
  {next_state, connected, StateData}.

access_point({reconfigure, Config}, StateData) ->
  case update_config(Config) of
    ok ->
      stop_access_point(),
      switch(checking, StateData#state{ fails_remains = ?FAILS_REMAINS });
    {error, _Reason} ->
      {next_state, access_point, StateData}
  end;
access_point(reconnect, StateData = #state{ config = Config }) ->
  reconfigure(Config),
  {next_state, access_point, StateData};
access_point(_Event, StateData) ->
  {next_state, access_point, StateData}.

handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

checking(_Event, _From, StateData) ->
  {reply, ignored, checking, StateData}.

connecting(_Event, _From, StateData) ->
  {reply, ignored, connecting, StateData}.

connected(_Event, _From, StateData) ->
  {reply, ignored, connected, StateData}.

access_point(_Event, _From, StateData) ->
  {reply, ignored, access_point, StateData}.

handle_sync_event(status, _From, StateName, StateData) ->
  Config = StateData#state.config,
  Reply0 = #{ state => StateName },
  Reply1 = case Config of
             undefined ->
              Reply0;
             _ when is_map(Config) ->
              maps:merge(Reply0, Config)
           end,
  {reply, Reply1, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info({timeout, Ref, poll},
            StateName,
            StateData = #state{ timer = Ref }) ->
  check_connection(StateName, StateData),
  {next_state, StateName, StateData#state{ timer = poll_timer() }};
handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% private

event (Event) ->
  gen_fsm:send_event(?MODULE, Event).

poll_timer () ->
  erlang:start_timer(?POLL_INT, self(), poll).

get_env (Param) ->
  case application:get_env(wifi_mgr, Param) of
    {ok, Val} ->
      Val;
    undefined ->
      Err = lists:flatten(io_lib:format("~p not set", [Param])),
      throw(Err)
  end.

% get_conf (Param, Config) ->
%   case maps:find(Param, Config) of
%     {ok, Val} ->
%       Val;
%     error ->
%       Err = lists:flatten(io_lib:format("~p not set", [Param])),
%       throw(Err)
%   end.

conf_dir () ->
  get_env(conf_dir).

conf_file () ->
  get_env(conf_file).

access_point_ssid () ->
  get_env(access_point_ssid).

access_point_password () ->
  get_env(access_point_password).

access_point_channel () ->
  get_env(access_point_channel).

wlan0_ip_addr () ->
  get_env(wlan0_ip_addr).

wlan0_ip_mask () ->
  get_env(wlan0_ip_mask).

wlan0_ip_network() ->
  get_env(wlan0_ip_network).

dhcp_ip_addr_min () ->
  get_env(dhcp_ip_addr_min).

dhcp_ip_addr_max () ->
  get_env(dhcp_ip_addr_max).

dhcp_lease_time () ->
  get_env(dhcp_lease_time).

conf_file_name () ->
  filename:join(conf_dir(), conf_file()).

switch (checking, State) ->
  check_config(),
  {next_state, checking, State};
switch (connecting, State = #state{ config = Config }) ->
  connect(Config),
  {next_state, connecting, State};
switch (connected, State) ->
  {next_state, connected, State};
switch (access_point, State) ->
  start_access_point(),
  {next_state, access_point, State}.

check_config () ->
  try
    ConfFile = conf_file_name(),
    Config = read_config(ConfFile),
    event({configuration, {ok, Config}})
  catch
    throw:Reason ->
      event({configuration, {error, Reason}});
    _Class:_Error ->
      event({configuration, {error, {_Class, _Error}}})
  end.

read_config (FileName) ->
  case filelib:is_file(FileName) of
    true ->
      case filelib:is_regular(FileName) of
        true ->
          case file:consult(FileName) of
            {ok, ConfigList} ->
              maps:from_list(ConfigList);
            {error, Reason} ->
              Err =
                lists:flatten(io_lib:format("cannot read configuration file: ~p",
                                            [Reason])),
              throw(Err)
          end;
        false ->
          case filelib:is_dir(FileName) of
            true ->
              throw("configuration file is a directory");
            false ->
              throw("configuration file has undefined file type")
          end
      end;
    _ ->
      case file:write_file(FileName, <<>>) of
        ok ->
          throw("empty configuration file");
        {error, Reason} ->
          Err =
            lists:flatten(io_lib:format("cannot create configuration file: ~p",
                                        [Reason])),
          throw(Err)
      end
  end.

update_config (Config) ->
  try
    ConfFile = conf_file_name(),
    write_config(ConfFile, Config)
  catch
    throw:Reason ->
      {error, Reason};
    _Class:_Error ->
      {error, {_Class, _Error}}
  end.

write_config (ConfFile, Config) when is_list(ConfFile) andalso is_map(Config) ->
  case file:open(ConfFile, [write]) of
    {ok, Fd} ->
      write_config(Fd, maps:to_list(Config));
    {error, Reason} ->
      Err =
        lists:flatten(io_lib:format("cannot open configuration file for writing: ~p",
                                    [Reason])),
      throw(Err)
  end;
write_config (Fd, []) ->
  file:close(Fd);
write_config (Fd, [ Cfg | ConfigList ]) ->
  io:format(Fd, "~p.~n", [Cfg]),
  write_config(Fd, ConfigList).

connect (Config = #{ ssid := SSID, password := Pass })
  when is_list(SSID) andalso
       is_list(Pass) andalso
       length(SSID) >  0 andalso
       length(Pass) >= 8 ->
  try
    configure_hardware(Config),
    event({connection, ok})
  catch
    throw:Reason ->
      event({connection, {error, Reason}});
    _Class:_Error ->
      event({connection, {error, {_Class, _Error}}})
  end;
connect (_) ->
  event({connection, {error, bad_config}}).

configure_hardware (Config) ->
  {ok, Data} = wpa_supplicant_dtl:render([{config, Config}]),
  case file:write_file("/etc/wpa_supplicant/wpa_supplicant.conf",
                       iolist_to_binary(Data)) of
    ok ->
      ok;
    {error, Reason} ->
      Err1 = lists:flatten(io_lib:format("cannot write wpa_supplicant.conf: ~p",
                                         [Reason])),
      throw(Err1)
  end,
  cmd("ifconfig wlan0 up"),
  restart_wpa_supplicant(),
  cmd("wpa_cli -i wlan0 reconfigure").

check_connection (connected, #state{ config = #{ ssid := SSID } }) ->
  cmd("iwconfig wlan0 | grep -o SSID:.* | grep -o " ++ SSID ++ " > /tmp/.wifi_mgr"),
  Bin = iolist_to_binary(SSID),
  Size = size(Bin),
  case file:read_file("/tmp/.wifi_mgr") of
    {ok, <<Bin:Size/binary, _/binary>>} ->
      ok;
    _ ->
      event({connection, closed})
  end;
check_connection (_, _) ->
  ok.

start_access_point () ->
  cmd("rm /etc/wpa_supplicant/wpa_supplicant.conf"),
  cmd("killall -9 wpa_supplicant"),
  cmd("systemctl stop dnsmasq"),
  cmd("systemctl stop hostapd"),
  backup("/etc/dhcpcd.conf"),
  cmd("echo 'denyinterface wlan0' >> /etc/dhcpcd.conf"),
  write_network_interfaces_wlan0(),
  cmd("ip addr flush dev wlan0"),
  cmd("systemctl restart dhcpcd"),
  cmd("ifdown wlan0"),
  cmd("ifup wlan0"),
  backup("/etc/dnsmasq.conf"),
  write_dnsmasq_conf(),
  backup("/etc/hostapd/hostapd.conf"),
  write_hostapd_conf(),
  backup("/etc/default/hostapd"),
  cmd("echo 'DAEMON_CONF=\"/etc/hostapd/hostapd.conf\"' >> /etc/default/hostapd"),
  cmd("systemctl start hostapd"),
  cmd("systemctl start dnsmasq").

stop_access_point () ->
  cmd("systemctl stop dnsmasq"),
  cmd("systemctl stop hostapd"),
  restore("/etc/dhcpcd.conf"),
  remove_network_interfaces_wlan0(),
  restore("/etc/dnsmasq.conf"),
  restore("/etc/hostapd/hostapd.conf"),
  restore("/etc/default/hostapd"),
  cmd("systemctl restart dhcpcd"),
  cmd("ip addr flush dev wlan0"),
  cmd("ifdown wlan0"),
  cmd("ifup wlan0").

backup (File) ->
  BackupFile = File ++ ".backup",
  case filelib:is_regular(File) of
    false ->
      ?LOG("backup: ~p NO~n", [File]),
      ok;
    _ ->
      case file:copy(File, BackupFile) of
        {ok, _} ->
          ?LOG("backup: ~p OK~n", [File]),
          ok;
        {error, Reason} ->
          ?LOG("backup: ~p BAD: ~p~n", [File, Reason]),
          throw(Reason)
      end
  end.

restore (File) ->
  BackupFile = File ++ ".backup",
  case filelib:is_regular(BackupFile) of
    false ->
      ?LOG("restore: ~p NO~n", [File]),
      ok;
    _ ->
      case file:rename(BackupFile, File) of
        ok ->
          ?LOG("restore: ~p OK~n", [File]),
          ok;
        {error, Reason} ->
          ?LOG("restore: ~p BAD: ~p~n", [File, Reason]),
          throw(Reason)
      end
  end.

write_network_interfaces_wlan0 () ->
  {ok, Data} = wlan0_dtl:render([{ip, wlan0_ip_addr()},
                                 {mask, wlan0_ip_mask()},
                                 {network, wlan0_ip_network()}]),
  file:write_file("/etc/network/interfaces.d/wlan0", iolist_to_binary(Data)).

write_dnsmasq_conf () ->
  {ok, Data} = dnsmasq_conf_dtl:render([{min, dhcp_ip_addr_min()},
                                        {max, dhcp_ip_addr_max()},
                                        {network, wlan0_ip_network()},
                                        {leasetime, dhcp_lease_time()}]),
  file:write_file("/etc/dnsmasq.conf", iolist_to_binary(Data)).

write_hostapd_conf () ->
  {ok, Data} = hostapd_conf_dtl:render([{ssid, access_point_ssid()},
                                        {password, access_point_password()},
                                        {channel, access_point_channel()}]),
  file:write_file("/etc/hostapd/hostapd.conf", iolist_to_binary(Data)).

remove_network_interfaces_wlan0 () ->
  file:delete("/etc/network/interfaces.d/wlan0").

cmd (Command) ->
  ?LOG("$ ~s~n~s", [Command, os:cmd(Command)]).

restart_wpa_supplicant () ->
  cmd("wpa_supplicant -B -c/etc/wpa_supplicant/wpa_supplicant.conf -iwlan0").
