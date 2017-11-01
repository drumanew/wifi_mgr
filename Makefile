PROJECT = wifi_mgr
PROJECT_DESCRIPTION = Raspberry Pi WiFi Manager
PROJECT_VERSION = 0.1.0

DEPS = cowboy erlydtl

SP = 2

include erlang.mk

SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT))'

