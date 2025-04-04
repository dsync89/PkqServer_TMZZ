%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc platform.

-module(platform_pay).
-include("common.hrl").
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the platform server.
start() ->
    platform_deps:ensure(),
    ensure_started(crypto),
	ensure_started(inets),
    application:start(platform_pay).


%% @spec stop() -> ok
%% @doc Stop the platform server.
stop() ->
    application:stop(platform_pay).
