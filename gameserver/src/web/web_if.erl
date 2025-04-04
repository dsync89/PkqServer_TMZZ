%% @author admin
%% @doc web interface 模块

-module(web_if).
 
-export([start/0, stop/0]).

ensure_started(App)  ->
    case application:start(App) of
        ok  ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    web_if_deps:ensure(),
    ensure_started(crypto),
    ensure_started(inets),
    application:start(web_if).

stop()  ->  
    application:stop(web_if).