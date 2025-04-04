%% @author admin
%% @doc 服务器总控制接口
%% Created 2013-2-19


-module(tk).
-include("common.hrl").

-export([start/0,stop/0]).

-define(APP, [sasl, crypto, inets, logger, tool, web_if, emysql, db, world, behavior, role, gateway]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    case is_cross_server() of
        false ->
            tk_misc:start_applications(?APP);
        true ->
            tk_misc:start_applications(?APP -- [web_if, gateway])
    end,
    httpc:set_options([{max_pipeline_length,2000}, {max_keep_alive_length,5000}, {max_sessions,2000}]),
    %% 启动后对虚拟机整体垃圾回收一次
    lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, erlang:processes()).
	
stop() ->
	ets:insert(?ETS_CLOSE_FLAG, {close, true}),
    case  is_cross_server() of
        false ->
            %% 先t所有玩家,并关闭游戏入口。
            application:stop(gateway),
            %% 等待10秒，等玩家进程保存数据
            timer:sleep(10*1000),
            %% 关闭mochiweb接口
            application:stop(web_if);
        true ->
            next
    end,
	%% 关闭玩家application
	application:stop(role),
	%% 关闭世界application，并等待10秒写完数据
	application:stop(behavior),
    application:stop(world),
	timer:sleep(10*1000),
	%% 
	tk_misc:stop_applications( [sasl, crypto, inets, logger, tool, emysql, db]),
	timer:sleep(3*1000).

is_cross_server() ->
    {ok, TupleList} = file:consult(filename:join([tk_config:root_dir(), "setting/setting.config"])),
    case lists:keyfind(is_cross_master, 1, TupleList) of
        {is_cross_master,true} ->
            true;
        _ ->
            false
    end.

	




