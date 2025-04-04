%% @author Mochi Media <dev@mochimedia.com>
%% @copyright platform Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the platform application.

-module(platform_pay_app).
-include("common.hrl").
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for platform.
start(_Type, _StartArgs) ->
    platform_deps:ensure(),
	?DEBUG("platform start1"),
    tk_config:preload_config(),
	?DEBUG("platform start2"),
	{IP,Port,Name,Pass,DBName,Num} = data_common:get(database),
	?DEBUG("platform start3"),
	emysql:add_pool(?DB, Num,Name,Pass, IP, Port,DBName, utf8),
	?DEBUG("platform start4"),
    platform_pay_sup:start_link().
	

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for platform.
stop(_State) ->
    ok.


add_connect(IP, Port, Name, Pass, DBName,Num) ->													  
	lists:foreach(fun(_) -> mysql:connect(?DB, IP, Port, Name, Pass, DBName, utf8, true),timer:sleep(10) end, lists:duplicate(Num, 1)).