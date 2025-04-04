%% @author Mochi Media <dev@mochimedia.com>
%% @copyright platform Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the platform application.

-module(platform_app).
-include("common.hrl").
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for platform.
start(_Type, _StartArgs) ->
    platform_deps:ensure(),
	ets:new(?ETS_ACC_INFO, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_SER_INFO, [{keypos, 1}, set, public, named_table]),
	{IP,Port,Name,Pass,DBName,Num} = data_common:get(database),
	emysql:add_pool(?DB, Num,Name,Pass, IP, Port,DBName, utf8),
	%% mysql:start(?DB, IP, Port, Name, Pass, DBName, fun(_,_,_,_) ->ok end, utf8),
	%%mysql:connect(?DB, IP, Port, Name, Pass, DBName, utf8, true),
	%% add_connect(IP,Port,Name,Pass,DBName,Num),
    Return = platform_sup:start_link(),
    platform_db_server:start(),
    Return.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for platform.
stop(_State) ->
    ok.


add_connect(IP, Port, Name, Pass, DBName,Num) ->													  
	lists:foreach(fun(_) -> mysql:connect(?DB, IP, Port, Name, Pass, DBName, utf8, true),timer:sleep(10) end, lists:duplicate(Num, 1)).