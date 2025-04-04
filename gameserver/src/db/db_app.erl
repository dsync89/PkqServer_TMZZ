%% @author admin
%% @doc database application 
%% Created 2013-2-19


-module(db_app).
-include("common.hrl").
-include("log_create_sql.hrl").
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
	%db:start(),
	db:connect_mysql(),
	ets:new(?ETS_ROLE_ONLINE, [{keypos, 1}, set, public, named_table]),
    ets:new(?ROLE_PAY_ARG, [{keypos, 1}, set, public, named_table]),
	%% 缓存玩家的role信息,keypos=2,如何清理缓存?
	ets:new(?ETS_ROLE_PUBLIC, [{keypos, 2}, set, public, named_table]),
	ets:new(?ETS_HOMESTEAD_DATA_BASE_TABLE, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_HOMESTEAD_DATA_MACHINE_TABLE, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_HOMESTEAD_DATA_LOG_TABLE, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_ENARGY_FRIEND_DATA, [{keypos, 2}, set, public, named_table]),
	ets:new(?ETS_ROLE_DUMP, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_CLOSE_FLAG, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_ETC, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_ROLE_ACTIVITY, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_DUNGEON_MON_CACHE,[{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_ID, [{keypos, 1}, set, public, named_table]),
    create_replay_table(),
	tk_id:init(),
	ets:new(?ETS_RANDOM_SEED,[{keypos,1},named_table,set,public]),
	ets:insert(?ETS_RANDOM_SEED,{seed,random:uniform(100)}),
    case 'db_sup':start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_replay_table() ->
	lists:foreach(fun(ReplayType) ->
						  CeateSql = io_lib:format(?CREATE_REPLAY_SQL, [ReplayType]),
						  db_sql:sql_execute_with_log(CeateSql)
				  end, [?REPLAY_TYPE_PVP,?REPLAY_TYPE_RULE|?REPLAY_TYPE_LIST_EXCEPT_PVP_RULE]).

