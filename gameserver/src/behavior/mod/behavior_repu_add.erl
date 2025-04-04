%% @author admin
%% @doc @todo 声望获得表


-module(behavior_repu_add).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/9
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_repu_add(State),
	{ok,[]}.

log(RoleID, VipLevel, Repu, CurRepu, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Repu, CurRepu, Date, Time, Type, ArgID, Desc}}).