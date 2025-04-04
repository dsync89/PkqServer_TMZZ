%% @author admin

-module(behavior_login_log).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/4
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_login(State),
	{ok,[]}.

log(Accid, RoleID, DevID, IP) ->
	DateTime = erlang:localtime(),
	Duration = db_sql:get_role_lastLogoutTime(RoleID) - db_sql:get_role_lastLoginTime(RoleID),   
	if Duration >= 0 ->
		   erlang:send(?MODULE, {add, {Accid, RoleID, DevID, IP, DateTime,Duration}});
	   true->
		   erlang:send(?MODULE, {add, {Accid, RoleID, DevID, IP, DateTime,0}})
	end.