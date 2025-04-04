%% @author admin


-module(behavior_tencent_consume).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
         write/1
        ,log/8
        ]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
    db_sql:log_tencent_consume(State),
    {ok,[]}.

log(RoleID, DateTime, Billno, Gold, GoldBonus, Type, ArgID, Desc) ->
    erlang:send(?MODULE, {add, {RoleID, DateTime, Billno, Gold, GoldBonus, Type, ArgID, Desc}}).