
-module(behavior_logout_log).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
         write/1
        ,log/6
        ]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
    case State of
        [] ->
            next;
        _ ->
            db_sql:log_logout(State),
            erlang:send(db_manage_server, {logout_list, State})
    end,
    {ok,[]}.

log(Accid, RoleID, DevID, IP, Duration,SrcType) ->
    erlang:send(?MODULE, {add, {Accid, RoleID, DevID, IP, erlang:localtime(),Duration,SrcType}}).