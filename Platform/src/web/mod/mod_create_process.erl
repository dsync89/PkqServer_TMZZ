%% @doc 记录创建角色流程的流失情况
-compile(export_all).
-module(mod_create_process).
-include("common.hrl").

log_create_process(RoleID, Accid, Process) ->
    case Process of
        0 ->
            db_func:log_create_role(Accid, RoleID),
            ok;
        1 ->
            db_func:log_select_ger(Accid, RoleID),
            ok;
        2 ->
            db_func:log_enter_game(Accid, RoleID),
            ok;
        true ->
            ?ERR("error process:~w", [Process])
    end.

handle(Req) ->
    Reply = ejson:encode({[{<<"result">>,0}]}),
    platform_tool:return(Req, Reply),
    QueryString = Req:parse_post(),
    Accid = proplists:get_value("accid", QueryString),
    RoleID = proplists:get_value("roleid", QueryString),
    Process = proplists:get_value("process", QueryString),
%%     ?ERR("Accid:~w,RoleID:~w,Process:~w,QueryString:~w", [Accid,RoleID,Process,QueryString]),
    if Accid =:= ?undefined orelse RoleID =:= ?undefined orelse Process =:= ?undefined ->
            ?ERR("log create process post data error");
        true ->
            case erlang:list_to_integer(Process) of
                0 ->
                    db_func:log_create_role(erlang:list_to_integer(Accid), erlang:list_to_integer(RoleID));
                1 ->
                    db_func:log_select_ger(erlang:list_to_integer(Accid), erlang:list_to_integer(RoleID));
                2 ->
                    db_func:log_enter_game(erlang:list_to_integer(Accid), erlang:list_to_integer(RoleID));
                true ->
                    ?ERR("~w", [Process])
            end
    end.