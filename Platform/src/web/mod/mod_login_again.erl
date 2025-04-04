%% @doc 重登陆时记录loglogin

-compile(export_all).
-module(mod_login_again).
-include("common.hrl").

log_login_again(Accid) ->
    Type = db_func:get_acctype(Accid),
    case Type of
        SrcType when is_integer(SrcType) ->
            pm_platform_server:add_loglogin(Accid, SrcType),
            ok;
        Err ->
            ?ERR("loglogin error, common.config no src_type:~p~n",[Err])
    end.

handle(Req) ->
	Reply = ejson:encode({[{<<"result">>,0}]}),
    platform_tool:return(Req, Reply),
    QueryString = Req:parse_post(),
    %io:format("qs=~100p\n",[QueryString]),
    Accid = proplists:get_value("accid", QueryString),
	Type = db_func:get_acctype(erlang:list_to_integer(Accid)),
    if Accid =:= ?undefined ->
            ?ERR("loglogin post data error");
        true ->
            case Type of
				SrcType when is_integer(SrcType) ->
                    erlang:spawn(fun() -> pm_platform_server:add_loglogin(erlang:list_to_integer(Accid), SrcType) end);
                Err ->
                    ?ERR("loglogin error, common.config no src_type:~p~n",[Err])
            end
    end.