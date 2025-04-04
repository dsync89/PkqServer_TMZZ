%% @author caijunjun
%% @doc 点乐登录.


-module(mod_login_dianjoy).


-include("common.hrl").

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    QueryString = Req:parse_qs(),
    case check_auth(QueryString) of
        {true, Uid, DevID, Version} ->
            case db_func:get_dianjoy_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_DIANJOY, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_DIANJOY),
                    Reply = ejson:encode({[
                                            {<<"result">>,0},
                                            {<<"accid">>,Accid},
                                            {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                            {<<"valid_time">>,list_to_binary(ValidTime)},
                                            {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                            {<<"server_list">>,ServerList}
                                        ]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        {false, Reason} ->
            ?ERR("dklogin check failed, Reason:~w",[Reason]),
            Reply = ejson:encode({[{<<"result">>,2}]}),
            platform_tool:return(Req, Reply)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(QS) ->
    Uid = proplists:get_value("uid", QS),
    DevID = proplists:get_value("devid", QS),
    Version = proplists:get_value("version", QS),
    Token = proplists:get_value("token", QS),
    
    Url = "http://pay.dianjoy.com/open/api/tokenCheck.php?" ++ mochiweb_util:urlencode([{uid, Uid}, {token, Token}]),
    case httpc:request(get, {Url, []}, [], []) of
        {ok,{_,_,Ret}} ->
            case unpack_response(Ret) of
                true ->
                    {true, Uid, DevID, Version};
                Err ->
                    Err
            end;
        _ ->
            {false, "http error :" ++ Url}
    end.


unpack_response(Ret) ->
    {Ret2} = try ejson:decode(Ret) catch
                 _:_ -> {[]}
             end,
    Status = proplists:get_value(<<"status">>, Ret2),
    Desc = proplists:get_value(<<"message">>, Ret2),
    case Status of
        true -> true;
        _Err -> {false, Desc}
    end.

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").
    











