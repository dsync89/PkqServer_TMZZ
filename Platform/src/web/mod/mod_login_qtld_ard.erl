%% @author liuqiang
%% @doc 奇天乐地ARD登录

-module(mod_login_qtld_ard).
-export([handle/1]).
-include("common.hrl").

-define(GID, "2036").
-define(APPKEY, "af31c25c0a8608a9b7f4d0ea8c0fdfcc").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_auth(Req) of
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,Uid,DevID,Version} ->
            case db_func:get_qtld_ard_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_QTLD_ARD, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_QTLD_ARD),
                    Reply = ejson:encode({[
                                            {<<"result">>,0},
                                            {<<"accid">>,Accid},
                                            {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                            {<<"valid_time">>,list_to_binary(ValidTime)},
                                            {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                            {<<"server_list">>,ServerList}
                                        ]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Req) ->
    QueryList = Req:parse_qs(),
%%    ?ERR("debug login qtld_ard, QueryList = ~p~n", [QueryList]),
%%     Pid = proplists:get_value("pid", QueryList),
    UserID = proplists:get_value("uid", QueryList),
    SessionID = proplists:get_value("sessionID", QueryList),
    DevID = proplists:get_value("devid", QueryList),
    Version = proplists:get_value("version", QueryList),
    case (not lists:member(?undefined, [UserID,SessionID,DevID,Version])) of
        false ->
            ?ERR("qtld_ard login, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            URL = "http://www.44755.com/notify-login",
            CType = "application/x-www-form-urlencoded",
            Body = mochiweb_util:urlencode([{pid,"1"},{session_id,SessionID},{uid,UserID}]),
            case httpc:request(post, {URL,[],CType,Body}, [], []) of
                {ok,{_,_,Ret}} ->
                    case Ret of
                        "1" ->
                            {true,UserID,DevID,Version};
                        _ ->
                            ?ERR("qtld_ard login, check ret error,Ret = ~p~n", [Ret]), false
                    end;
                _ ->
                    ?ERR("qtld_ard login, url request error, URL = ~p~n", [URL]), false
            end
    end.

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").

%% ----------------------------test code----------------------------

