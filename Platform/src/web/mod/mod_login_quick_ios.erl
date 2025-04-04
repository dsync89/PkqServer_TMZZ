%% @author admin
%% @doc quick SDK登录

-module(mod_login_quick_ios).
-export([handle/1]).
-include("common.hrl").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_auth(Req) of
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,Uid,DevID,Version} ->
            case db_func:get_quickios_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_QUICK, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_QUICK),
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
    ?DEBUG("debug login quick ios, QueryList = ~p~n", [QueryList]),
    App = proplists:get_value("appid", QueryList),
    Sdk = proplists:get_value("channelid", QueryList),
    Uin = proplists:get_value("userid", QueryList),
    Sess = proplists:get_value("token", QueryList),
    DevID = proplists:get_value("devid", QueryList),
    Version = proplists:get_value("version", QueryList),
    case (not lists:member(?undefined, [App,Sdk,Uin,Sess,DevID,Version])) of
        false ->
            ?ERR("quick ios login, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            URL = "http://checkuser.sdk.quicksdk.net/v2/checkUserInfo?"
                    ++mochiweb_util:urlencode(
						[{"token",Sess},{"uid",Uin},{"product_code","11496350130771143443388772385836"}]),
	    ?DEBUG("debug URL = ~s~n", [URL]),
            case httpc:request(get, {URL, []}, [], []) of
                {ok,{_,_,Ret}} ->
                    case Ret of
                        "1" ->
                            {true,Uin,DevID,Version};
                        _ ->
                            ?ERR("quick ios login, check ret error,Ret = ~p~n", [Ret]), false
                    end;
                _ ->
                    ?ERR("quick ios login, url request error, URL = ~p~n", [URL]), false
            end
    end.

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").

%% ----------------------------test code----------------------------

