%% @author liuqiang
%% @doc 悠悠村sdk登录处理
%% Created 2014/6/11

-module(mod_login_uu).

-include("common.hrl").
-include("record.hrl").

-export([handle/1]).


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    QueryString = Req:parse_qs(),
%%    ?ERR("debug uu login, the query_string is: ~p~n", [QueryString]),
    Uuptid = proplists:get_value("uuptid", QueryString),
    Ssotoken = proplists:get_value("ssotoken", QueryString),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    case not lists:member(?undefined, [Uuptid,Ssotoken,DevID,Version]) of
        false ->
            ?ERR("uu login query_string error, the query_string is: ~p~n", [QueryString]),
            Reply = ejson:encode({[{<<"result">>,3}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        true ->
            case check_auth(Ssotoken) of
                false ->
                    ?ERR("uu login check_auth failed, the ssotoken is: ~p~n", [Ssotoken]),
                    Reply = ejson:encode({[{<<"result">>,2}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                true ->
                    case db_func:get_uu_accid(Uuptid, DevID,util:get_ip_from_req(Req)) of
                        [] ->
                            ?ERR("uu login get accid failed~n", []),
                            Reply = ejson:encode({[{<<"result">>,1}]}),
                            Req:ok({"text/html; charset=utf-8",Reply});
                        [Accid] ->
                            {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_UU, Version),
                            ValidTime = integer_to_list(util:now()+?KEY_VALID_INTERVAL),
                            ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                            LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                            pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_UU),
                            Reply = ejson:encode({[
                                                    {<<"result">>,0},
                                                    {<<"accid">>,Accid},
                                                    {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                                    {<<"valid_time">>,list_to_binary(ValidTime)},
                                                    {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                                    {<<"server_list">>,ServerList}
                                                    ]}),
%%                            ?ERR("Login Reply: ~p~n", [Reply]),
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Ssotoken) ->
    URL = "http://passport.uuserv20.com/checkSSOToken.do?jsonString={\"token\":\""++Ssotoken++"\"}",
    case httpc:request(get, {URL,[]}, [], []) of
        {ok, {_,_,Ret}} ->
            try ejson:decode(Ret) of
                {[{<<"errorCode">>,0}]} -> true;
                _ -> false
            catch _:_ -> false end;
        _ -> false
    end,
    true.

latestSL(LatestSL) ->
    string:join([integer_to_list(E) || E <- LatestSL], ",").



%% ------------------------------------------------------------------------------------------------------------
test() ->
    Ssotoken = "b3066822e277f30638966f3e23719de2",
    Ret = check_auth(Ssotoken),
    ?ERR("the check auth result is: ~p~n", [Ret]).
