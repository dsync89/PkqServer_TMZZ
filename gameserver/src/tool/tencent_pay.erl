-module(tencent_pay).

-include("def_role.hrl").

-export([get_gold/2,
         pay_gold/7,
         add_gold/3,
         cancel_pay/4,
         notify_client_update_pay_arg/1,
		 check_pay_arg/1,
         check_pay_arg/3
        ]).

-define(APPID, "1103881795").
-define(APPKEY, "sBb0md511pxcPQ3l&").
-define(TENCENT_GIFT_ID, "1433491258PID201501051446542103").
-define(TENCENT_ACTIVITY_ID, "UM150105144654187").

%% 取消游戏币支付
cancel_pay(RoleID, SrcType, Billno, ReturnGold) ->
    case get_arg(RoleID,SrcType) of
        {true, OpenID, PayToken, Openkey, PF, PFKey} ->
            cancel_pay(OpenID, PayToken, Openkey, PF, PFKey, SrcType, Billno, ReturnGold, RoleID);
        false ->
            ?ERR("cancel_pay RoleID:~w, no_arg", [RoleID]),
            {false, no_arg}
    end.

cancel_pay(OpenID, PayToken, Openkey, PF, PFKey, SrcType, Billno, ReturnGold, RoleID) ->
    case data_setting:get(is_release) of
        true ->
            URL = "http://msdk.qq.com/mpay/cancel_pay_m?";
        false ->
            URL = "http://msdktest.qq.com/mpay/cancel_pay_m?"
    end,
    QueryString = gen_query_string_cancel_pay(OpenID, PayToken, Openkey, PF, PFKey, Billno, ReturnGold),
    Cookie = gen_cookie_cancel_pay(SrcType),
    Sig = cacl_sig(QueryString, "/mpay/cancel_pay_m"),
    http_request_cancel_pay(URL ++ mochiweb_util:urlencode([{"sig", Sig} | QueryString]), Cookie, RoleID).

gen_cookie_cancel_pay(SrcType) ->
    case SrcType of
        ?ACCOUNT_TYPE_QQ ->
            SessionType = "kp_actoken",
            SessionID = "openid";
        ?ACCOUNT_TYPE_WEIXIN ->
            SessionType = "wc_actoken",
            SessionID = "hy_gameid"
    end,
    "session_id=" ++ SessionID ++ ";session_type=" ++ SessionType ++ ";org_loc=" ++ http_uri:encode("/mpay/cancel_pay_m").

gen_query_string_cancel_pay(OpenID, PayToken, Openkey, PF, PFKey, Billno, ReturnGold) ->
    [
     {"amt" , erlang:integer_to_list(ReturnGold)} ,
     {"appid" , ?APPID} ,
     {"billno" , Billno} ,
     {"openid" , OpenID} , 
     {"openkey" , Openkey} ,
     {"pay_token" , PayToken} , 
     {"pf" , PF} , 
     {"pfkey" , PFKey} ,
     {"ts" , erlang:integer_to_list(util:now())} ,
     {"zoneid" , erlang:integer_to_list(data_setting:get(server_id))}
    ].

http_request_cancel_pay(URL, Cookie, RoleID) ->
    case httpc:request(get, {URL,[{"Cookie", Cookie}]},[{timeout, 20000}, {connect_timeout, 15000}], []) of
        {ok, {_,_,R}} ->
            {RS} = ejson:decode(R),
            Ret = get_value(RS, <<"ret">>),
            Msg = get_value(RS, <<"msg">>),
            case Ret of
                0 ->
                    true;
                1018 ->
                    ?ERR("cancel_pay Ret:~w, Msg:~w,URL:~s, Cookie:~s, RoleID:~w", [Ret, Msg, URL, Cookie, RoleID]),
                    {false, check_fail};
                _ ->
                    ?ERR("cancel_pay Ret:~w, Msg:~w,URL:~s, Cookie:~s, R:~w, RoleID:~w", [Ret, Msg, URL, Cookie,R, RoleID]),
                    {false, unknow_error}
            end;
        Err ->
            ?ERR("cancel_pay Err:~w,URL:~s, Cookie:~s, RoleID:~w", [Err, URL, Cookie, RoleID]),
            {false, network_error}
    end.

%% 赠送游戏币
add_gold(RoleID, SrcType, AddGold) ->
    case SrcType =:= ?ACCOUNT_TYPE_QQ orelse SrcType =:= ?ACCOUNT_TYPE_WEIXIN of
        true ->
            Result =
                case get_arg(RoleID,SrcType) of
                    {true, OpenID, PayToken, Openkey, PF, PFKey} ->
                        add_gold(OpenID, PayToken, Openkey, PF, PFKey, SrcType, AddGold, RoleID);
                    false ->
                        notify_client_update_pay_arg(RoleID),
                        ?ERR("add_gold RoleID:~w, no_arg", [RoleID]),
                        {false, no_arg}
                end,
            case Result of
                {true, _} ->
                    next;
                _ ->
                    ?CATCH(role_data:fill_gold_bonus(AddGold))
            end,
            Result;
        false ->
            ignore
    end.

add_gold(OpenID, PayToken, Openkey, PF, PFKey, SrcType, AddGold, RoleID) ->
    case data_setting:get(is_release) of
        true ->
            URL = "http://msdk.qq.com/mpay/present_m?";
        false ->
            URL = "http://msdktest.qq.com/mpay/present_m?"
    end,
    QueryString = gen_query_string_add_gold(OpenID, PayToken, Openkey, PF, PFKey, AddGold),
    Cookie = gen_cookie_add_gold(SrcType),
    Sig = cacl_sig(QueryString, "/mpay/present_m"),
    http_request_add_gold(URL ++ mochiweb_util:urlencode([{"sig", Sig} | QueryString]), Cookie, RoleID).

gen_cookie_add_gold(SrcType) ->
    case SrcType of
        ?ACCOUNT_TYPE_QQ ->
            SessionType = "kp_actoken",
            SessionID = "openid";
        ?ACCOUNT_TYPE_WEIXIN ->
            SessionType = "wc_actoken",
            SessionID = "hy_gameid"
    end,
    "session_id=" ++ SessionID ++ ";session_type=" ++ SessionType ++ ";org_loc=" ++ http_uri:encode("/mpay/present_m").

gen_query_string_add_gold(OpenID, PayToken, Openkey, PF, PFKey, AddGold) ->
    [
     {"appid" , ?APPID} ,
     {"discountid" , ?TENCENT_ACTIVITY_ID} ,
     {"giftid" , ?TENCENT_GIFT_ID} ,
     {"openid" , OpenID} , 
     {"openkey" , Openkey} ,
     {"pay_token" , PayToken} , 
     {"pf" , PF} , 
     {"pfkey" , PFKey} ,
     {"presenttimes" , erlang:integer_to_list(AddGold)} ,
     {"ts" , erlang:integer_to_list(util:now())} ,
     {"zoneid" , erlang:integer_to_list(data_setting:get(server_id))}
    ].

http_request_add_gold(URL, Cookie, RoleID) ->
    case httpc:request(get, {URL,[{"Cookie", Cookie}]},[{timeout, 20000}, {connect_timeout, 15000}], []) of
        {ok, {_,_,R}} ->
            {RS} = ejson:decode(R),
            Ret = get_value(RS, <<"ret">>),
            Balance = get_value(RS, <<"balance">>),
            Msg = get_value(RS, <<"msg">>),
            case Ret of
                0 ->
                    {true, Balance};
                1001 ->
                    notify_client_update_pay_arg(RoleID),
                    ?ERR("add_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, RoleID:~w", [Ret, Msg, URL, Cookie, RoleID]),
                    {false, arg_error};
                1018 ->
                    notify_client_update_pay_arg(RoleID),
                    ?ERR("add_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, RoleID:~w", [Ret, Msg, URL, Cookie, RoleID]),
                    {false, check_fail};
                _ ->
                    notify_client_update_pay_arg(RoleID),
                    ?ERR("add_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, R:~w, RoleID:~w", [Ret, Msg, URL, Cookie, R, RoleID]),
                    {false, unknow_error}
            end;
        Err ->
            notify_client_update_pay_arg(RoleID),
            ?ERR("add_gold Err:~w, URL:~s, Cookie:~s, RoleID:~w", [Err, URL, Cookie, RoleID]),
            {false, network_error}
    end.

%% 查询游戏币余额

get_gold(RoleID, SrcType) ->
    case get_arg(RoleID,SrcType) of
        {true, OpenID, PayToken, Openkey, PF, PFKey} ->
            get_gold(OpenID, PayToken, Openkey, PF, PFKey, SrcType, RoleID);
        false ->
            ?ERR("get_gold RoleID:~w, no_arg", [RoleID]),
            {false, no_arg}
    end.

get_gold(OpenID, PayToken, Openkey, PF, PFKey, SrcType, RoleID) ->
    case data_setting:get(is_release) of
        true ->
            URL = "http://msdk.qq.com/mpay/get_balance_m?";
        false ->
            URL = "http://msdktest.qq.com/mpay/get_balance_m?"
    end,
    QueryString = gen_query_string_get_gold(OpenID, PayToken, Openkey, PF, PFKey),
    Cookie = gen_cookie_get_gold(SrcType),
    Sig = cacl_sig(QueryString, "/mpay/get_balance_m"),
    http_request_get_gold(URL ++ mochiweb_util:urlencode([{"sig", Sig} | QueryString]), Cookie, RoleID).

gen_cookie_get_gold(SrcType) ->
    case SrcType of
        ?ACCOUNT_TYPE_QQ ->
            SessionType = "kp_actoken",
            SessionID = "openid";
        ?ACCOUNT_TYPE_WEIXIN ->
            SessionType = "wc_actoken",
            SessionID = "hy_gameid"
    end,
    "session_id=" ++ SessionID ++ ";session_type=" ++ SessionType ++ ";org_loc=" ++ http_uri:encode("/mpay/get_balance_m").

gen_query_string_get_gold(OpenID, PayToken, Openkey, PF, PFKey) ->
    [
     {"appid" , ?APPID} ,
     {"openid" , OpenID} , 
     {"openkey" , Openkey} ,
     {"pay_token" , PayToken} , 
     {"pf" , PF} , 
     {"pfkey" , PFKey} ,
     {"ts" , erlang:integer_to_list(util:now())} ,
     {"zoneid" , erlang:integer_to_list(data_setting:get(server_id))}
    ].

http_request_get_gold(URL, Cookie, RoleID) ->
    case httpc:request(get, {URL,[{"Cookie", Cookie}]},[{timeout, 20000}, {connect_timeout, 15000}], []) of
        {ok, {_,_,R}} ->
            {RS} = ejson:decode(R),
            Ret = get_value(RS, <<"ret">>),
            Balance = get_value(RS, <<"balance">>),
            GenBalance = get_value(RS, <<"gen_balance">>),
            Msg = get_value(RS, <<"msg">>),
            case Ret of
                0 ->
                    {true, Balance - GenBalance, GenBalance};
                1001 ->
                    ?ERR("get_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, RoleID:~w", [Ret, Msg, URL, Cookie, RoleID]),
                    {false, arg_error};
                1018 ->
                    ?ERR("get_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, RoleID:~w", [Ret, Msg, URL, Cookie, RoleID]),
                    {false, check_fail};
                _ ->
                    ?ERR("get_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, R:~w, RoleID:~w", [Ret, Msg, URL, Cookie, R, RoleID]),
                    {false, unknow_error}
            end;
        Err ->
            ?ERR("get_gold Err:~w, URL:~s, Cookie:~s, RoleID:~w", [Err, URL, Cookie, RoleID]),
            {false, network_error}
    end.

%% 扣除游戏币

pay_gold(RoleID, SrcType, Gold, GoldBonus, Type, ArgID, Desc) ->
    case get_arg(RoleID,SrcType) of
        {true, OpenID, PayToken, Openkey, PF, PFKey} ->
            pay_gold(RoleID, SrcType, OpenID, PayToken, Openkey, PF, PFKey, Gold, GoldBonus, Type, ArgID, Desc);
        false ->
            ?ERR("pay_gold RoleID:~w, no_arg", [RoleID]),
            notify_client_update_pay_arg(RoleID),
            {false, no_arg}
    end.

pay_gold(RoleID, SrcType, OpenID, PayToken, Openkey, PF, PFKey, Gold, GoldBonus, Type, ArgID, Desc) ->
    case data_setting:get(is_release) of
        true ->
            URL = "http://msdk.qq.com/mpay/pay_m?";
        false ->
            URL = "http://msdktest.qq.com/mpay/pay_m?"
    end,
    QueryString = gen_query_string_pay_gold(OpenID, PayToken, Openkey, PF, PFKey, Gold + GoldBonus),
    Cookie = gen_cookie_pay_gold(SrcType),
    Sig = cacl_sig(QueryString, "/mpay/pay_m"),
    http_request_pay_gold(URL ++ mochiweb_util:urlencode([{"sig",Sig} | QueryString]), RoleID, Gold, GoldBonus, Type, ArgID, Desc, Cookie).

gen_cookie_pay_gold(SrcType) ->
    case SrcType of
        ?ACCOUNT_TYPE_QQ ->
            SessionType = "kp_actoken",
            SessionID = "openid";
        ?ACCOUNT_TYPE_WEIXIN ->
            SessionType = "wc_actoken",
            SessionID = "hy_gameid"
    end,
    "session_id=" ++ SessionID ++ ";session_type=" ++ SessionType ++ ";org_loc=" ++ http_uri:encode("/mpay/pay_m").

gen_query_string_pay_gold(OpenID, PayToken, Openkey, PF, PFKey, Gold) ->
    [
     {"amt" , erlang:integer_to_list(Gold)} ,
     {"appid" , ?APPID} ,
     {"openid" , OpenID} ,
     {"openkey" , Openkey} ,
     {"pay_token" , PayToken} ,
     {"pf" , PF} ,
     {"pfkey" , PFKey} ,
     {"ts" , erlang:integer_to_list(util:now())} ,
     {"zoneid" , erlang:integer_to_list(data_setting:get(server_id))}
    ].

http_request_pay_gold(URL, RoleID, Gold, GoldBonus, Type, ArgID, Desc, Cookie) ->
    case httpc:request(get, {URL,[{"Cookie", Cookie}]},[{timeout, 20000}, {connect_timeout, 15000}], []) of
        {ok, {_,_,R}} ->
            {RS} = ejson:decode(R),
            Ret = get_value(RS, <<"ret">>),
            Billno = get_value(RS, <<"billno">>),
            Msg = get_value(RS, <<"msg">>),
            case Ret of
                0 ->
                    behavior_tencent_consume:log(RoleID, erlang:localtime(), Billno, Gold, GoldBonus, Type, ArgID, Desc),
                    true;
                1004 ->
                    {false, gold_not_enough};
                1018 ->
                    notify_client_update_pay_arg(RoleID),
                    ?ERR("pay_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, RoleID:~w", [Ret, Msg, URL, Cookie, RoleID]),
                    {false, check_fail};
                _ ->
                    notify_client_update_pay_arg(RoleID),
                    ?ERR("pay_gold Ret:~w, Msg:~w, URL:~s, Cookie:~s, R:~w, RoleID:~w", [Ret, Msg, URL, Cookie, R, RoleID]),
                    {false, unknow_error}
            end;
        Err ->
            notify_client_update_pay_arg(RoleID),
            ?ERR("pay_gold Err:~w, URL:~s, Cookie:~s, RoleID:~w", [Err, URL, Cookie, RoleID]),
            {false, network_error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_arg(RoleID,SrcType) ->
    case role_lib:get_rolePublic(RoleID) of
        #rolePublic{accid=Accid} ->
            case ets:lookup(?ROLE_PAY_ARG, Accid) of
                [{Accid, OpenID, PayToken, Openkey, PF, PFKey}] ->
                    case SrcType of
                        ?ACCOUNT_TYPE_QQ ->
                            {true, OpenID, PayToken, Openkey, PF, PFKey};
                        ?ACCOUNT_TYPE_WEIXIN ->
                            {true, OpenID, "", Openkey, PF, PFKey}
                    end;
                [] ->
                    false
            end;
        [] ->
            ?ERR("ets look up tencent arg, unkown RoleID:~w", [RoleID]),
            false
    end.

cacl_sig(QueryString, Path) ->
    SourceList = get_original_encode_list(QueryString),
    SourceList2 = "GET" ++ "&" ++
                      http_uri:encode(Path) ++ "&" ++
                      SourceList,
    SignTemp = crypto:hmac('sha', erlang:list_to_binary(?APPKEY), SourceList2),
    binary_to_list(base64:encode(SignTemp)).


get_original_encode_list(QS)->
    QS2 = lists:keysort(1, QS),
    [_H|L] = 
        lists:foldl(fun({A,B},Acc) ->
                            case A of
                                "sig" ->
                                    Acc;
                                _ ->
                                    Acc++"&"++A++"="++ open_encode(B)
                            end
                    end, "", QS2),
    http_uri:encode(L).

open_encode(B) ->
    ExList = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!*().-_",
    lists:foldr(fun(E, Acc)->
                        case lists:member(E, ExList) of
                            true ->
                                [E|Acc];
                            _ ->
                                "%" ++ integer_to_list(E, 16) ++ Acc
                        end
                end, [], B).

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            ?undefined;
        {Key, Value} ->
            Value
    end.

notify_client_update_pay_arg(RoleID) ->
    case role_data:get_roleID() of
        ?undefined ->
            ?unicast(RoleID, #sc_account_update_pay_arg{});
        _ ->
            ?sendself(#sc_account_update_pay_arg{})
    end.

%% 验证应用宝的支付合法性
-spec check_pay_arg(#role{}) -> boolean().
check_pay_arg(#role{roleID = RoleID, srcType = SrcType}) ->
	check_pay_arg(RoleID, SrcType, 0).

check_pay_arg(RoleID, SrcType, NeedGold) ->
    case SrcType =:= ?ACCOUNT_TYPE_QQ orelse SrcType =:= ?ACCOUNT_TYPE_WEIXIN of
        true ->
            case get_gold(RoleID, SrcType) of
                {true, Gold, GoldBonus} ->
                    Gold + GoldBonus >= NeedGold;
                _ ->
                    notify_client_update_pay_arg(RoleID),
                    false
            end;
        false ->
            true
    end.


