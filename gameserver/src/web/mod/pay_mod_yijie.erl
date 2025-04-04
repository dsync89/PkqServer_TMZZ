%% @author admin
%% @doc 易接充值处理

-module(pay_mod_yijie).
-export([pay_gold/1]).
-include("common.hrl").

%%-define(APPKEY, "9PMY6NYEOTP292V50T61G4KNXLYYR8TB").
-define(APPKEY, "HJDIS29ZQ67WCY8RNAZFFLVC3QNZCAV8").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {_,_,0,_,_,_} ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,_,RoleID,Amount,QueryList,Sign} ->
            QS = mochiweb_util:urlencode(QueryList),
            pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_YIJIE),
            AccID = db_sql:get_role_accid(RoleID),
            Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_yijie(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
    ?DEBUG("in gameserver[pay_mod_yijie], debug yijie pay, QueryList = ~p~n", [QueryList]),
    App = proplists:get_value("app", QueryList),
    Cbi = proplists:get_value("cbi", QueryList),
    Ct = proplists:get_value("ct", QueryList),
    Fee = proplists:get_value("fee", QueryList),
    Pt = proplists:get_value("pt", QueryList),
    Sdk = proplists:get_value("sdk", QueryList),
    Ssid = proplists:get_value("ssid", QueryList),
    St = proplists:get_value("st", QueryList),
    Tcd = proplists:get_value("tcd", QueryList),
    Uid = proplists:get_value("uid", QueryList),
    Ver = proplists:get_value("ver", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [App,Cbi,Ct,Fee,Pt,Sdk,Ssid,St,Tcd,Uid,Ver,Sign])) of
        false ->
            ?ERR("yijie pay, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            case sign(App,Cbi,Ct,Fee,Pt,Sdk,Ssid,St,Tcd,Uid,Ver) =:= Sign andalso St =:= "1" of
                false ->
                    ?ERR("yijie pay, check sign error, QueryList = ~p~n", [QueryList]), false;
                true ->
                    [SServerID,SRoleID] = string:tokens(Cbi, "|"),
                    ServerID = erlang:list_to_integer(SServerID),
                    RoleID = erlang:list_to_integer(SRoleID),
                    Amount2 = erlang:round(erlang:list_to_integer(Fee) * 2),
                    {true,ServerID,RoleID,Amount2,QueryList,Sign}
            end
    end.

sign(App,Cbi,Ct,Fee,Pt,Sdk,Ssid,St,Tcd,Uid,Ver) ->
    Appkey = data_sdk_id:get(App),
    OriStr = "app="++App++"&cbi="++Cbi++"&ct="++Ct++"&fee="++Fee++"&pt="++Pt++"&sdk="++Sdk
    ++"&ssid="++Ssid++"&st="++St++"&tcd="++Tcd++"&uid="++Uid++"&ver="++Ver++Appkey,
    md5(OriStr).


md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

