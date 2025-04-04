%% @author liuqiang
%% @doc PPS sdk充值处理

-module(pay_mod_pps).
-export([pay_gold/1]).
-include("common.hrl").

-define(APPID, "2744").
-define(APPKEY, "74974bf301ff7e270d0e1e6860735f38").
-define(PAYKEY, "KDYG42c36777108aeHc387b102cfHm2744").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,_,RoleID,Money,QueryList,MySign} ->
            Amount = Money * 10,
            QS = mochiweb_util:urlencode(QueryList),
            pay_gold2(RoleID, Amount, QS, MySign, ?ACCOUNT_TYPE_PPS),
            AccID = db_sql:get_role_accid(RoleID),
            Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_pps(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
%%     ?ERR("debug pps pay, QueryList = ~p~n", [QueryList]),
    UserID = proplists:get_value("user_id", QueryList),
    TmpRoleID = proplists:get_value("role_id", QueryList),
    OrderID = proplists:get_value("order_id", QueryList),
    Money = proplists:get_value("money", QueryList),
    Time = proplists:get_value("time", QueryList),
    UserData = proplists:get_value("userData", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [UserID,TmpRoleID,OrderID,Money,Time,UserData,Sign])) of
        false ->
            ?ERR("pps pay, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            case sign(UserID, TmpRoleID, OrderID, Money, Time) =:= Sign of
                false ->
                    ?ERR("pps pay, check sign error, QueryList = ~p~n", [QueryList]), false;
                true ->
                    [ServerID, RoleID, MyTime] = string:tokens(UserData, "."),
                    MySign = sign(UserID, TmpRoleID, OrderID, Money, MyTime),
                    {true,list_to_integer(ServerID),list_to_integer(RoleID),list_to_integer(Money),QueryList,MySign}
            end
    end.

sign(UserID, TmpRoleID, OrderID, Money, Time) ->
    md5(UserID++TmpRoleID++OrderID++Money++Time++?PAYKEY).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% -------------------------------------- test code ---------------------------------------------
