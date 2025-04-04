%% @author caijunjun
%% @doc YY充值 服务器验证模块.


-module(pay_mod_yy).

-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-define(APPKEY, "4a22c6c60e64e33e1a3ae9a69cb2601d").

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString) of
        true ->
            RoleID = proplists:get_value("cparam", QueryString),
            RoleID2 = erlang:list_to_integer(RoleID),
            RoleAccID = db_sql:get_role_accid(RoleID2),
            Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            
            Amount = get_amount(QueryString),
            QS = mochiweb_util:urlencode(QueryString),
            Sign = proplists:get_value("sign", QueryString),
            pay_gold2(RoleID2, Amount, QS, Sign, ?ACCOUNT_TYPE_YY);
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_yy(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].


verify_sign(QS) ->
    Sign = proplists:get_value("sign", QS),
    Account = proplists:get_value("account", QS),   
    OrderID = proplists:get_value("orderid", QS),
    RMB = proplists:get_value("rmb", QS),
    Num = proplists:get_value("num", QS),
    Type = proplists:get_value("type", QS),
    Time = proplists:get_value("time", QS),
    Game = proplists:get_value("game", QS),
    Server = proplists:get_value("server", QS),
    Role = proplists:get_value("role", QS),
    ItemID = proplists:get_value("itemid", QS),
    Price = proplists:get_value("price", QS),
    CParam = proplists:get_value("cparam", QS),
    DataList = Account ++ OrderID ++ RMB ++ Num ++ Type ++ Time 
                   ++ Game ++ Server ++ Role ++ ItemID ++ Price ++ CParam,
    util:md5(DataList ++ ?APPKEY) =:= Sign.

get_amount(QS) ->
    RMB = proplists:get_value("rmb", QS),
    erlang:trunc(to_num(RMB) * 10).

to_num(List) ->
    case catch erlang:list_to_integer(List) of
        Int when erlang:is_integer(Int) ->
            Int;
        _Err ->
            erlang:list_to_float(List)
    end.



