%% @author admin
%% @doc @todo Add description to pay_mod_xy


-module(pay_mod_xy).
-include("common.hrl").


-define(APPID, "100005057").
-define(APPKEY, "YPfgti0EXXDsg2OhoOjA4RnBKX9ZUjzM").
-define(PAYKEY, "zeMZ2nYqxdlZx5OpNAdK5Eq8RhSBVAp3").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
    QueryString = Req:parse_post(),
    case check_sign(QueryString) of
        {true,_ServerID,RoleID,AmountT,Sign} ->
            Amount = erlang:trunc(AmountT * 10),
            AccID = db_sql:get_role_accid(RoleID),
            Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,AccID}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            QS = mochiweb_util:urlencode(QueryString),
            pay_gold2(RoleID,Amount,QS,Sign,?ACCOUNT_TYPE_XY);
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("xy check_order failed. reason:sign wrong,order:~w",[QueryString])
    end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_xy(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].



%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QueryList) ->
    OrderID = proplists:get_value("orderid", QueryList),
    UID = proplists:get_value("uid", QueryList),
    ServerID = proplists:get_value("serverid", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    Extra = proplists:get_value("extra", QueryList),
    Timestamp = proplists:get_value("ts", QueryList),
    Sign = proplists:get_value("sign", QueryList),
	Sig = proplists:get_value("sig", QueryList),
    case (not lists:member(?undefined, [OrderID,UID,ServerID,Amount,Extra,Timestamp,Sign])) of
        false ->
            ?ERR("xy pay, query list error, QueryList = ~p~n", [QueryList]),
            false;
        true ->
            case sign(?APPKEY, Amount, Extra, OrderID, ServerID, Timestamp, UID) =:= Sign
				andalso sign(?PAYKEY, Amount, Extra, OrderID, ServerID, Timestamp, UID) =:= Sig of
                false ->
                    ?ERR("xy pay, check sign error, QueryList = ~p~n", [QueryList]),
                    false;
                true ->
                    RoleID = Extra,
                    {true,erlang:list_to_integer(ServerID),erlang:list_to_integer(RoleID),
                     erlang:list_to_float(Amount),Sign}
            end
    end.

sign(Key, Amount, Extra, OrderID, ServerID, Timestamp, UID) ->
    md5(Key ++ "amount=" ++ Amount ++ "&extra=" ++ Extra ++ "&orderid=" ++ OrderID ++ "&serverid=" ++ ServerID
       ++ "&ts=" ++ Timestamp ++ "&uid=" ++ UID).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.