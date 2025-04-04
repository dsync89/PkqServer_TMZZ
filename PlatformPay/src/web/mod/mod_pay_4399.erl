%% @author admin
%% @doc 4399 sdk充值处理

-module(mod_pay_4399).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(SECRET, "pAaZfynHgjE0mXtKMAIYGi6jUHKS5S1B").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,Money,QueryList,MoneyT,GameMoneyT} ->
            Amount = erlang:trunc(Money * 10),
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_4399?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_4399, QueryList),
                            Reply = "100" ++ "|" ++ MoneyT ++ "|" ++ GameMoneyT,
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_4399, QueryList),
                            ?ERR("pay 4399, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = "other_error",
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_4399, QueryList),
                    ?ERR("pay 4399, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = "other_error",
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ ->
            void
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
%%     ?ERR("debug 4399 pay, QueryList = ~p~n", [QueryList]),
    UserID = proplists:get_value("uid", QueryList),
    OrderID = proplists:get_value("orderid", QueryList),
    Money = proplists:get_value("money", QueryList),
    GameMoney = proplists:get_value("gamemoney", QueryList),
    Time = proplists:get_value("time", QueryList),
    SID = proplists:get_value("serverid", QueryList),
    Mark = proplists:get_value("mark", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [UserID,OrderID,Money,GameMoney,Time,Mark,Sign])) of
        false ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_4399, QueryList),
            ?ERR("4399 pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "other_error",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            Mask2 = lists:sublist(Mark, 11, 22),
            ServerID = lists:sublist(Mask2, 4),
            RoleID = lists:sublist(Mask2, 5, 18),
            case sign(OrderID,UserID,Money,GameMoney,SID,Mark,Time) =:= Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_4399, QueryList),
                    ?ERR("4399 pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "sign_error",
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
                    {true,erlang:list_to_integer(ServerID),erlang:list_to_integer(RoleID),erlang:list_to_integer(Money),
                     QueryList,Money,GameMoney}
            end
    end.


sign(OrderID,UserID,Money,GameMoney,?undefined,Mark,Time) ->
    md5(OrderID ++ UserID ++ Money ++ GameMoney ++ ?SECRET ++ Mark ++ Time);
sign(OrderID,UserID,Money,GameMoney,SID,Mark,Time) ->
    md5(OrderID ++ UserID ++ Money ++ GameMoney ++ SID ++ ?SECRET ++ Mark ++ Time).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).



%% -------------------------------------- test code ---------------------------------------------
