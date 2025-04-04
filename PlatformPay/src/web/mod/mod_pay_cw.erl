%% @author admin
%% @doc 益玩 sdk充值处理

-module(mod_pay_cw).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(APP_ID,"10117").
-define(PACKET_ID,"25331").
-define(APP_KEY,"wAU0pCdyF2IcMYvs").
-define(SIGN_KEY,"rXTt9bJWlzN3inR6").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,AmountT,QueryList} ->
            Amount = erlang:trunc(AmountT / 10),
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_cw?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_CW, QueryList),
                            Reply = "1",
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_CW, QueryList),
                            ?ERR("pay cw, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = "1",
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_CW, QueryList),
                    ?ERR("pay cw, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = "1",
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ ->
            ignore
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
    ServerID = proplists:get_value("serverid", QueryList),
    CustomInfo = proplists:get_value("custominfo", QueryList),
    UserID = proplists:get_value("openid", QueryList),
    OrderID = proplists:get_value("ordernum", QueryList),
    Status = proplists:get_value("status", QueryList),
    PayType = proplists:get_value("paytype", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    ErrDesc = proplists:get_value("errdesc", QueryList),
    PayTime = proplists:get_value("paytime", QueryList),    
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,PayTime,Sign])) of
        false ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_CW, QueryList),
            ?ERR("cw pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "101",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            case sign(ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,ErrDesc,PayTime) =:= Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_CW, QueryList),
                    ?ERR("cw pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "100",
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
                    {CustomInfo2} = ejson:decode(CustomInfo),
                    RoleID = get_value(CustomInfo2, <<"roleid">>),
                    {true,erlang:list_to_integer(ServerID),RoleID,erlang:list_to_integer(Amount),
                     QueryList}
            end
    end.

sign(ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,?undefined,PayTime) ->
    md5(ServerID ++ "|" ++ CustomInfo ++ "|" ++ UserID ++ "|" ++ OrderID ++ "|" ++ Status ++ "|" ++ PayType ++ "|" ++ Amount ++ "|" ++ PayTime ++ "|" ++ ?APP_KEY);

sign(ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,ErrDesc,PayTime) ->
    md5(ServerID ++ "|" ++ CustomInfo ++ "|" ++ UserID ++ "|" ++ OrderID ++ "|" ++ Status ++ "|" ++ PayType ++ "|" ++ Amount ++ "|" ++ ErrDesc ++ "|" ++ PayTime ++ "|" ++ ?APP_KEY).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.
