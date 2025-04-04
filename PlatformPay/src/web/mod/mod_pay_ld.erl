%% @author admin
%% @doc 乐逗 sdk充值处理

-module(mod_pay_ld).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(SECRET,"9c40d6e2250a0276b1ef").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,AmountT,QueryList} ->
            Amount = erlang:trunc(AmountT * 10),
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_ld?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_LD, QueryList),
                            Reply = "ok",
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_LD, QueryList),
                            ?ERR("pay ld, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = "fail",
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_LD, QueryList),
                    ?ERR("pay ld, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = "fail",
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
    ?ERR("debug ld pay, QueryList = ~p~n", [QueryList]),
    OrderId = proplists:get_value("orderId", QueryList),
    OpenId = proplists:get_value("openid", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    ActualAmount = proplists:get_value("actualAmount", QueryList),
    ExtraInfo = proplists:get_value("extraInfo", QueryList),
    Success = proplists:get_value("success", QueryList),
    _Msg = proplists:get_value("msg", QueryList),
    _Created = proplists:get_value("created", QueryList),
    _P_ID = proplists:get_value("p_id", QueryList),
    _P_Identifier = proplists:get_value("p_identifier", QueryList),    
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [OrderId,OpenId,Amount,ActualAmount,Success,ExtraInfo,Sign])) of
        false ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_LD, QueryList),
            ?ERR("ld pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "fail",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            ExtraInfo2 = http_uri:decode(ExtraInfo),
            case md5("orderId=" ++ OrderId ++ "&openid=" ++ OpenId ++ "&amount=" ++ Amount ++
                          "&actualAmount=" ++ ActualAmount ++ "&success=" ++ Success ++
                          "&extraInfo=" ++ ExtraInfo2 ++ "&secret=" ++ ?SECRET) =:= Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_LD, QueryList),
                    ?ERR("ld pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
                    ExtraInfo3 = mochiweb_util:parse_qs(ExtraInfo2),
                    RoleID = proplists:get_value("roleid", ExtraInfo3),
                    ServerID = proplists:get_value("serverid", ExtraInfo3),
                    {true,erlang:list_to_integer(ServerID),erlang:list_to_integer(RoleID),erlang:list_to_integer(Amount),
                     QueryList}
            end
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


