%% @author liuqiang
%% @doc PPS sdk充值处理

-module(mod_pay_pps).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(APPID, "2744").
-define(APPKEY, "74974bf301ff7e270d0e1e6860735f38").
-define(PAYKEY, "KDYG42c36777108aeHc387b102cfHm2744").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,Money,QueryList,_} ->
            Amount = Money * 10,
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_pps?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_PPS, QueryList),
                            Reply = pack_reply(0, "success"),
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_PPS, QueryList),
                            ?ERR("pay pps, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = pack_reply(-6, "internal error"),
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_PPS, QueryList),
                    ?ERR("pay pps, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = pack_reply(-6, "internal error"),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ -> void
    end.

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
            ?ERR("pps pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = pack_reply(-2, "parameters error"),
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            case sign(UserID, TmpRoleID, OrderID, Money, Time) =:= Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_PPS, QueryList),
                    ?ERR("pps pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = pack_reply(-1, "sign error"),
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
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

pack_reply(Result, Message) ->
    ejson:encode({[
                   {<<"result">>,Result}
                  ,{<<"desc">>,erlang:list_to_binary(Message)}
                  ]}).

%% -------------------------------------- test code ---------------------------------------------
