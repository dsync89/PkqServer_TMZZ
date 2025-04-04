%% @author liuqiang
%% @doc 奇天乐地IOS 充值处理

-module(mod_pay_qtld_ios).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(GID, "2036").
-define(APPKEY, "af31c25c").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,Amount,QueryList,_} ->
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_qtld_ios?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_QTLD_IOS, QueryList),
                            Reply = "1",
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_QTLD_IOS, QueryList),
                            ?ERR("pay qtld_ios, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = "0",
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_QTLD_IOS, QueryList),
                    ?ERR("pay qtld_ios, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = "0",
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ -> void
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_post(),
%%     ?ERR("debug qtld_ios pay, QueryList = ~p~n", [QueryList]),
    Amount = proplists:get_value("amount", QueryList),
    Cash = proplists:get_value("cash", QueryList),
    Exchange = proplists:get_value("exchange", QueryList),
    Extra = proplists:get_value("extra", QueryList),
    Money = proplists:get_value("money", QueryList),
    OrderID = proplists:get_value("orderid", QueryList),
    PayTime = proplists:get_value("pay_time", QueryList),
    Sid = proplists:get_value("sid", QueryList),
    Uid = proplists:get_value("uid", QueryList),
    WayName = proplists:get_value("way_name", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [Amount,Cash,Exchange,Extra,Money,OrderID,PayTime,Sid,Uid,WayName,Sign])) of
        false ->
            ?ERR("qtld_ios pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "0",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            case sign(Amount,Cash,Exchange,Extra,Money,OrderID,PayTime,Sid,Uid,WayName) =:= Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_QTLD_IOS, QueryList),
                    ?ERR("qtld_ios pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "0",
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
                    [SServerID,SRoleID] = string:tokens(Extra, "{},"),
                    ServerID = erlang:list_to_integer(SServerID),
                    RoleID = erlang:list_to_integer(SRoleID),
                    Amount2 = erlang:round(erlang:list_to_float(Money) * 10),
                    {true,ServerID,RoleID,Amount2,QueryList,Sign}
            end
    end.

sign(Amount,Cash,Exchange,Extra,Money,OrderID,PayTime,Sid,Uid,WayName) ->
    OriStr = "amount="++Amount++"cash="++Cash++"exchange="++Exchange++"extra="++Extra++"money="++Money
            ++"orderid="++OrderID++"pay_time="++PayTime++"sid="++Sid++"uid="++Uid++"way_name="++WayName++?APPKEY,
    md5(OriStr).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% -------------------------------------- test code ---------------------------------------------
