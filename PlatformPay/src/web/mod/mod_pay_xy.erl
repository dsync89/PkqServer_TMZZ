%% @author admin
%% @doc @todo Add description to mod_pay_xy


-module(mod_pay_xy).
-include("common.hrl").
-include("record.hrl").

-define(APPID, "100005057").
-define(APPKEY, "YPfgti0EXXDsg2OhoOjA4RnBKX9ZUjzM").
-define(PAYKEY, "zeMZ2nYqxdlZx5OpNAdK5Eq8RhSBVAp3").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
    QueryString = Req:parse_post(),
%%  ?ERR("info:~w",[QueryString]),
    case check_sign(QueryString) of
        {true,ServerID,RoleID,AmountT,_Sign} ->
            Amount = erlang:trunc(AmountT * 10),
            Server = data_server_list:get(ServerID),        
            HttpPort = integer_to_list(Server#server.serverHttpPort),
            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payxy",
            Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []), 
            
            case Response of
                {ok, {_,_,Content}} ->
                    {Content2} = ejson:decode(Content),
                    Result2 = get_value(Content2, <<"result">>),
                    if Result2 == 1 ->
                           AccID = get_value(Content2, <<"accid">>),
                           db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_XY, QueryString),
                           Reply = pack_reply(0, "pay succ"),
                           Req:ok({"text/html; charset=utf-8", Reply});
                       true ->
                           db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_XY, QueryString),
                           Reply = pack_reply(8, "other error"),
                           Req:ok({"text/html; charset=utf-8", Reply}),
                           ?ERR("xy check_order failed. reason:game server return false,order:~w",[QueryString])
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_XY, QueryString),
                    Reply = pack_reply(3, "game server not exist"),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("xy check_order failed. reason:game server no response,order:~w",[QueryString])
            end;
        {false, Reply} ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_XY, QueryString),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("xy check_order failed. reason:sign wrong,QueryString:~w",[QueryString])
    end.


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
            Reply = pack_reply(1, "parameters error"),
            {false, Reply};
        true ->
            case sign(?APPKEY, Amount, Extra, OrderID, ServerID, Timestamp, UID) =:= Sign
				andalso sign(?PAYKEY, Amount, Extra, OrderID, ServerID, Timestamp, UID) =:= Sig of
                false ->
                    ?ERR("xy pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = pack_reply(6, "sign error"),
                    {false, Reply};
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

pack_reply(Result, Message) ->
    ejson:encode({[
                   {<<"ret">>,Result}
                  ,{<<"msg">>,erlang:list_to_binary(Message)}
                  ]}).





