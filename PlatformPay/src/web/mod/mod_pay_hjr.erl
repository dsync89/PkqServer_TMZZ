%% @author caijunjun
%% @doc 好接入充值.


-module(mod_pay_hjr).

-include("common.hrl").
-include("record.hrl").

-define(SECURITYKEY, "tfXF66h0Jzvph1Zo6VqQ9f9Ze2YZ5OMS").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

-compile(export_all).

handle(Req) ->
    QueryString = Req:parse_post(),
    case check_sign(QueryString) of
        true ->
            Extend = proplists:get_value("extend", QueryString),
            RoleID2 = erlang:list_to_integer(Extend),
            Amount2 = proplists:get_value("amount", QueryString),
            Amount = trunc(to_num(Amount2) * 10),
            
            ServerID = (RoleID2 div 1000000) - 1,
            Server = data_server_list:get(ServerID),            
            HttpPort = integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryString),
            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payhjr?" ++ SendQS,
            Response = httpc:request(get, {Url, []}, [], []),
            case Response of
                {ok, {_,_,Content}} ->
                    {Content2} = ejson:decode(Content),
                    Result = get_value(Content2, <<"result">>),
                    if Result == 1 ->
                           AccID = get_value(Content2, <<"accid">>),
                           db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_HJR, QueryString),
                           Reply = ejson:encode({[{<<"result">>,<<"0">>},{<<"result_desc">>,<<"充值成功">>}]}),
                           Req:ok({"text/html; charset=utf-8", Reply});
                       true ->
                           db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_HJR, QueryString),
                           Reply = ejson:encode({[{<<"result">>,<<"1">>},{<<"result_desc">>,<<"订单错误">>}]}),
                           Req:ok({"text/html; charset=utf-8", Reply}),
                           ?ERR("hjr check_order failed. reason:game server return false,order:~w, result ~p",[QueryString, Result])
                    end;
                Error ->
                    db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_HJR, QueryString),
                    Reply = ejson:encode({[{<<"result">>,<<"1">>},{<<"result_desc">>,<<"游戏服务器验证错误">>}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("hjr check_order failed. reason:game Server not response not 0,order:~w, ~p",[QueryString, Error])
            end;
        _ ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_HJR, QueryString),
            Reply = ejson:encode({[{<<"result">>,<<"1">>},{<<"result_desc">>,<<"验证失败">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply}), 
            ?ERR("hjr check_order failed. reason:sign wrong,order:~w",[QueryString])
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QS) ->
    Sign = proplists:get_value("sign", QS),
    QueryStr = mochiweb_util:urlencode(lists:keysort(1, lists:keydelete("sign", 1, QS))),
    
    Sign == util:md5(util:md5(QueryStr) ++ ?SECURITYKEY).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

to_num(List) ->
    case catch erlang:list_to_float(List) of
        Num when erlang:is_float(Num) ->
            Num;
        _ ->
            erlang:list_to_integer(List)
    end.


