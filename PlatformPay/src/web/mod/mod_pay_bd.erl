

-module(mod_pay_bd).
-include("common.hrl").
-include("record.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
    QueryString = Req:parse_post(),
    AppID = proplists:get_value("AppID",QueryString),
    Sign = proplists:get_value("Sign",QueryString),
    OrderSerial = proplists:get_value("OrderSerial",QueryString),
    CooperatorOrderSerial = proplists:get_value("CooperatorOrderSerial",QueryString),
    Content = proplists:get_value("Content",QueryString),
    case AppID =:= "5009295" of
        true ->
            LocalSign = sign([AppID,OrderSerial,CooperatorOrderSerial,Content,"ZuEK6k8H6fPnG3E6GO6rDa0wBXjVM2FI"]),
            {Content2} = ejson:decode(base64:decode(Content)),
            Amount = trunc(erlang:list_to_float(erlang:binary_to_list(get_value(Content2, <<"OrderMoney">>))) * 10),
            OrderStatus = get_value(Content2, <<"OrderStatus">>),
            ExtInfo = get_value(Content2, <<"ExtInfo">>),
            RoleID = erlang:list_to_integer(erlang:binary_to_list(ExtInfo)),
            if
                Sign == LocalSign ->
                    if
                        OrderStatus =:= 1 ->
                            ServerID2 = (RoleID div 1000000) - 1,
                            Server = data_server_list:get(ServerID2),
                            HttpPort = integer_to_list(Server#server.serverHttpPort),
                            SendQS = mochiweb_util:urlencode(QueryString),
                            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paybd?" ++ SendQS,
                            Response = httpc:request(get, {Url, []}, [], []),   
                            case Response of
                                {ok, {_,_,RContent}} ->
                                    {RContent2} = ejson:decode(RContent),
                                    Result = get_value(RContent2, <<"result">>),
                                    if Result == 1 ->
                                           AccID = get_value(RContent2, <<"accid">>),
                                           db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_BAIDU, QueryString),
                                           Reply = ejson:encode({[{<<"AppID">>,5009295},{<<"ResultCode">>,1},{<<"ResultMsg">>,<<"succ">>},{<<"Sign">>,req_sign(1)},{<<"Content">>, <<"">>}]}),
                                           Req:ok({"text/html; charset=utf-8", Reply});
                                       true ->
                                           ?ERR("91 pay failed order:~w,Reason:GameServer return",[QueryString]),
                                           db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_BAIDU, QueryString),
                                           Reply = ejson:encode({[{<<"AppID">>,5009295},{<<"ResultCode">>,0},{<<"ResultMsg">>,<<"fail">>},{<<"Sign">>,req_sign(0)},{<<"Content">>, <<"">>}]}),
                                           Req:ok({"text/html; charset=utf-8", Reply})
                                    end;
                                _ ->
                                    ?ERR("91 pay failed order:~w,Reason:GameServer no response",[QueryString]),
                                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_BAIDU, QueryString),
                                    Reply = ejson:encode({[{<<"AppID">>,5009295},{<<"ResultCode">>,0},{<<"ResultMsg">>,<<"fail">>},{<<"Sign">>,req_sign(0)},{<<"Content">>, <<"">>}]}),
                                    Req:ok({"text/html; charset=utf-8", Reply})
                            end;
                        true ->
                            ?ERR("91 pay failed order:~w,Reason:pay status",[QueryString]),
                            db_func:log_pay_info(5, RoleID, 0, Amount, erlang:localtime(), 0, ?ACCOUNT_TYPE_BAIDU, QueryString),
                            Reply = ejson:encode({[{<<"AppID">>,5009295},{<<"ResultCode">>,0},{<<"ResultMsg">>,<<"fail">>},{<<"Sign">>,req_sign(0)},{<<"Content">>, <<"">>}]}),
                            Req:ok({"text/html; charset=utf-8", Reply})
                    end;
                true ->
                    ?ERR("91 pay failed order:~w,Reason:sign wrong",[QueryString]),
                    db_func:log_pay_info(2, RoleID, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_BAIDU, QueryString),
                    Reply = ejson:encode({[{<<"AppID">>,5009295},{<<"ResultCode">>,0},{<<"ResultMsg">>,<<"fail">>},{<<"Sign">>,req_sign(0)},{<<"Content">>, <<"">>}]}),
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;
        _ ->
            ?ERR("91 pay failed order:~w,reason:appid wrong",[QueryString]),
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_BAIDU, QueryString),
            Reply = ejson:encode({[{<<"AppID">>,5009295},{<<"ResultCode">>,0},{<<"ResultMsg">>,<<"fail">>},{<<"Sign">>,req_sign(0)},{<<"Content">>, <<"">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.




%% ====================================================================
%% Internal functions
%% ====================================================================
get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

sign(StrList) ->
    md5(sign2(StrList)).

sign2([A]) ->
    A;
sign2([A,B]) ->
    A++B;
sign2([]) ->
    "";
sign2([A|T]) ->
    A++sign2(T).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

req_sign(ResultCode) ->
	erlang:list_to_binary(md5("5009295" ++ erlang:integer_to_list(ResultCode) ++ "ZuEK6k8H6fPnG3E6GO6rDa0wBXjVM2FI")).

