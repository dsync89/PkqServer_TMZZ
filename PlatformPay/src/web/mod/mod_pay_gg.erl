%% @author admin
%% @doc @todo Add description to mod_pay_gg


-module(mod_pay_gg).
-include("common.hrl").
-include("record.hrl").

-define(GAME_ID,"15704").
-define(APP_KEY,"0c7da86a2f095eda21b7d76e6b4f8aa4").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
    QueryString = Req:parse_post(),
    %?ERR("info:~w",[QueryString]),
    case check_sign(QueryString) of
        {true,ServerID,RoleID,AmountT} ->
            Amount = erlang:trunc(AmountT / 10),
            Server = data_server_list:get(ServerID),        
            HttpPort = integer_to_list(Server#server.serverHttpPort),
            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paygg",
            Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []), 
            case Response of
                {ok, {_,_,Content}} ->
                    {Content2} = ejson:decode(Content),
                    Result2 = get_value(Content2, <<"result">>),
                    if Result2 == 1 ->
                           AccID = get_value(Content2, <<"accid">>),
                           db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_GG, QueryString),
                           Reply = "success",
                           Req:ok({"text/html; charset=utf-8", Reply});
                       true ->
                           db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_GG, QueryString),
                           Reply = "error",
                           Req:ok({"text/html; charset=utf-8", Reply}),
                           ?ERR("gg check_order failed. reason:game server return false,order:~w",[QueryString])
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_GG, QueryString),
                    Reply = "error",
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("gg check_order failed. reason:game server no response,order:~w",[QueryString])
            end;
        {false, Reply} ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_GG, QueryString),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("gg check_order failed. QueryString:~w",[QueryString])
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QueryList) ->
    OpenIdT = proplists:get_value("openId", QueryList),
    ServerIdT = proplists:get_value("serverId", QueryList),
    ServerNameT = proplists:get_value("serverName", QueryList),
    RoleIdT = proplists:get_value("roleId", QueryList),
    RoleNameT = proplists:get_value("roleName", QueryList),
    OrderIdT = proplists:get_value("orderId", QueryList),
    OrderStatusT = proplists:get_value("orderStatus", QueryList),
    PayTypeT = proplists:get_value("payType", QueryList),
    AmountT = proplists:get_value("amount", QueryList), %%单位为分，float
    RemarkT = proplists:get_value("remark", QueryList),
    CallBackInfoT = proplists:get_value("callBackInfo", QueryList),
    PayTimeT = proplists:get_value("payTime", QueryList),
    PaySUTimeT = proplists:get_value("paySUTime", QueryList),
    SignT = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [OpenIdT,ServerIdT,RoleIdT,OrderIdT,OrderStatusT,AmountT,SignT])) of
        false ->
            ?ERR("gg pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "error",
            {false, Reply};
        true ->
            ArgList =
                [OpenId, ServerId, ServerName, RoleId, RoleName, OrderId, OrderStatus,
                 PayType, Amount, Remark, CallBackInfo, PayTime, PaySUTime] =
                    lists:map(fun(String) ->
                                      case String of
                                          ?undefined ->
                                              String;
                                          _ ->
                                              http_uri:decode(String)
                                      end
                              end, [OpenIdT, ServerIdT, ServerNameT, RoleIdT, RoleNameT, OrderIdT, OrderStatusT,
                                    PayTypeT, AmountT, RemarkT, CallBackInfoT, PayTimeT, PaySUTimeT]),
            ArgNameList = ["openId", "serverId", "serverName", "roleId", "roleName", "orderId", "orderStatus",
                           "payType", "amount", "remark", "callBackInfo", "payTime", "paySUTime"],
            case OrderStatus of
                "1" ->
                    case md5(gen_sign(ArgNameList, ArgList, [])) =:= http_uri:decode(SignT) of
                        false ->
                            ?ERR("gg pay, check sign error, QueryList = ~p~n", [QueryList]),
                            Reply = "errorSign",
                            {false, Reply};
                        true ->
                            {true,erlang:list_to_integer(ServerId),erlang:list_to_integer(RoleId),
                             erlang:list_to_integer(Amount)}
                    end;
                _ ->
                    ?ERR("gg pay, order status not succ, QueryList = ~p~n", [QueryList]),
                    Reply = "success",
                    {false, Reply}
            end
    end.

gen_sign([], [], List) ->
    [_|List2] = List ++ "&app_key=" ++ ?APP_KEY,
    List2;
gen_sign([ArgName|ArgNameList], [Arg|ArgList], List) ->
    case Arg of
        ?undefined ->
            NewList = List ++ "&" ++ ArgName ++ "=",
            gen_sign(ArgNameList, ArgList, NewList) ;
        _ ->
            NewList = List ++ "&" ++ ArgName ++ "=" ++ Arg,
            gen_sign(ArgNameList, ArgList, NewList)   
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.




