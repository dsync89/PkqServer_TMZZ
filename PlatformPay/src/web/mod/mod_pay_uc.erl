%% @author admin
%% @doc @todo Add description to mod_pay_uc.


-module(mod_pay_uc).

-include("common.hrl").
-include("record.hrl").

-define(srcType,3).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

handle(Req) ->
	QueryStringT = Req:parse_post(),
	[{QS,[]}]=QueryStringT,
	QueryString = str_to_term(QS),
	%% ?ERR("Req=~w ###### \nQS= ~p\n ### QST~w",[Req,QueryString,QueryStringT]),
	%% 提取消息中的信息
	Data = proplists:get_value("data", QueryString),
	Sign = proplists:get_value("sign", QueryString),
	OrderId = proplists:get_value("orderId", Data),
	GameId = proplists:get_value("gameId", Data),
	PayWay = proplists:get_value("payWay", Data),
	Amount = proplists:get_value("amount", Data),
    UCServerId = proplists:get_value("serverId", Data),
    UCid = proplists:get_value("ucid", Data),
    CpOrderId = proplists:get_value("cpOrderId", Data),
	CallbackInfo = proplists:get_value("callbackInfo", Data),
	[_,RoleID] = string:tokens(CallbackInfo,"."),
	RoleID2 = list_to_integer(RoleID),
	ServerID2 = (RoleID2 div 1000000) - 1,
	Server = data_server_list:get(ServerID2),

	OrderStatus = proplists:get_value("orderStatus", Data),
	FailedDesc = proplists:get_value("failedDesc", Data),
	
	%%本地数据
	ApiKey = "be48f1f254ad9373ebe808c81bf366a3",
	GameIdLocal = "550934",
	Amount2 = trunc(list_to_float(Amount)*10),
    case GameIdLocal =:= GameId of
        true ->
            case CpOrderId of
                ?undefined ->
                    Sign2 = sign(["22703","amount=",Amount,"callbackInfo=",CallbackInfo,
                                  "failedDesc=",FailedDesc,"gameId=",GameId,
                                  "orderId=",OrderId,"orderStatus=",OrderStatus,"payWay=",PayWay,
                                  "serverId=", UCServerId, "ucid=",UCid, ApiKey]);
                _ ->
                    Sign2 = sign(["22703","amount=",Amount,"callbackInfo=",CallbackInfo,
                                  "cpOrderId=", CpOrderId, "failedDesc=",FailedDesc,"gameId=",GameId,
                                  "orderId=",OrderId,"orderStatus=",OrderStatus,"payWay=",PayWay,
                                  "serverId=", UCServerId, "ucid=",UCid, ApiKey])
            end,
            if Sign2 =:= Sign ->
					if OrderStatus =:= "S" ->	
							HttpPort = integer_to_list(Server#server.serverHttpPort),
							Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payuc/",
							%% ?ERR("Url:~w",[Url]),
							Response = httpc:request(post, {Url,[], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),
							case Response of
								{ok, {_,_,Content3}} ->
									{Content4} = ejson:decode(Content3),
									Result = get_value(Content4, <<"result">>),
									if Result == 1 ->
										   AccID = get_value(Content4, <<"accid">>),
										   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
										   Reply = "SUCCESS",
										   Req:ok({"text/html; charset=utf-8", Reply});
									   true ->
										   db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
											?ERR("uc order fail3:~w",[QueryString]),
										   	Reply = "SUCCESS",
											Req:ok({"text/html; charset=utf-8", Reply})
									end;
								Err ->
									db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
									?ERR("uc order fail2:~w\nErr:~w",[QueryString,Err]),
									Reply = "SUCCESS",
									Req:ok({"text/html; charset=utf-8", Reply})
							end;
					   true->
						   db_func:log_pay_info(5, RoleID2, 0, Amount2, erlang:localtime(), 0, ?srcType, QueryString),
							 ?ERR("uc order fail5:~w",[QueryString]),
							 Reply = "SUCCESS",
							 Req:ok({"text/html; charset=utf-8", Reply})
					end;
				true ->
					db_func:log_pay_info(2, RoleID2, 0, Amount2, erlang:localtime(), 0, ?srcType, QueryString),
					?ERR("uc order fail4:~w",[QueryString]),
					Reply = "FAILURE",
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		false ->
			db_func:log_pay_info(2, RoleID2, 0, Amount2, erlang:localtime(), 0, ?srcType, QueryString),
			?ERR("uc order fail:~w",[QueryString]),
			Reply = "FAILURE",
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

str_to_term(Str)->
	M = lists:foldl(fun(E,Acc)->
							case E of
								$,->
									[${,$,,$}|Acc];
								$:->
									[$,|Acc];
								${->
									[${,$[|Acc];
								$}->
									[$],$}|Acc];
								P ->
									[P|Acc]
							end end,[], Str),
	M1 = lists:flatten(lists:reverse(["."|M])),
	{ok,Tokens,_} = erl_scan:string(M1),
	{ok,M3} = erl_parse:parse_term(Tokens),
	M3.