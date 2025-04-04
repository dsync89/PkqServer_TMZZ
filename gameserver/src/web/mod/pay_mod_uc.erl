%% @author admin
%% @doc 异步用的充值接受模块


-module(pay_mod_uc).
-include("common.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryStringT = Req:parse_post(),
	[{QS,[]}]=QueryStringT,
	QueryString = str_to_term(QS),
	?DEBUG("Req=~w ###### \nQS= ~p\n ### QST~w",[Req,QueryString,QueryStringT]),
	%% 提取消息中的信息
	Data = proplists:get_value("data", QueryString),
	Sign = proplists:get_value("sign", QueryString),
	OrderId = proplists:get_value("orderId", Data),
	GameId = proplists:get_value("gameId", Data),
	PayWay = proplists:get_value("payWay", Data),
	Amount = proplists:get_value("amount", Data),
	CallbackInfo = proplists:get_value("callbackInfo", Data),
    UCServerId = proplists:get_value("serverId", Data),
    UCid = proplists:get_value("ucid", Data),
    CpOrderId = proplists:get_value("cpOrderId", Data),
	[_ServerID,RoleID] = string:tokens(CallbackInfo,"."),
	RoleID2 = list_to_integer(RoleID),
	
	OrderStatus = proplists:get_value("orderStatus", Data),
	FailedDesc = proplists:get_value("failedDesc", Data),
	
	%%本地数据
	ApiKey = "be48f1f254ad9373ebe808c81bf366a3",
	GameIdLocal = "550934",
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
					RoleAccID = db_sql:get_role_accid(RoleID2),
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					if OrderStatus =:= "S" ->
							Amount2 = list_to_float(Amount),
							pay_gold2(RoleID2,trunc(Amount2*10),QS,Sign,3);
					   true->
							 ?ERR("OrderStatus err:",[Data])  
					end;
				true ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		false ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_uc(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================

get_value(Response, Key) when is_list(Key)->
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



%% ================
%%     test
%% ================

test()->
	QS= [{[123,34,115,105,103,110,34,58,34,101,97,97,102,49,50,101,98,53,101,97,97,98,98,99,54,52,97,52,48,99,51,99,101,54,100,97,99,51,56,102,57,34,44,34,100,97,116,97,34,58,123,34,102,97,105,108,101,100,68,101,115,99,34,58,34,34,44,34,97,109,111,117,110,116,34,58,34,54,46,48,48,34,44,34,99,97,108,108,98,97,99,107,73,110,102,111,34,58,34,34,44,34,117,99,105,100,34,58,34,50,48,48,56,48,50,56,53,49,34,44,34,103,97,109,101,73,100,34,58,34,53,50,51,55,50,53,34,44,34,112,97,121,87,97,121,34,58,34,49,48,49,34,44,34,115,101,114,118,101,114,73,100,34,58,34,50,50,51,57,34,44,34,111,114,100,101,114,83,116,97,116,117,115,34,58,34,83,34,44,34,111,114,100,101,114,73,100,34,58,34,50,48,49,51,48,56,50,54,49,49,51,54,49,51,55,56,48,48,52,55,57,34,125,125],[]}],
	[{M2,[]}] = QS,
	Pq = str_to_term(M2),
	Pp = proplists:get_value("data", Pq),
	io:format("~p\n~p",[Pq,Pp]).

	