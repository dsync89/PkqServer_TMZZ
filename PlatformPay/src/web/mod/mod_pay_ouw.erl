%% @author admin
%% @doc @todo Add description to pay_mod_ouw


-module(mod_pay_ouw).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,30).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_qs(),
	case verify_sign(QueryString) of
		true ->
			Result = proplists:get_value("orderStatus", QueryString),
			case Result of
				"1" ->
					GameRole = proplists:get_value("callbackInfo", QueryString),
					[_ServerIDT,RoleID|_] = string:tokens(GameRole, "_"),
					RoleID2 = erlang:list_to_integer(RoleID),
					ServerID = (RoleID2 div 1000000) - 1,			
					Server = data_server_list:get(ServerID),		
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					SendQS = mochiweb_util:urlencode(QueryString),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payouw?" ++ SendQS,
					Response = httpc:request(get, {Url, []}, [], []),

					Amount = get_amount(QueryString),
					
					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							Result2 = get_value(Content2, <<"result">>),
							if Result2 == 1 ->
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "success",
								   Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "fail, game server no reply",
								   Req:ok({"text/html; charset=utf-8", Reply}),
								   ?ERR("ouw check_order failed. reason:game server return false,order:~w",[QueryString])
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "fail, game server no response", %% 订单未正确获取,需要重新获取订单
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("ouw check_order failed. reason:game server no response,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					Reply = "fail, pay result not 1",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("ouw check_order failed. reason:payresult not 1,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "fail sign wrong",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("ouw check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("amount", Notify_Data),
	trunc(erlang:list_to_integer(Value) * 10).


verify_sign(QueryString)->
	Sign = proplists:get_value("sign", QueryString),
	Str = get_original_encode_list(QueryString),
	?ERR("info:~s",[Str]),
	Check = md5(Str ++ "56adb9c1f2a2fab8"),
	Sign == Check.


get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	lists:foldl(fun({A,B},Acc) ->
						case A of
							"sign" ->
								Acc;
							_ ->
								Acc++A++"="++B
						end
						end, "", QS2).

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

test()->
[{"serverId","1"},
 {"callbackInfo","2_3000005"},
 {"openId","10390b295d230d2b"},
 {"orderId","m20140328130981350"},
 {"orderStatus","1"},
 {"payType","ALIPAY"},
 {"amount","1"},
 {"remark",[]},
 {"sign","0cefa6229722e41477d7ed81db49e4bb"}].

