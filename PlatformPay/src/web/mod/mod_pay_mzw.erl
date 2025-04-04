%% @author admin
%% @doc @todo Add description to pay_mod_mzw


-module(mod_pay_mzw).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,38).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_qs(),
	%%?ERR("QS in mzw:~w",[QueryString]),
	case check_sign(QueryString) of
		true ->
			AmountT = proplists:get_value("money", QueryString),
			Amount = list_to_integer(AmountT) * 10,
			%Actual_amount = proplists:get_value("actual_amount", QueryString),
			Pt = proplists:get_value("extern",QueryString),
%% 			[_ServerIDT,RoleID|_] = string:tokens(Pt,"A"),
			RoleID2 = erlang:list_to_integer(Pt),
			ServerID = (RoleID2 div 1000000) - 1,
			Server = data_server_list:get(ServerID),			
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			SendQS = mochiweb_util:urlencode(QueryString),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_mzw?" ++ SendQS,
			%?ERR("Url:~s",[Url]),
			Response = httpc:request(get, {Url, []}, [], []),
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result = get_value(Content2, <<"result">>),
					if Result == 1 ->
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = "SUCCESS",
						   Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = "gameserver return false",
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   ?ERR("mzw check_order failed. reason:game server return false,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
					Reply = "gameserver no response", %% 订单未正确获取,需要重新获取订单
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("mzw check_order failed. reason:game Server not response not 0,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "sign check fail",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("mzw check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QueryString)->
	OrderID = proplists:get_value("orderID",QueryString),
	ProductName = proplists:get_value("productName",QueryString),
	ProductDesc = proplists:get_value("productDesc",QueryString),
	ProductID = proplists:get_value("productID", QueryString),
	Amount = proplists:get_value("money", QueryString),
	Uid = proplists:get_value("uid", QueryString),
	Extern = proplists:get_value("extern", QueryString),
	Sign = proplists:get_value("sign",QueryString),
	util:md5("1e26d943d093096606308c6d64a4270d"++OrderID++ProductName++ProductDesc++ProductID++Amount++Uid++Extern++"54b3986d2c7a2") == Sign.


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

test()->
	QS=[{[111,114,100,101,114,95,105,100],[53,50,101,48,99,56,102,57,54,53,54,54,55,56,57,57,56]},{[97,109,111,117,110,116],[49,48]},{[111,114,100,101,114,95,117,105,100],[49,53,52,49,54,56,55,57,53,49]},{[115,111,117,114,99,101],[49,57,50,50,54,53,55,57,57,55]},{[97,99,116,117,97,108,95,97,109,111,117,110,116],[49,48]},{[112,116],[123,49,125,44,123,50,48,48,48,48,48,51,125]},{[115,105,103,110,97,116,117,114,101],[102,56,55,98,51,98,50,57,97,52,102,101,53,101,101,54,97,57,52,52,55,52,102,101,102,101,52,102,49,99,56,97,49,101,54,56,53,48,54,51]}],
	QS.
