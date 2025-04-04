%% @author admin
%% @doc @todo Add description to pay_mod_lenovo


-module(mod_pay_lenovo).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,21).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	%?ERR("QS in lenovo:~w",[QueryString]),
	Sign = proplists:get_value("sign", QueryString),
	TransData = proplists:get_value("transdata", QueryString),
	case check_sign(TransData,Sign) of
		true ->
			{DataList} = ejson:decode(TransData),
			case get_value(DataList,<<"result">>) of
				0 ->
					Amount = trunc(proplists:get_value(<<"money">>,DataList) div 10),
					Pt = erlang:binary_to_list(proplists:get_value(<<"cpprivate">>,DataList)),
					[_ServerIDT,RoleID] = string:tokens(Pt,"{},"),
					RoleID2 = erlang:list_to_integer(RoleID),
					ServerID = (RoleID2 div 1000000) - 1,
					Server = data_server_list:get(ServerID),			
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					SendQS = mochiweb_util:urlencode(QueryString),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paylenovo?" ++ SendQS,
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
								   Reply = "FAILED",
								   Req:ok({"text/html; charset=utf-8", Reply}),
								   ?ERR("lenovo check_order failed. reason:game server return false,order:~w",[QueryString])
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "FAILED", %% 订单未正确获取,需要重新获取订单
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("lenovo check_order failed. reason:game Server not response not 0,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					Reply = "FAILED",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("lenovo check_order failed. reason:status wrong,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "FAILED",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("lenovo check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(TransData,Sign)->
	Key = "MTk3Njc1NTg1OTIxOTQzREUyQjY2ODNEMzhERkEwMDQ0MEExRTdDRE1USXhNekV4TkRVNE56SXdPVEkwTVRjM056RXJNVGt3TVRVMU5qY3dORFUwTnprME1EY3lOemMyTWpjek16Y3lPRGt6TkRjMk16VTFNVEU1",
	Key1 = erlang:binary_to_list(base64:decode(Key)),
	Key2 = lists:sublist(Key1, 41, length(Key1)),
	Key3 = erlang:binary_to_list(base64:decode(Key2)),
	[P_keyT,M_keyT] = string:tokens(Key3, "+"),
	P_key = erlang:list_to_integer(P_keyT),
	M_key = erlang:list_to_integer(M_keyT),
	Sign2 = string:tokens(Sign, " "),
	Md5Sign = lists:foldl(fun(H,Acc) ->
								  I = crypto:mod_exp(erlang:list_to_integer(H,16),P_key, M_key),
								  Acc++get_value_byte(I)
								  end,[],Sign2),
	Md5Sign2 = http_uri:decode(string:strip(Md5Sign)),
	Md5Data = util:md5(TransData),
	Md5Data == Md5Sign2.



get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

get_value_byte(Num)->
	get_value_byte(Num,"").
get_value_byte(0,S)->
	lists:flatten(S);
get_value_byte(In, S)->
	P = In rem 256,
	P1 = io_lib:format("~c", [P]),
	get_value_byte(erlang:trunc(In div 256), P1++S).
	
test()->
	[{[116,114,97,110,115,100,97,116,97],[123,34,101,120,111,114,100,101,114,110,111,34,58,34,49,97,50,48,48,48,48,48,51,97,49,51,57,49,56,53,52,54,53,53,34,44,34,116,114,97,110,115,105,100,34,58,34,48,50,53,49,52,48,50,48,56,49,56,49,55,51,54,50,49,57,51,50,34,44,34,119,97,114,101,115,105,100,34,58,55,44,34,97,112,112,105,100,34,58,34,50,48,48,49,56,55,48,48,48,48,48,48,48,50,50,48,48,49,56,55,34,44,34,102,101,101,116,121,112,101,34,58,50,44,34,109,111,110,101,121,34,58,49,48,44,34,99,111,117,110,116,34,58,49,44,34,114,101,115,117,108,116,34,58,48,44,34,116,114,97,110,115,116,121,112,101,34,58,48,44,34,116,114,97,110,115,116,105,109,101,34,58,34,50,48,49,52,45,48,50,45,48,56,32,49,56,58,49,55,58,53,51,34,44,34,99,112,112,114,105,118,97,116,101,34,58,34,123,49,125,44,123,50,48,48,48,48,48,51,125,34,44,34,112,97,121,116,121,112,101,34,58,52,48,49,125]},{[115,105,103,110],[101,53,97,57,56,101,100,55,56,54,50,98,54,57,56,48,54,49,48,51,98,50,48,54,97,99,51,51,97,52,52,32,99,57,101,55,102,48,101,52,99,54,97,99,52,51,54,51,49,53,57,97,100,49,99,101,48,98,97,49,97,51,100,97,32,57,102,55,56,53,102,57,98,97,101,49,54,51,101,101,102,49,49,53,52,102,56,49,56,102,101,100,50,57,97,48,48,32]}].

