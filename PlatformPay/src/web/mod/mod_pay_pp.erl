%% @author admin
%% @doc @todo Add description to pay_mod_pp


-module(mod_pay_pp).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType, 11).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	Info = decode_data(QueryString),
	OrderID = proplists:get_value("order_id", QueryString),
	BillNo = proplists:get_value("billno", QueryString),
	Account = proplists:get_value("account", QueryString),
	Amount = proplists:get_value("amount", QueryString),
	Status = proplists:get_value("status", QueryString),
	AppID = proplists:get_value("app_id", QueryString),
	Amount2 = erlang:trunc(erlang:list_to_float(Amount)) * 10,
	case verify_info(Info, OrderID, BillNo, Account, Amount, Status, AppID) of
		true ->
			GameRole = proplists:get_value("roleid", QueryString),
			[_ServerIDT,RoleID] = string:tokens(GameRole,"{},"),
			RoleID2 = erlang:list_to_integer(RoleID),
			ServerID = (RoleID2 div 1000000) - 1,
			Server = data_server_list:get(ServerID),			
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paypp/",%"?"++SendQS,
			Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),			
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result = get_value(Content2, <<"result">>),
					if Result == 1 ->
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = "success",
						   Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = "fail",
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   ?ERR("ky check_order failed. reason:gameserver return false,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
					Reply = "fail",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("pp check_order failed. reason:game server no response,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "fail",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("pp pay failed order:~w",[QueryString])
	end.

decode_data(QueryString)->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApCh8hZckG9RSvyJajg72LIPMM8J01IW3thMT44JsDmOje7ZF1jJm57TgrXF0eRcN7a6z1AXRvLmCvnVD6zPdyMo5b65yqzhuzRwbEdr3MaHHJom+e15jVx4CMjToWDkWQgaaTLJJcLicskeQ/5BTZkdE2zrrKVbfCJRg7lk8DBN/rhICrPUsJy7zE/2a3ab4WTG3d0tJa3Pm9psLjX9DrNFt3NVBvSB0Ngp/RZn20UTEVdik/fdfxHcgFqCG3AofFnlDDyh1y+iFO5CDopq1jufRuEjuhlWJ/QNchJ7/Pamj7byUqsyGp9a98G30SqNViKHVZVIVkOhwwQaf42oJoQIDAQAB\n-----END PUBLIC KEY-----">>,
	Base64Sign = proplists:get_value("sign",QueryString),
	Sign = base64:decode(Base64Sign),
	PemEntries = public_key:pem_decode(PubKey),
	RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),
	Data = public_key:decrypt_public(Sign, RSAPubKey,['rsa_pkcs1_padding']),
	{struct, List} = mochijson2:decode(Data),
	List.


test_decode()->
	Base64Sign = "cr1W9Amng5h5HrmtwCKOfGpfRdHbaZL/UocVTjmRYxhCZqgTm2c7LZyaHalygbPxckAo0OIvGed+HFjJQmBT3k5Gpmikg2ceYIHPTDMiVKX1L3M50i+5S7zVhl2FOTHRxrovoNFhFCuCWLW87IUX3uGLErQg/o9He6FZB6GIKqaXv3rhWG3fb4u6vYTMZA1Lkg0sRPLZY21rZ88QNVunUnU5vwoIdaViMiJ3EtEURbmbIvWp5XfofcJRfY0UIrPstZL1KmUIkTbgYwygFh4fXYPWBJfXPnsIMB61pP24Xs7ofsLMYpuxC1hCNz90kLAKdOtyhlNf5gh4e+uG9OD0lQ==",
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApCh8hZckG9RSvyJajg72LIPMM8J01IW3thMT44JsDmOje7ZF1jJm57TgrXF0eRcN7a6z1AXRvLmCvnVD6zPdyMo5b65yqzhuzRwbEdr3MaHHJom+e15jVx4CMjToWDkWQgaaTLJJcLicskeQ/5BTZkdE2zrrKVbfCJRg7lk8DBN/rhICrPUsJy7zE/2a3ab4WTG3d0tJa3Pm9psLjX9DrNFt3NVBvSB0Ngp/RZn20UTEVdik/fdfxHcgFqCG3AofFnlDDyh1y+iFO5CDopq1jufRuEjuhlWJ/QNchJ7/Pamj7byUqsyGp9a98G30SqNViKHVZVIVkOhwwQaf42oJoQIDAQAB\n-----END PUBLIC KEY-----">>,
	Sign = base64:decode(Base64Sign),

	PemEntries = public_key:pem_decode(PubKey),

	RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),

	Data = public_key:decrypt_public(Sign, RSAPubKey,['rsa_pkcs1_padding']),

	mochijson2:decode(Data).


%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(Info, VOrderID, VBillNo, VAccount, VAmount, VStatus, VAppID)->
	OrderID = binary:bin_to_list(proplists:get_value(<<"order_id">>, Info)),
	BillNO = binary:bin_to_list(proplists:get_value(<<"billno">>, Info)),
	Account = binary:bin_to_list(proplists:get_value(<<"account">>, Info)),
	Amount = binary:bin_to_list(proplists:get_value(<<"amount">>, Info)),
	Status = erlang:integer_to_list(proplists:get_value(<<"status">>, Info)),
	AppID = binary:bin_to_list(proplists:get_value(<<"app_id">>, Info)),	
	if OrderID == VOrderID , BillNO == VBillNo, Account == VAccount , 
	   Status == VStatus, AppID == VAppID ,Amount == VAmount ->
		   true;
	   true ->
		   false
	end.

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.
