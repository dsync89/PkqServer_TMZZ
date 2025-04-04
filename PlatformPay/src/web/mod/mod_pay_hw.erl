%% @author admin
%% @doc @todo Add description to pay_mod_hw


-module(mod_pay_hw).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,16).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	case verify_sign(QueryString) of
		true ->
			Result = proplists:get_value("result", QueryString),
			case Result of
				"0" ->
					GameRole = proplists:get_value("requestId", QueryString),
					%Subject = proplists:get_value("subject", QueryString),
					[_ServerIDT,RoleID|_] = string:tokens(GameRole, "{},"),
					RoleID2 = erlang:list_to_integer(RoleID),
					ServerID = (RoleID2 div 1000000) - 1,			
					Server = data_server_list:get(ServerID),		
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payhw",
					Amount = get_amount(QueryString),
					Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),	

					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							Result2 = get_value(Content2, <<"result">>),
							if Result2 == 1 ->
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = ejson:encode({[{<<"result">>,0}]}),
								   Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = ejson:encode({[{<<"result">>,94}]}),
								   Req:ok({"text/html; charset=utf-8", Reply}),
								   ?ERR("hw check_order failed. reason:game server return false,order:~w",[QueryString])
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = ejson:encode({[{<<"result">>,2}]}), %% 订单未正确获取,需要重新获取订单
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("hw check_order failed. reason:game server no response,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					Reply = ejson:encode({[{<<"result">>,97}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("hw check_order failed. reason:payresult not 0,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("hw check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("amount", Notify_Data),
	trunc(erlang:list_to_float(Value) * 10).

get_pubKey()->                                                   
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAKFbddw/wxwthJhOkeiom2FQK0GjnaYQEuOjfSf4+UotURQqwxnfUoXxdCTWkMFOC3sezULY5Zi8ZeZXothOVcECAwEAAQ==\n-----END PUBLIC KEY-----">>,
	PemEntries = public_key:pem_decode(PubKey),
	public_key:pem_entry_decode(hd(PemEntries)).

decode_data(Base64Data) ->
	Data = base64:decode(Base64Data),
	RSAPubKey = get_pubKey(),
	Data2 = public_key:decrypt_public(Data, RSAPubKey,['rsa_pkcs1_padding']),
	mochiweb_util:parse_qs(binary:bin_to_list(Data2)).

verify_sign(QueryString)->
	RSAPubKey = get_pubKey(),
	Base64Sign = proplists:get_value("sign",QueryString),
	Sign = base64:decode(Base64Sign),
	Data = get_original_encode_list(QueryString),
	public_key:verify(list_to_binary(Data), 'sha', Sign, RSAPubKey).


get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[H|L] = 
	lists:foldl(fun({A,B},Acc) ->
						case A of
							"sign" ->
								Acc;
							_ ->
								Acc++"&"++A++"="++B
						end
						end, "", QS2),
	L.

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
	QS=[{"result","0"},
		{"userName","900086000020110429"},
		{"productName",[48,46,49,48,229,133,131,229,174,157]},
		{"payType","4"},
		{"amount","0.10"},
		{"orderId","A201312310227147731F5317"},
		{"notifyTime","1388472000177"},
		{"requestId","{1},{2000049}"},
		{"sign",
		 "YsjewBoY8HetFGWEq4GVqqvxNYEnILxmUqCqWeFsCo9ZkC++ldCZOrSNguH8+BtGbupt8MOwRIOurxjOcdlLGg=="}],
	verify_sign(QS),
	QS.
