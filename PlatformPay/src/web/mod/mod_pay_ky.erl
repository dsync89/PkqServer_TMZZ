%% @author admin
%% @doc @todo Add description to pay_mod_ky


-module(mod_pay_ky).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,12).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	case verify_sign(QueryString) of
		true ->
			NotifyData = proplists:get_value("notify_data", QueryString),
			case check_pay(NotifyData) of
				true ->
					%GameRole = proplists:get_value("uid", QueryString),
					%Subject = proplists:get_value("subject", QueryString),
					GameRole = proplists:get_value("dealseq", QueryString),
					[_ServerIDT,RoleID|_] = string:tokens(GameRole, "_"),
					RoleID2 = erlang:list_to_integer(RoleID),
					ServerID = (RoleID2 div 1000000) - 1,	
					Server = data_server_list:get(ServerID),			
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payky",

					Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),	
					Amount = get_amount(NotifyData),

					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							Result = get_value(Content2, <<"result">>),
							if Result == 1 ->
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "success",
								   Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "success",
								   Req:ok({"text/html; charset=utf-8", Reply}),
								   ?ERR("ky check_order failed. reason:game server return false,order:~w",[QueryString])
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "fail", %% 订单未正确获取,需要重新获取订单
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("ky check_order failed. reason:game server no response,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					Reply = "success",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("ky check_order failed. reason:payresult not 0,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "success",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("ky check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("fee", Notify_Data2),
	trunc(erlang:list_to_float(Value) * 10).

check_pay(Notify_Data) ->
	Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("payresult", Notify_Data2),
	if Value =:= "0" ->
		   true;
	   true ->
		   false
	end.

get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDXMg+UPhLRdsuPr8M2d3FeXnjOTWCIALyx1IQjrQj6KxpyUAKmWXUoR60mZktvJQ0OskgoDZLX80R93FFpMK8pIB1uSN4AxcF5VgRHEwww1+kb1RlsKM9681+XHUERCru8tTPK8kgLa6Wc5NR76UrZtOgd40WlC7m1ZoGv2Im3VQIDAQAB\n-----END PUBLIC KEY-----">>,
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


get_original_encode_list(QueryString)->
	DealSeq = "dealseq="++proplists:get_value("dealseq", QueryString),
	NotifyData = "notify_data="++proplists:get_value("notify_data", QueryString),
	OrderID = "orderid="++proplists:get_value("orderid", QueryString),
	Uid = "uid="++proplists:get_value("uid", QueryString),
	Subject = "subject="++proplists:get_value("subject", QueryString),
	Version = "v="++proplists:get_value("v", QueryString),
	DealSeq ++ "&" ++ NotifyData ++ "&" ++ OrderID ++ "&" ++ Subject ++ "&" ++ Uid ++ "&" ++ Version.

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
	QS=[{"sign","BmqxCC7C1umX6FKBwBrPsg4i5OoQAMrqmin7dKSJhzceEIDVDCU/PQq1XMZi3EeDB28x14BLY0ZlpJ3/ESCfQ66pcEKPD07Jm7UyE9mLa60V3V+P1KaDG5bHY6t9UQOWE1cNjaSWDchK3SNZawhKDmvVnqaQcGlHqDsHPlZHO5Y="},
        {"uid","{1},{2000013}"},
        {"v", "1.0"},
        {"notify_data","Ylj5cSyh/QqFWGsr/6WcJ81T/mFndRH1xmpuJHULPhToiX1v8Dh2koiVUq10Z854dSXYcr9XJ1OP\nLSvF0Uf9Jh2z7t3wnhyy0wsQsho+33s93hsOueKPt9GySCvReWsyci9aDOC1c8WGBZK7E02QyqSt\nIIBqBfklIRZU1nI+LQs="},
        {"dealseq","2000013-2031002060"},
        {"subject",[54,48,229,133,131,229,174,157]},
        {"orderid","131225260619544586338"}],
	verify_sign(QS),
	QS.
