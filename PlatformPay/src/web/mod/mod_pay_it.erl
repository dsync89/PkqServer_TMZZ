%% @author admin
%% @doc @todo Add description to pay_mod_it


-module(mod_pay_it).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,14).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	{NotifyData, SignData} = decode_data(proplists:get_value("notify_data", QueryString)),
	case verify_sign(QueryString, SignData) of
		true ->
			case check_pay(NotifyData) of
				true ->
					GameRoleT = proplists:get_value(<<"order_id_com">>, NotifyData),
					GameRole = binary:bin_to_list(GameRoleT),
					[_ServerIDT,RoleID|_] = string:tokens(GameRole, "."),
					RoleID2 = erlang:list_to_integer(RoleID),
					ServerID = (RoleID2 div 1000000) - 1,
					Server = data_server_list:get(ServerID),			
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payit",
					
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
								   ?ERR("it check_order failed. reason:game server return false,order:~w",[QueryString])
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "fail", %% 订单未正确获取,需要重新获取订单
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("it check_order failed. reason:game Server not response not 0,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					Reply = "success",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("it check_order failed. reason:result not fail,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "fail",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("it check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = binary:bin_to_list(proplists:get_value(<<"amount">>, Notify_Data)),
	trunc(erlang:list_to_float(Value) * 10).

check_pay(Notify_Data) ->
	Value = proplists:get_value(<<"result">>, Notify_Data),
	if Value =:= <<"success">> ->
		   true;
	   true ->
		   false
	end.

get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC2kcrRvxURhFijDoPpqZ/IgPlAgppkKrek6wSrua1zBiGTwHI2f+YCa5vC1JEiIi9uw4srS0OSCB6kY3bP2DGJagBoEgj/rYAGjtYJxJrEiTxVs5/GfPuQBYmU0XAtPXFzciZy446VPJLHMPnmTALmIOR5Dddd1Zklod9IQBMjjwIDAQAB\n-----END PUBLIC KEY-----">>,
	PemEntries = public_key:pem_decode(PubKey),
	public_key:pem_entry_decode(hd(PemEntries)).

decode_data(Base64Data) ->
	Data = binary:bin_to_list(base64:decode(Base64Data)),
	RSAPubKey = get_pubKey(),
	Length = trunc(length(Data) / 128),
	{_, Data2} = lists:foldl(fun(_, {DataAcc, Data2Acc})->
									 {Data0,DataAcc2}=lists:split(128,DataAcc),
									 Data2Acc2 = public_key:decrypt_public(list_to_binary(Data0), RSAPubKey),
									 {DataAcc2, Data2Acc ++ binary:bin_to_list(Data2Acc2)}
							 end , {Data,""}, lists:seq(1, Length)),
	{struct, List} = mochijson2:decode(Data2),
%% 	lists:foldl(fun({A,B}, Acc)when is_binary(A) ->
%% 						[{binary:bin_to_list(A), binary:bin_to_list(B)}|Acc];
%% 				   ({A,B}, Acc) ->
%% 						[{A,B}|Acc]
%% 				   	end,[],List),
	{List, Data2}.

verify_sign(QueryString, Data)->
	RSAPubKey = get_pubKey(),
	Base64Sign = proplists:get_value("sign",QueryString),
	Sign = base64:decode(Base64Sign),
	public_key:verify(erlang:list_to_binary(Data), 'sha', Sign, RSAPubKey).


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
	QS = [{[110,111,116,105,102,121,95,100,97,116,97],[103,48,98,56,97,97,110,67,57,76,104,109,56,74,76,57,120,87,117,54,121,57,119,102,89,70,111,66,43,79,98,113,118,111,118,81,89,110,70,118,57,76,108,74,51,98,49,90,109,117,80,112,57,72,97,74,70,68,111,57,98,116,77,122,70,82,81,79,47,49,110,119,101,81,90,97,71,99,72,75,98,48,75,121,84,120,52,57,48,106,119,50,90,86,116,69,51,83,48,70,74,116,82,57,69,52,48,82,79,76,43,74,110,69,100,71,108,88,112,82,120,108,47,109,89,88,72,86,111,89,54,117,73,115,109,53,84,113,115,112,90,115,70,77,51,53,100,76,43,109,77,112,90,68,103,76,84,117,99,97,49,122,48,65,111,52,69,115,48,115,108,84,78,72,57,75,75,97,105,78,101,105,99,119,69,113,68,65,86,121,56,104,99,117,80,53,65,80,66,116,65,48,72,75,112,112,89,66,84,113,113,73,77,122,67,87,111,86,49,67,67,100,70,68,85,72,51,66,74,104,55,76,65,57,54,113,69,83,112,47,109,73,119,72,114,113,87,77,47,73,98,105,105,74,47,52,48,117,113,83,70,118,102,88,54,103,119,47,114,48,102,85,79,101,70,85,83,43,117,81,108,120,112,71,72,118,71,83,102,103,120,80,109,90,117,120,121,87,73,78,108,109,78,100,98,114,54,55,115,108,80,71,105,51,118,113,115,49,98,75,48,117,101,104,86,55,47,76,110,109,122,98,76,49,108,122,81,53,72,81,112,103,61,61]},{[115,105,103,110],[108,83,120,102,66,75,116,84,47,106,82,113,82,50,112,117,121,52,119,85,82,56,82,78,81,79,52,75,109,106,111,109,70,120,75,82,81,107,122,79,87,43,115,99,121,86,79,57,83,79,115,122,74,101,102,120,73,47,86,89,110,81,107,55,109,115,113,54,119,84,113,67,55,82,110,76,49,105,107,50,88,98,106,68,120,73,72,48,82,108,90,78,57,50,101,109,51,108,86,76,111,71,73,102,102,52,120,68,119,102,122,109,107,55,48,75,70,79,70,114,87,102,57,101,70,86,83,122,48,119,43,110,107,67,66,111,51,82,43,83,109,105,67,99,121,111,117,108,90,74,83,84,80,117,51,114,115,83,106,71,100,84,86,99,117,89,57,90,122,57,119,61]}],
decode_data(proplists:get_value("notify_data", QS)),
	QS.

test2()->
		QS=[{"sign","BmqxCC7C1umX6FKBwBrPsg4i5OoQAMrqmin7dKSJhzceEIDVDCU/PQq1XMZi3EeDB28x14BLY0ZlpJ3/ESCfQ66pcEKPD07Jm7UyE9mLa60V3V+P1KaDG5bHY6t9UQOWE1cNjaSWDchK3SNZawhKDmvVnqaQcGlHqDsHPlZHO5Y="},
        {"uid","{1},{2000013}"},
        {"v", "1.0"},
        {"notify_data","Ylj5cSyh/QqFWGsr/6WcJ81T/mFndRH1xmpuJHULPhToiX1v8Dh2koiVUq10Z854dSXYcr9XJ1OP\nLSvF0Uf9Jh2z7t3wnhyy0wsQsho+33s93hsOueKPt9GySCvReWsyci9aDOC1c8WGBZK7E02QyqSt\nIIBqBfklIRZU1nI+LQs="},
        {"dealseq","2000013-2031002060"},
        {"subject",[54,48,229,133,131,229,174,157]},
        {"orderid","131225260619544586338"}],
		X = proplists:get_value("notify_data", QS),
		io:format("x:~w\n",[X]),
		decode_data(X).
