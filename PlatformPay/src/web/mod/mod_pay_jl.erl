%% @author admin
%% @doc @todo Add description to pay_mod_jl


-module(mod_pay_jl).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,34).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	%?ERR("info:~w",[QueryString]),
	case verify_sign(QueryString) of
		true ->
			Amount = trunc(erlang:list_to_float(proplists:get_value("deal_price", QueryString)) * 10),
			GameRole = proplists:get_value("out_order_no", QueryString),
			[_ServerIDT,RoleID|_] = string:tokens(GameRole, "A"),
			RoleID2 = erlang:list_to_integer(RoleID),
			ServerID = (RoleID2 div 1000000) - 1,			
			Server = data_server_list:get(ServerID),		
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payjl",
			Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),	
			
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
						   Reply = "fail, server return false",
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   ?ERR("jl check_order failed. reason:game server return false,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
					Reply = "fail, server no response", %% 订单未正确获取,需要重新获取订单
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("jl check_order failed. reason:game server no response,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "fail, sign check fail",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("jl check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================


get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCJdkX28oMIldtBcC/vVRri6uuQmLZ3vx56E5OOLUg8Ku/gifuujmT7ixiXbXj3aUYBE10vtIKstVKCfsfGE3iQhsKcfnKJf6zg4W1WhFcIEm1VP0LC+D0xQazI2YcOC6mQhUL7SNjtvFrlAku1XajOEjNEsxYyZlVYcQ1/KZmbCQIDAQAB\n-----END PUBLIC KEY-----">>,
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
	[_H|L] = 
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
[{"api_key","CEAA27BCA99F487D8CDC3C84939CF262"},
 {"close_time","20140506142850"},
 {"create_time","20140506142830"},
 {"deal_price","0.01"},
 {"out_order_no","1A2000152A1399357709"},
 {"pay_channel","100"},
 {"submit_time","20140506142829"},
 {"user_id","null"},
 {"sign",
  "F3aCoFHfDl+lxCXSPWC6dz2k9Iy37oi7iPo9vEHiGUlwWrvVL9nqC7FXBDHDxFYlwNYWHLQ9NH5sZJmqnQALF244KpLglGUc0xQwBByhxQjNvOB4Ka0XbVlGkFpZH57OxP6gSgfLftb8YRNEQ/uVMisQOjtiU7dZM9rObXlaGMg="}].
