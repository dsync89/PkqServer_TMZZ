%% @author admin
%% @doc @todo Add description to pay_mod_oppo


-module(mod_pay_oppo).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,22).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	%?ERR("info:~w",[QueryString]),
	case verify_sign(QueryString) of
		true ->
			%GameRole = proplists:get_value("uid", QueryString),
			%Subject = proplists:get_value("subject", QueryString),
			GameRole = proplists:get_value("attach", QueryString),
			[_ServerIDT,RoleID|_] = string:tokens(GameRole, "{}."),
			RoleID2 = erlang:list_to_integer(RoleID),
			ServerID = (RoleID2 div 1000000) - 1,	
			Server = data_server_list:get(ServerID),			
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payoppo",
			Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),	
			Amount = list_to_integer(proplists:get_value("price", QueryString)) div 10,
			 
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result = get_value(Content2, <<"result">>),
					if Result == 1 ->
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = "result=OK&resultMsg=成功",
						   Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = "result=FAIL&resultMsg=游戏服验证失败",
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   ?ERR("oppo check_order failed. reason:game server return false,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
					Reply = "result=FAIL&resultMsg=游戏服无响应", %% 订单未正确获取,需要重新获取订单
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("oppo check_order failed. reason:game server no response,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "result=FAIL&resultMsg=验签失败",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("oppo check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCmreYIkPwVovKR8rLHWlFVw7YDfm9uQOJKL89Smt6ypXGVdrAKKl0wNYc3/jecAoPi2ylChfa2iRu5gunJyNmpWZzlCNRIau55fxGW0XEu553IiprOZcaw5OuYGlf60ga8QT6qToP0/dpiL/ZbmNUO9kUhosIjEu22uFgR+5cYyQIDAQAB\n-----END PUBLIC KEY-----">>,
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
	NotifyID = "notifyId="++proplists:get_value("notifyId", QueryString),
	PartnerOrder = "partnerOrder="++proplists:get_value("partnerOrder", QueryString),
	ProductName = "productName="++proplists:get_value("productName", QueryString),
	ProductDesc = "productDesc="++proplists:get_value("productDesc", QueryString),
	Price = "price="++proplists:get_value("price", QueryString),
	Count = "count="++proplists:get_value("count", QueryString),
	Attach = "attach="++proplists:get_value("attach",QueryString),
	NotifyID ++ "&" ++ PartnerOrder ++ "&" ++ ProductName ++ "&" ++ 
		ProductDesc ++ "&" ++ Price ++ "&" ++ Count ++ "&" ++ Attach.

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.
