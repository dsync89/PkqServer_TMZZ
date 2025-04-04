%% @author admin
%% @doc @todo Add description to mod_pay_wdj.


-module(mod_pay_wdj).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType, 7).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_post(),
	%% ?ERR("Req:~w,QS=~p\n,body=~p",[Req,QueryString,Req:recv_body()]),
	case verify_sign(QueryString) of
		true ->
			Content = proplists:get_value("content", QueryString),
			%% ?ERR("Content:~p",[Content]),
			Content2 = str_to_term(Content),
			%% ?ERR("Content2:~p",[Content2]),
			GameRole = proplists:get_value("out_trade_no", Content2),
			%% ?ERR("GameRole:~w",[GameRole]),
			GameRole2 = mochiweb_util:parse_qs(GameRole),
			RoleID = proplists:get_value("RoleID",GameRole2),
			RoleID2 = list_to_integer(RoleID),
			ServerID2 = (RoleID2 div 1000000) - 1,
			Server = data_server_list:get(ServerID2),
			%% ?ERR("mod_pay_wdj serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
			HttpPort = integer_to_list(Server#server.serverHttpPort),	
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paywdj/",
			%% ?ERR("Url:~w",[Url]),
			Response = httpc:request(post, {Url,[], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),
			Amount = proplists:get_value("money",Content2),
			Amount2 = Amount div 10,
			case Response of
				{ok, {_,_,Content3}} ->
					{Content4} = ejson:decode(Content3),
					Result = get_value(Content4, <<"result">>),
					if Result == 1 ->
						   AccID = get_value(Content4, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
						   %% ?ERR("wdj order success:~w",[QueryString]),
						   Reply = <<"success">>,
							Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							?ERR("wdj order fail3:~w",[QueryString]),
						   	Reply = <<"success">>,
						   Req:ok({"text/html; charset=utf-8", Reply})
					end;
				Err ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
					?ERR("wdj order fail2:~w\nErr:~w",[QueryString,Err]),
					Reply = <<"success">>,
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			?ERR("wdj order fail:~w",[QueryString]),
			Reply = "success",
			Req:ok({"text/html; charset=utf-8", Reply})	
	end.
	



%% ====================================================================
%% Internal functions
%% ====================================================================

%% verify_sign(Data) -> true | false
%% Data - list
verify_sign(QueryString) ->
     %% 读取公钥和签名，
     %% 读取的PubKey和Base64Sign都是binary类型
     %% {ok, PubKey} = file:read_file("wdj_pubkey.pem"),
	 PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCd95FnJFhPinpNiE/h4VA6bU1rzRa5+a25BxsnFX8TzquWxqDCoe4xG6QKXMXuKvV57tTRpzRo2jeto40eHKClzEgjx9lTYVb2RFHHFWio/YGTfnqIPTVpi7d7uHY+0FZ0lYL5LlW4E2+CQMxFOPRwfqGzMjs1SDlH7lVrLEVy6QIDAQAB\n-----END PUBLIC KEY-----">>,
     %% {ok, Base64Sign} = file:read_file("sig.txt"),
	 Data = proplists:get_value("content", QueryString),
	 Base64Sign = proplists:get_value("sign",QueryString),
     %% 将签名的base64编码解码
     Sign = base64:decode(Base64Sign),
     %% 获取RSA public key
     PemEntries = public_key:pem_decode(PubKey),
     RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),
     %% 验证签名
     public_key:verify(list_to_binary(Data), 'sha', Sign, RSAPubKey).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

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

