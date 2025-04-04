%% @author admin
%% @doc @todo Add description to mod_pay_az.


-module(mod_pay_az).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType, 10).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QS = Req:parse_post(),
	%% ?ERR("QS:~w",[QS]),
	QS2 = proplists:get_value("data", QS),
	%% ?ERR("QS2:~w",[QS2]),
	AppSec = "mL7b1LHPC4Hrtm5Dr800e6Si",
	QS3 = decrypt_des3_ecb(AppSec,QS2),
	{QueryString} = ejson:decode(QS3),
	%% ?ERR("QueryString:~w",[QueryString]),
	ReqCode = proplists:get_value(<<"code">>, QueryString),
	if ReqCode =:= 1 ->
			GameRoleBin = proplists:get_value(<<"cpInfo">>, QueryString),
			GameRole = binary_to_list(GameRoleBin), 
			[_ServerID,RoleID] = string:tokens(GameRole,"."),
			RoleID2 = list_to_integer(RoleID),
			ServerID2 = (RoleID2 div 1000000) - 1,
			AmountBin = proplists:get_value(<<"orderAmount">>, QueryString),			
			Amount = list_to_integer(binary_to_list(AmountBin)),
			Amount2 = Amount div 10,
			Server = data_server_list:get(ServerID2),
			%% ?ERR("mod_pay_wdj serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
			HttpPort = integer_to_list(Server#server.serverHttpPort),	
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payaz/",
			%% ?ERR("Url:~w",[Url]),
			Response = httpc:request(post, {Url,[], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),
			case Response of
				{ok, {_,_,Content3}} ->
					{Content4} = ejson:decode(Content3),
					Result = get_value(Content4, <<"result">>),
					if Result == 1 ->
						   AccID = get_value(Content4, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
							%% ?ERR("az order success:~w",[QueryString]),
							Reply = "success",
							Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						    db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
							?ERR("az order fail3:~w",[QueryString]),
						   	Reply = "success",
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
				Err ->
					db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
					?ERR("az order fail2:~w\nErr:~w",[QueryString,Err]),
					Reply = "success",
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		true ->
			db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			?ERR("az order fail:~w",[QueryString]),
			Reply = "success",
			Req:ok({"text/html; charset=utf-8", Reply})	
	end.
	



%% ====================================================================
%% Internal functions
%% ====================================================================

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.


decrypt_des3_ecb(Key,Cipher) ->
	<<K1:64,K2:64,K3:64>> = list_to_binary(Key), 
	Cipher1 = base64:decode_to_string(Cipher),
	{_,_,_,TextReal,_} = lists:foldl(fun(E,{Key1,Key2,Key3,CipherList,CipherByte})->
							CipherByte2 = [E|CipherByte],
							if length(CipherByte2) < 8 ->
									{Key1,Key2,Key3,CipherList,CipherByte2};
								true ->
									Bin = crypto:block_decrypt(des3_cbc,[Key1,Key2,Key3],<<0:64>>,list_to_binary(lists:reverse(CipherByte2))),
									{Key1,Key2,Key3,CipherList ++ binary_to_list(Bin),[]}
							end
						end,{<<K1:64>>,<<K2:64>>,<<K3:64>>,[],[]}, Cipher1),
	LastWord = lists:last(TextReal),
	if LastWord < 9 ->
			TextLen = length(TextReal) - LastWord,
			lists:sublist(TextReal,TextLen);
		true ->
			TextReal
	end.










