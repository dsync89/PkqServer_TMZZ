%% @author admin
%% @doc @todo Add description to pay_mod_pp


%% 发送给gameserver时,使用了这个格式

-module(mod_pay_vivo).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType, 27).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	%?ERR("~w",[QueryString]),
	case verify_info(QueryString) of
		true ->
			Status = proplists:get_value("respCode", QueryString),
			case check_payStatus(Status) of
				true ->
					GameRole = proplists:get_value("storeOrder",QueryString),
					Amount = proplists:get_value("orderAmount",QueryString),
					Amount2 = trunc(list_to_float(Amount) * 10) ,
					[_ServerIDT,RoleID|_] = string:tokens(GameRole,"_"),
					RoleID2 = erlang:list_to_integer(RoleID),
					ServerID = (RoleID2 div 1000000) - 1,
					Server = data_server_list:get(ServerID),			
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payvivo",
					Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),			
					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							Result = get_value(Content2, <<"result">>),
							if Result == 1 ->	
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "<response><ErrorCode>1</ErrorCode><ErrorDesc>Success</ErrorDesc></response>",
								   Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								   db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "<response><ErrorCode>1</ErrorCode><ErrorDesc>GameServerReturnFalse</ErrorDesc></response>",
								   Req:ok({"text/html; charset=utf-8", Reply}),
								   ?ERR("ky check_order failed. reason:gameserver return false,order:~w",[QueryString])
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "<response><ErrorCode>0</ErrorCode><ErrorDesc>Retry</ErrorDesc></response>",
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("vivo check_order failed. reason:game server no response,order:~w",[QueryString])
					end;
				_ ->
					?ERR("vivo pay failed order:~w,Reason:pay status",[QueryString]),
					db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					Reply = "<response><ErrorCode>1</ErrorCode><ErrorDesc>WrongStatus</ErrorDesc></response>",
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "<response><ErrorCode>1</ErrorCode><ErrorDesc>SignWrong</ErrorDesc></response>",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("vivo pay failed order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(QueryString)->
	Info = get_original_encode_list(QueryString),
	%io:format("~ts~n",[Info]),
	Key = "&" ++ util:md5("8EBBE49DD24776FAFB2F3F6BAB6FC48F"),
	Signature = proplists:get_value("signature", QueryString),
	Sign = md5(Info++Key),
	%	io:format("~s~n~s~n",[Signature,Sign]),
	Signature == Sign.

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[H|L] = 
	lists:foldl(fun({A,B},Acc) ->
						case {A,B} of
							{"signature",B} ->
								Acc;
							{"signMethod",B} ->
								Acc;
							{_, ""} ->
								Acc;
							{_, ?undefined} ->
								Acc;
							_ ->
								Acc++"&"++A++"="++B
						end
						end, "", QS2),
	L.

check_payStatus(Status)->
	case Status of
		"0000" ->
			true;
		_ ->
			false
	end.


test()->
	[{"respCode","0000"},
 {"signMethod","MD5"},
 {"vivoOrder","139393097842527050822"},
 {"storeOrder","123456878"},
 {"orderAmount","0.10"},
 {"channelFee","0.24"},
 {"respMsg",
  [228,186,164,230,152,147,229,174,140,230,136,144]},
 {"storeId","20140213145436275637"},
 {"channel","1002"},
 {"signature","010f05286889528e1793407da85c4566"}].
