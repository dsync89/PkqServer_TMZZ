-module(mod_pay_yj6).
-include("common.hrl").
-include("record.hrl").

-define(srcType,70).
-define(KEY, "FHHBJEOQ9PTRE3HFW6IEFT77TSTRW39G").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_qs(),
	?ERR("~p", mochiweb_util:urlencode(QueryString)),
	Sign = proplists:get_value("sign", QueryString),
	Fee = proplists:get_value("fee", QueryString),
	String = get_original_encode_list(lists:keydelete("sign", 1, QueryString)),
	MD5 = md5(String ++ ?KEY),
	case MD5 =:= Sign of
		true ->
			GameRole = proplists:get_value("cbi", QueryString),
			[_ServerIDT,RoleID|_] = string:tokens(GameRole, "|"),
			RoleID2 = erlang:list_to_integer(RoleID),		
			ServerID = (RoleID2 div 1000000) - 1,			
			Server = data_server_list:get(ServerID),		
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			Amount = erlang:list_to_integer(Fee) div 10,
			SendQS = mochiweb_util:urlencode(QueryString),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_yijie6?" ++ SendQS,

			Response = httpc:request(get, {Url, []}, [], []),
			
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result2 = get_value(Content2, <<"result">>),
					if Result2 == 1 ->
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Req:ok({"text/html; charset=utf-8", "SUCCESS"});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Req:ok({"text/html; charset=utf-8", "FAIL"}),
						   ?ERR("yj check_order failed. reason:game server return false,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
					Req:ok({"text/html; charset=utf-8", "FAIL"}),
					?ERR("yj check_order failed. reason:game server no response,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Req:ok({"text/html; charset=utf-8", "FAIL"}),
			?ERR("yj check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[_H|L] = 
		lists:foldl(fun({A,B},Acc) ->
							case A of
								"sign" ->
									Acc;
								_ ->
									Acc++"&"++A++"="++ B
							end
					end, "", QS2),
	L.