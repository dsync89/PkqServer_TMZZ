%% @author admin
%% @doc @todo Add description to mod_pay_dl.


-module(mod_pay_dl).
-include("common.hrl").
-include("record.hrl").

-define(srcType,4).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	Result = proplists:get_value("result",QueryString),
	case Result of
		"1" ->
			Money = proplists:get_value("money",QueryString),
			Order = proplists:get_value("order",QueryString),
			Mid = proplists:get_value("mid",QueryString),
			Time = proplists:get_value("time",QueryString),
			Signature = proplists:get_value("signature",QueryString),
			GameRole = proplists:get_value("ext",QueryString),
			Key="NoRKr4EZwIMm",
			SignString = "order="++Order++"&money="++Money++"&mid="++Mid++"&time="++Time++"&result="++Result++"&ext="++GameRole++"&key="++Key,
			SignLocal = md5(SignString),
			Amount = trunc(list_to_float(Money)*10),
			[_ServerID,RoleID] = string:tokens(GameRole,"."),
			RoleID2 = list_to_integer(RoleID),
			if SignLocal =:= Signature ->
				   ServerID2 = (RoleID2 div 1000000) - 1,
					Server = data_server_list:get(ServerID2),
					%% ?ERR("mod_pay_dk serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
					SendQS = mochiweb_util:urlencode(QueryString),
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paydl/?"++SendQS,
					%% ?ERR("Url:~w",[Url]),
					Response = httpc:request(get, {Url, []}, [], []),
					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							GameResult = get_value(Content2, <<"result">>),
							if GameResult == 1 ->
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
									Reply = "success",
									Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   	Reply = "fail",
									Req:ok({"text/html; charset=utf-8", Reply})
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "fail",
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
			   true ->
				   db_func:log_pay_info(2, RoleID2, 0, 0, erlang:localtime(), 1, ?srcType, QueryString),
				   	Reply = "success",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("dl pay failed order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
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

sign(StrList) ->
	md5(sign2(StrList)).

sign2([A]) ->
	A;
sign2([A,B]) ->
	A++B;
sign2([]) ->
	"";
sign2([A|T]) ->
	A++sign2(T).

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

