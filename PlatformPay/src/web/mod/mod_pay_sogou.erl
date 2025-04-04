%% @author admin
%% @doc @todo Add description to pay_mod_sogou


-module(mod_pay_sogou).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,23).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryString = Req:parse_post(),
	%?ERR("info:~w",[QueryString]),
	case verify_sign(QueryString) of
		true ->
			case check_appid(QueryString) of
				true ->
					GameRole = proplists:get_value("appdata", QueryString),
					[_ServerIDT,RoleID|_] = string:tokens(http_uri:decode(GameRole), "{},"),
					RoleID2 = erlang:list_to_integer(RoleID),
					ServerID = (RoleID2 div 1000000) - 1,	
					Server = data_server_list:get(ServerID),			
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paysogou",

					Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),	
					Amount = get_amount(QueryString),

					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							Result = get_value(Content2, <<"result">>),
							if Result == 1 ->
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "OK",
								   Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
								   Reply = "ERR_200",
								   Req:ok({"text/html; charset=utf-8", Reply}),
								   ?ERR("sogou check_order failed. reason:game server return false,order:~w",[QueryString])
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "ERR_500", %% 订单未正确获取,需要重新获取订单
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("sogou check_order failed. reason:game server no response,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					Reply = "ERR_100",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("sogou check_order failed. reason:appid wrong,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "ERR_100",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("sogou check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(QueryString)->
	Value = proplists:get_value("amount1", QueryString),
	trunc(erlang:list_to_integer(Value) * 10).

check_appid(QueryString) ->
	Gid = proplists:get_value("gid", QueryString),
	Gid == "268".

verify_sign(QueryString)->
	Amount1 = "amount1="++proplists:get_value("amount1",QueryString),
	Amount2 = "amount2="++proplists:get_value("amount2",QueryString),
	Appdata = "appdata="++proplists:get_value("appdata",QueryString),
	Date = "date="++proplists:get_value("date", QueryString),
	Gid = "gid="++proplists:get_value("gid", QueryString),
	Oid = "oid="++proplists:get_value("oid", QueryString),
	RealAmount = "realAmount="++proplists:get_value("realAmount", QueryString),
	Role = "role="++proplists:get_value("role", QueryString),
	Sid = "sid="++proplists:get_value("sid", QueryString),
	Time = "time="++proplists:get_value("time", QueryString),
	Uid = "uid="++proplists:get_value("uid", QueryString),
	Sign = proplists:get_value("auth", QueryString),
	PayKey = "{7D334D88-66D6-4B23-971D-18A641C9E606}",
	Str = Amount1 ++ "&" ++ Amount2 ++ "&" ++ Appdata ++ "&" ++ Date ++ "&" ++
			  Gid ++ "&" ++ Oid ++ "&" ++ RealAmount ++ "&" ++ Role ++ "&" ++
			  Sid ++ "&" ++ Time ++ "&" ++ Uid ++ "&" ++ PayKey,
	md5(Str) == Sign.

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
	[{"gid","148"},
 {"sid","1"},
 {"uid","8416967"},
 {"role",[]},
 {"oid","P140211_87959"},
 {"date","140211"},
 {"amount1","1"},
 {"amount2","10"},
 {"time","20140211175513"},
 {"appdata","%7B1%7D%2C%7B2000018%7D"},
 {"realAmount","1"},
 {"auth","781bdde64e7d2df23a7c7c611a4ff119"}].
