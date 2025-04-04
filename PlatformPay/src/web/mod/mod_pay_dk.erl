%% @author admin
%% @doc @todo Add description to mod_pay_dk.


-module(mod_pay_dk).
-include("common.hrl").
-include("record.hrl").

-define(srcType, 8).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	Amount = proplists:get_value("amount", QueryString),
	CardType = proplists:get_value("cardtype", QueryString),
	OrderID = proplists:get_value("orderid", QueryString),
	NetResult = proplists:get_value("result", QueryString),
	Timetamp = proplists:get_value("timetamp", QueryString),
	Aid = proplists:get_value("aid", QueryString),
	ClientSec = proplists:get_value("client_secret", QueryString),
	AppSec = "2f4dc6204c8fb62d9728485a34bdc0d0",
	ClientSecLocal = sign([Amount,CardType,OrderID,NetResult,Timetamp,AppSec,Aid]),
	%% ?ERR("md5:~w,md5local:~w",[ClientSec,ClientSecLocal]),
	if NetResult =:= "1" ->
			if ClientSec =:= ClientSecLocal ->
					[_ServerID,RoleID] = string:tokens(Aid,"."),
					RoleID2 = list_to_integer(RoleID),
					ServerID2 = (RoleID2 div 1000000) - 1,
					Amount2 = list_to_integer(Amount)*10,
					Server = data_server_list:get(ServerID2),
					%% ?ERR("mod_pay_dk serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
					SendQS = mochiweb_util:urlencode(QueryString),
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paydk/?"++SendQS,
					%% ?ERR("Url:~w",[Url]),
					Response = httpc:request(get, {Url, []}, [], []),
					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							Result = get_value(Content2, <<"result">>),
							if Result == 1 ->
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
									Reply = "SUCCESS",
									Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								   db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
								   	Reply = "ERROR_FAIL",
									Req:ok({"text/html; charset=utf-8", Reply})
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "ERROR_FAIL",
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
			   true ->
				   db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 1, ?srcType, QueryString),
				   	Reply = "ERROR_SIGN",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("dk pay failed order:~w",[QueryString])
			end;
		true ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = "SUCCESS",
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

