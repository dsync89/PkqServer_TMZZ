%% @author admin
%% @doc @todo Add description to mod_pay_kw.


-module(mod_pay_kw).
-include("common.hrl").
-include("record.hrl").

-define(srcType, 36).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:QS=~p\n",[QueryString]),
	Amount = proplists:get_value("money", QueryString),
	UserID = proplists:get_value("user_id", QueryString),
	OrderNo = proplists:get_value("order_no", QueryString),
	Ext = proplists:get_value("ext_order_num", QueryString),
	Sign = proplists:get_value("token", QueryString),
	Key = "16d564e2bfd2e865523f774db8d25f32",
	MSign = md5(UserID ++ Amount ++ OrderNo ++ Key),

	if Sign =:= MSign ->
		   [_ServerID,RoleID|_] = string:tokens(Ext,"A"),
		   RoleID2 = list_to_integer(RoleID),
		   ServerID2 = (RoleID2 div 1000000) - 1,
		   Amount2 = list_to_integer(Amount)*10,
		   Server = data_server_list:get(ServerID2),
		   %% ?ERR("mod_pay_kw serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
		   SendQS = mochiweb_util:urlencode(QueryString),
		   HttpPort = integer_to_list(Server#server.serverHttpPort),
		   Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paykw/?"++SendQS,
		   %% ?ERR("Url:~w",[Url]),
		   Response = httpc:request(get, {Url, []}, [], []),
		   case Response of
			   {ok, {_,_,Content}} ->
				   {Content2} = ejson:decode(Content),
				   Result = get_value(Content2, <<"result">>),
				   if Result == 1 ->
						  AccID = get_value(Content2, <<"accid">>),
						  db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
						  Reply = ejson:encode({[{<<"result">>,<<"1">>}]}),
						  Req:ok({"text/html; charset=utf-8", Reply});
					  true ->
						  db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
						  Reply = ejson:encode({[{<<"result">>,<<"-4">>}]}),
						  Req:ok({"text/html; charset=utf-8", Reply})
				   end;
			   _ ->
				   db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
				   Reply = ejson:encode({[{<<"result">>,<<"-4">>}]}),
				   Req:ok({"text/html; charset=utf-8", Reply})
		   end;
	   true ->
		   db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 1, ?srcType, QueryString),
		   Reply = ejson:encode({[{<<"result">>,<<"-2">>}]}),
		   Req:ok({"text/html; charset=utf-8", Reply}),
		   ?ERR("kw pay failed order:~w",[QueryString])
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

test()->
	[{"user_id","226649405"},
                                       {"ext_product_id",[]},
                                       {"order_no","GP140505173008284670"},
                                       {"money","1"},
                                       {"ext_order_num",
                                        "1A2000146A1399282193"},
                                       {"token",
                                        "ee32166f9643222fc4d89bba033af007"},
                                       {"game_id","6019000"}].