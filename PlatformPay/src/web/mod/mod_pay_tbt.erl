%% @author admin
%% @doc @todo Add description to mod_pay_tbt


-module(mod_pay_tbt).
-include("common.hrl").
-include("record.hrl").

-define(srcType, 15).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	Key = "&key=cz*WM7wG9qf4DaPnzK*Mj7w9Tq4sCamc",
	Source = proplists:get_value("source", QueryString),
	Trade_no = proplists:get_value("trade_no", QueryString),
	Amount = proplists:get_value("amount", QueryString),
	Partner = "&partner=" ++ proplists:get_value("partner", QueryString),
	Paydes = proplists:get_value("paydes", QueryString), 
	Debug = proplists:get_value("debug", QueryString), 
	Sign = proplists:get_value("sign", QueryString),
	TbOrder = proplists:get_value("tborder", QueryString),
	ParamString = "source=" ++ Source ++ "&trade_no=" ++ Trade_no ++ "&amount=" ++ Amount ++
				 Partner ++ "&paydes=" ++ Paydes ++ "&debug=" ++ Debug ++ "&tborder=" ++ TbOrder ++ Key,
	MSign = sign([ParamString]),
	Amount2 = trunc(erlang:list_to_integer(Amount) / 10),
	if Sign =:= MSign ->
		   [_ServerID,RoleID] = string:tokens(Paydes,"."),
		   RoleID2 = list_to_integer(RoleID),
		   ServerID2 = (RoleID2 div 1000000) - 1,
		   Server = data_server_list:get(ServerID2),
		   %% ?ERR("mod_pay_tbt serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
		   SendQS = mochiweb_util:urlencode(QueryString),
		   HttpPort = integer_to_list(Server#server.serverHttpPort),
		   Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paytbt/?"++SendQS,
		   %% ?ERR("Url:~w",[Url]),
		   Response = httpc:request(get, {Url, []}, [], []),
		   case Response of
			   {ok, {_,_,Content}} ->
				   {Content2} = ejson:decode(Content),
				   Result = get_value(Content2, <<"result">>),
				   if Result == 1 ->
						  AccID = get_value(Content2, <<"accid">>),
						  db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
						  Reptbt =  ejson:encode({[{<<"status">>,<<"success">>}]}),
						  Req:ok({"text/html; charset=utf-8", Reptbt});
					  true ->
						  db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
						  Reptbt =  ejson:encode({[{<<"status">>,<<"fail">>}]}),
						  Req:ok({"text/html; charset=utf-8", Reptbt}),
						  ?ERR("ky check_order failed. reason:gameserver return false,order:~w",[QueryString])
				   end;
			   _ ->
				   db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
				   Reptbt =  ejson:encode({[{<<"status">>,<<"fail">>}]}),
				   Req:ok({"text/html; charset=utf-8", Reptbt}),
				   ?ERR("tbt check_order failed. reason:gameserver no response,order:~w",[QueryString])
				   
		   end;
	   true ->
		   db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
		   Reptbt =  ejson:encode({[{<<"status">>,<<"fail">>}]}),
		   Req:ok({"text/html; charset=utf-8", Reptbt}),
		   ?ERR("tbt pay failed reason:sign wrong order:~w",[QueryString])
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

