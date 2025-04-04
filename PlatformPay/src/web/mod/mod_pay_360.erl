%% @author admin
%% @doc @todo Add description to mod_pay_360.


-module(mod_pay_360).
-include("common.hrl").
-include("record.hrl").

-define(srcType, 6).
%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
	Flag = proplists:get_value("gateway_flag", QueryString),
	if Flag =:= "success" ->
			case check_order(QueryString) of
				true ->
					GameRole = proplists:get_value("app_ext1", QueryString),
					[_ServerID,RoleID] = string:tokens(GameRole,"{},"),
					RoleID2 = list_to_integer(RoleID),
					ServerID2 = (RoleID2 div 1000000) - 1,
					Server = data_server_list:get(ServerID2),
					%% ?ERR("mod_pay_360 serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
					SendQS = proplists_to_str(QueryString),
					%% ?ERR("SendQS:~w",[SendQS]),
					%% ?ERR("ServerIP:~w",[Server#server.serverIP]),	
					HttpPort = integer_to_list(Server#server.serverHttpPort),
					Url = "http://"++Server#server.serverIP ++":"++HttpPort++"/pay360/?"++SendQS,
					%% ?ERR("Url:~w",[Url]),
					Response = httpc:request(get, {Url, []}, [], []),
					Amount = proplists:get_value("amount", QueryString),
					Amount2 = list_to_integer(Amount) div 10,
					case Response of
						{ok, {_,_,Content}} ->
							{Content2} = ejson:decode(Content),
							Result = get_value(Content2, <<"result">>),
							if Result == 1 ->
								   AccID = get_value(Content2, <<"accid">>),
								   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
									Reply = "ok",
									Req:ok({"text/html; charset=utf-8", Reply});
							   true ->
								    db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
								   	Reply = "failed",
									Req:ok({"text/html; charset=utf-8", Reply})
							end;
						_ ->
							db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryString),
							Reply = "failed",
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
				{false,Reason} ->
					db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
					?ERR("360 check_order failed. reason:~w,order:~w",[Reason,QueryString])
			end;
	   true ->
		    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
		   	Reply = "ok",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("360 pay failed order:~w",[QueryString])
	end.



%% ====================================================================
%% Internal functions
%% ====================================================================

check_order(QueryString) ->
	KeyList = proplists:get_keys(QueryString),
	KeyList2 = lists:sort(KeyList),
	{CheckStr,_} = lists:foldl(fun(E,{Acc,QS}) ->
										if E =:= "sign" orelse E =:= "sign_return" ->
												{Acc,QS};
											true ->
												Value = proplists:get_value(E,QS),
												{Acc++Value++"#",QS}
										end
									end,
					{[],QueryString},KeyList2),
	AppSec = "fe766e97181e4d5836d69c74056dd000",
	CheckStr2 = CheckStr ++ AppSec,
	Md5Local = md5(CheckStr2),
	Md5 = proplists:get_value("sign",QueryString),
	%% ?ERR("Md5:~w,Md5Local:~w, CheckStr:~w",[Md5,Md5Local,CheckStr2]),
	if Md5 =:= Md5Local ->
			true;
		true ->
			{false,2}
	end.


md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.


proplists_to_str(QueryString) ->
	KeyList = proplists:get_keys(QueryString),
	{Result,_} = lists:foldl(fun(E,{Acc,QS}) ->
									if Acc =:= [] ->
											Value = proplists:get_value(E,QS),
											{Acc++E++"="++Value,QS};
										true ->
											Value = proplists:get_value(E,QS),
											{Acc++"&"++E++"="++Value,QS}
									end
								end,
					{[],QueryString},KeyList),
	Result.


test() ->
	TestStr="order_id=1211090012345678901&app_key=1234567890abcdefghijklmnopqrstuv&product_id=p1&amount=10 1&app_uid=123456789&app_ext1=XXX201211091985&user_id=987654321&sign_type=md5&gateway_flag=s uccess&sign=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&sign_return=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
	case check_order(TestStr) of
		{false,Reason} ->
			?ERR("test output: fail reason:~w",[Reason]);
		{true,_} ->
			?ERR("test succees!!!")
	end.