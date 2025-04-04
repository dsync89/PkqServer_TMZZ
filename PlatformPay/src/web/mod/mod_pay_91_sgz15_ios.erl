%% @author admin
%% @doc @todo Add description to mod_pay_91.


-module(mod_pay_91_sgz15_ios).
-include("common.hrl").
-include("record.hrl").

-define(srcType,?ACCOUNT_TYPE_91).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("QS=~p\n",[QueryString]),
	AppID = proplists:get_value("AppId",QueryString),
	case check_appID(AppID) of
		{true , AppKey} ->
		   Act = proplists:get_value("Act", QueryString),
		   if Act == "1" ->
				  ProductName = proplists:get_value("ProductName", QueryString),
				  ConsumeStreamId = proplists:get_value("ConsumeStreamId", QueryString),
				  CooOrderSerial = proplists:get_value("CooOrderSerial", QueryString),
				  Uin = proplists:get_value("Uin", QueryString),
				  GoodsId = proplists:get_value("GoodsId", QueryString),
				  GoodsInfo = proplists:get_value("GoodsInfo", QueryString),
				  GoodsCount = proplists:get_value("GoodsCount", QueryString),
				  OriginalMoney = proplists:get_value("OriginalMoney", QueryString),
				  OrderMoney = proplists:get_value("OrderMoney", QueryString),
				  Note = proplists:get_value("Note", QueryString),
				  PayStatus = proplists:get_value("PayStatus", QueryString),
				  CreateTime = proplists:get_value("CreateTime",QueryString),
				  Sign = proplists:get_value("Sign", QueryString),
				  
				  Amount = trunc(erlang:list_to_float(OrderMoney) * 10),
				  
				  SignString = AppID ++ Act ++ ProductName ++ ConsumeStreamId ++ CooOrderSerial ++ Uin ++
								   GoodsId ++ GoodsInfo ++ GoodsCount ++ OriginalMoney ++ OrderMoney ++ 
								   Note ++ PayStatus ++ CreateTime ++ AppKey,

				  [_ServerID,RoleID] = string:tokens(Note,"A"),
				  RoleID2 = list_to_integer(RoleID),
				  LocalSign = sign([SignString]),
				  if Sign == LocalSign ->
						 if PayStatus == "1" ->

								ServerID2 = (RoleID2 div 1000000) - 1,
								Server = data_server_list:get(ServerID2),
								%% ?ERR("mod_pay_91 serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
								HttpPort = integer_to_list(Server#server.serverHttpPort),
								SendQS = mochiweb_util:urlencode(QueryString),
								Url = "http://"++Server#server.serverIP++":"++HttpPort++"/pay91_ios?" ++ SendQS,
								
								%% ?ERR("Url:~w",[Url]),
								Response = httpc:request(get, {Url, []}, [], []),	
								case Response of
									{ok, {_,_,Content}} ->
										{Content2} = ejson:decode(Content),
										Result = get_value(Content2, <<"result">>),
										if Result == 1 ->
											   AccID = get_value(Content2, <<"accid">>),
											   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
											   Reply = ejson:encode({[{<<"ErrorCode">>,<<"1">>},{<<"ErrorDesc">>,<<"接收成功">>}]}),
											   Req:ok({"text/html; charset=utf-8", Reply});
										   true ->
											   ?ERR("91 pay failed order:~w,Reason:GameServer return",[QueryString]),
											   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
											   Reply = ejson:encode({[{<<"ErrorCode">>,<<"0">>},{<<"ErrorDesc">>,<<"接收失败">>}]}),
											   Req:ok({"text/html; charset=utf-8", Reply})
										end;
									_ ->
										?ERR("91 pay failed order:~w,Reason:GameServer no response",[QueryString]),
										db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
										Reply = ejson:encode({[{<<"ErrorCode">>,<<"0">>},{<<"ErrorDesc">>,<<"接收失败">>}]}),
										Req:ok({"text/html; charset=utf-8", Reply})
								end;
							true ->
								?ERR("91 pay failed order:~w,Reason:pay status",[QueryString]),
								db_func:log_pay_info(5, RoleID2, 0, Amount, erlang:localtime(), 0, ?srcType, QueryString),
								Reply = ejson:encode({[{<<"ErrorCode">>,<<"0">>},{<<"ErrorDesc">>,<<"接收失败">>}]}),
								Req:ok({"text/html; charset=utf-8", Reply})
						 end;
					 true ->
						 ?ERR("91 pay failed order:~w,Reason:sign wrong",[QueryString]),
						 db_func:log_pay_info(2, RoleID2, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
						 Reply = ejson:encode({[{<<"ErrorCode">>,<<"5">>},{<<"ErrorDesc">>,<<"接收失败">>}]}),
						 Req:ok({"text/html; charset=utf-8", Reply})
				  end;
			  true ->
				  ?ERR("91 pay failed order:~w,reason:act wrong",[QueryString]),
				  db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
				  Reply = ejson:encode({[{<<"ErrorCode">>,<<"3">>},{<<"ErrorDesc">>,<<"接收失败">>}]}),
				  Req:ok({"text/html; charset=utf-8", Reply})
		   end;
	   _ ->
		   ?ERR("91 pay failed order:~w,reason:appid wrong",[QueryString]),
		   db_func:log_pay_info(2, 0, 0, 0, 0, erlang:localtime(), ?srcType, QueryString),
		   Reply = ejson:encode({[{<<"ErrorCode">>,<<"2">>},{<<"ErrorDesc">>,<<"接收失败">>}]}),
		   Req:ok({"text/html; charset=utf-8", Reply})
	end.




%% ====================================================================
%% Internal functions
%% ====================================================================

check_appID(AppID) ->
	case AppID of
		"116508" ->
			{true , "7fa47abbd9086e7613bcbec5c2405b9024933be1a7c768fc"};
%% 		"111254" ->
%% 			{true , "f629f78ef65109f2f612a529475bd0eb24ff9ae962fc2a07"};
		_ ->
			false
	end.

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



