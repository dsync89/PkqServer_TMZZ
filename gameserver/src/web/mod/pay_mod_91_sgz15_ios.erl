%% @author admin
%% @doc @todo Add description to pay_mod_91.


-module(pay_mod_91_sgz15_ios).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	AppID = proplists:get_value("AppId",QueryString),
	Act = proplists:get_value("Act", QueryString),
	case check_appid_and_act(AppID,Act) of
		{true, AppKey} ->
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
			
%% 			SignString = "AppId="++AppID ++ "&Act=" ++ Act ++ "&ProductName=" ++ ProductName ++
%% 							 "&ConsumeStreamId=" ++ ConsumeStreamId ++ "&CooOrderSerial=" ++ CooOrderSerial ++
%% 							 "&Uin=" ++ Uin ++ "&GoodsId=" ++ GoodsId ++ "&GoodsInfo=" ++ GoodsInfo ++
%% 							 "&GoodsCount=" ++ GoodsCount ++ "&OriginalMoney=" ++ OriginalMoney ++
%% 							 "&OrderMoney=" ++ OrderMoney ++ "&Note=" ++ Note ++ "&PayStatus=" ++ PayStatus ++
%% 							 "&CreateTime=" ++ CreateTime ++ "&Sign=" ++ Sign,
			
			SignString = AppID ++ Act ++ ProductName ++ ConsumeStreamId ++ CooOrderSerial ++ Uin ++
							 GoodsId ++ GoodsInfo ++ GoodsCount ++ OriginalMoney ++ OrderMoney ++ 
							 Note ++ PayStatus ++ CreateTime ++ AppKey,
			
			LocalSign = sign([SignString]),
			if Sign == LocalSign andalso PayStatus == "1" ->
				   [_ServerID,RoleID] = string:tokens(Note,"A"),
				   RoleID2 = list_to_integer(RoleID),
				   RoleAccID = db_sql:get_role_accid(RoleID2),
				   
				   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
				   Req:ok({"text/html; charset=utf-8", Reply}),

				   Amount = trunc(erlang:list_to_float(OrderMoney) * 10),
				   QS = mochiweb_util:urlencode(QueryString),
				   pay_gold2(RoleID2,Amount,QS,Sign	,?ACCOUNT_TYPE_91);
			   true ->
				   Reply = ejson:encode({[{<<"result">>,0}]}),
				   Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_91(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================

%% check_appid_and_act(AppID,Act)->
%% 	if AppID == "1042" orelse AppID == "1043" orelse AppID == "1039" orelse AppID == "1041" orelse AppID == "110939" orelse AppID =="110935" -> 
%% 		if Act == "1" ->
%% 			   true ;
%% 		   true ->
%% 			   false
%% 		end;
%% 	   true ->
%% 		   false
%% 	end.

check_appid_and_act(AppID, Act) ->
	if Act == "1" ->
		   case AppID of
			   "116508" ->
				   {true , "7fa47abbd9086e7613bcbec5c2405b9024933be1a7c768fc"};
			   _ ->
				   false
		   end;
	   true ->
		   false
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
