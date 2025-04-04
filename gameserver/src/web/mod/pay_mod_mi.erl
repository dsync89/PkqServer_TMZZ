%% @author admin
%% @doc @todo Add description to pay_mod_mi.


-module(pay_mod_mi).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
	AppId = proplists:get_value("appId", QueryString),
	CpOrderId = proplists:get_value("cpOrderId", QueryString),
	CpUserInfo = proplists:get_value("cpUserInfo", QueryString),
	Uid = proplists:get_value("uid", QueryString),
	OrderId = proplists:get_value("orderId", QueryString),
	OrderStatus = proplists:get_value("orderStatus", QueryString),
	PayFee = proplists:get_value("payFee", QueryString),
	ProductCode = proplists:get_value("productCode", QueryString),
	ProductName = proplists:get_value("productName", QueryString),
	ProductCount = proplists:get_value("productCount", QueryString),
	PayTime = proplists:get_value("payTime", QueryString),
	Signature = proplists:get_value("signature", QueryString),
    AppIdLocal = "2882303761517283416",
    AppKey = "Nsk95kbteICNnhFFXr3Zwg==",
	OriStr = "appId="++AppId++"&cpOrderId="++CpOrderId++"&cpUserInfo="++CpUserInfo++"&orderId="++OrderId++"&orderStatus="++OrderStatus
			++"&payFee="++PayFee++"&payTime="++PayTime++"&productCode="++ProductCode++"&productCount="++ProductCount++"&productName="++ProductName++"&uid="++Uid,
	<<SignTemp:160/integer>> = crypto:hmac('sha', AppKey, OriStr),
	SignLocal = lists:flatten(io_lib:format("~40.16.0b", [SignTemp])),
	%% ?ERR("md5:~w,md5local:~w",[ClientSec,ClientSecLocal]),
	if SignLocal =:= Signature andalso AppIdLocal =:= AppId ->
		   [_ServerID,RoleID] = string:tokens(CpUserInfo,"."),
		   RoleID2 = list_to_integer(RoleID),
		   RoleAccID = db_sql:get_role_accid(RoleID2),
		   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
		   Req:ok({"text/html; charset=utf-8", Reply}),
		   
		   Amount2 = list_to_integer(PayFee) div 10,
			QS = mochiweb_util:urlencode(QueryString),
			pay_gold2(RoleID2,Amount2,QS,Signature,9);
	   true ->
		   	Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("mi pay failed order:~w",[QueryString])
	end.


pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_mi(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================


