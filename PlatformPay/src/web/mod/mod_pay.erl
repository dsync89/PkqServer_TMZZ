%% @author admin
%% @doc 91充值异步通知
%% Created 2013-6-19


-module(mod_pay).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).

%% Internal functions
-export([]).



%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 获取角色基本信息
handle(Req) ->
    QueryString = Req:parse_qs(),
	%io:format("QS=~p\n",[QueryString]),
	PayStatus = platform_tool:get_value("PayStatus", QueryString),
	if PayStatus == "1" ->
		   AppId = platform_tool:get_value("AppId", QueryString),
		   ConfigAppId = data_common:get(app_id_91),
	Act = platform_tool:get_value("Act", QueryString),
		   case integer_to_list(ConfigAppId) == AppId
				andalso Act =:= "1" of
			   true ->
				  handle_pay2(Req, QueryString, AppId, PayStatus, Act);
			  false->
				  ignore
		   end;
	   true ->
		   ignore
	end.

handle_pay2(Req, QueryString, AppId, PayStatus, Act) ->
	ProductName = platform_tool:get_value("ProductName", QueryString),
	ConsumeStreamId = platform_tool:get_value("ConsumeStreamId", QueryString),
	CooOrderSerial = platform_tool:get_value("CooOrderSerial", QueryString),
	Uin = platform_tool:get_value("Uin", QueryString),
	GoodsId = platform_tool:get_value("GoodsId", QueryString),
	GoodsInfo = platform_tool:get_value("GoodsInfo", QueryString),
	GoodsCount = platform_tool:get_value("GoodsCount", QueryString),
	OriginalMoney = platform_tool:get_value("OriginalMoney", QueryString),
	OrderMoney = platform_tool:get_value("OrderMoney", QueryString),
	Note = platform_tool:get_value("Note", QueryString),
	% PayStatus
	CreateTime = platform_tool:get_value("PayStatus", QueryString),
	Sign = platform_tool:get_value("Sign", QueryString),
	AppKey = data_common:get(app_key_91),
	Md5 = util:md5(AppId++Act++ProductName++ConsumeStreamId++CooOrderSerial++
				   Uin++GoodsId++GoodsInfo++GoodsCount++OriginalMoney++OrderMoney++
				   Note++PayStatus++CreateTime++AppKey),
%% 	if Md5 == Sign ->
%% 		   pay_server:pay(
	ok.

	


%% ====================================================================
%% Internal functions
%% ====================================================================


