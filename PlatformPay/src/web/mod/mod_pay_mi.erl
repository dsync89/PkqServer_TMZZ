%% @author admin
%% @doc @todo Add description to mod_pay_mi.


-module(mod_pay_mi).

-include("common.hrl").
-include("record.hrl").

-define(srcType, 9).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
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
	Amount = list_to_integer(PayFee) div 10,
	%% ?ERR("Signature:~w,SignLocal:~w",[Signature,SignLocal]),
	if SignLocal =:= Signature andalso AppIdLocal =:= AppId ->
			[_ServerID,RoleID] = string:tokens(CpUserInfo,"."),
			RoleID2 = list_to_integer(RoleID),
			ServerID2 = (RoleID2 div 1000000) - 1,
			Server = data_server_list:get(ServerID2),
			%% ?ERR("mod_pay_dk serverid:~w, server:~w, roleid:~w",[ServerID2,Server,RoleID]),
			SendQS = mochiweb_util:urlencode(QueryString),
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paymi/?"++SendQS,
			%% ?ERR("Url:~w",[Url]),
			Response = httpc:request(get, {Url, []}, [], []),
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result = get_value(Content2, <<"result">>),
					if Result == 1 ->
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = ejson:encode({[{<<"errcode">>,200}]}),
						   Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   ?ERR("mi pay failed order3:~w",[QueryString]),
						   Reply = ejson:encode({[{<<"errcode">>,200}]}),
						   Req:ok({"text/html; charset=utf-8", Reply})
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
					?ERR("mi pay failed order2:~w",[QueryString]),
					Reply = ejson:encode({[{<<"errcode">>,200}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
	   true ->
		   db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 1, ?srcType, QueryString),
		   Reply = "fail",
		   HResponse = mochiweb_headers:make([]),
		   HResponse1 = mochiweb_headers:enter("Content-Type", "text/html; charset=utf-8",HResponse),
		   Req:respond({3515, HResponse1, Reply}),
		   ?ERR("mi pay failed order:~w",[QueryString])
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

