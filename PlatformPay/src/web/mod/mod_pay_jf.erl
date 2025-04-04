%% @author admin
%% @doc @todo Add description to pay_mod_pp


%% 机锋的数据较为特殊,原始数据没有使用application/x-www-form-urlencoded格式,只能使用recv_body提取数据
%% 发送给gameserver时,使用了这个格式

-module(mod_pay_jf).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType, 24).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
	QueryStringPost = erlang:binary_to_list(Req:recv_body()),
	%?ERR("body:~w",[QueryStringPost]),
	QueryStringGet = Req:parse_qs(),
	%?ERR("~w",[QueryStringGet]),
	Sign = proplists:get_value("sign", QueryStringGet),
	Time = proplists:get_value("time", QueryStringGet),
	case verify_info(Sign, Time) of
		true ->
			{ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(QueryStringPost),
			[{_,_,_,_,Amount,_}] = xmerl_xpath:string("//cost/text()",ParsedDocumentRootElement),
			[{_,_,_,_,GameRole,_}] = xmerl_xpath:string("//order_id/text()", ParsedDocumentRootElement),
			Amount2 = list_to_integer(Amount),
			[_ServerIDT,RoleID|_] = string:tokens(GameRole,"."),
			RoleID2 = erlang:list_to_integer(RoleID),
			ServerID = (RoleID2 div 1000000) - 1,
			Server = data_server_list:get(ServerID),			
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			SendQS = mochiweb_util:urlencode(QueryStringGet),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payjf/?"++SendQS,
			Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded",Req:recv_body()}, [], []),			
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result = get_value(Content2, <<"result">>),
					if Result == 1 ->	
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount2, erlang:localtime(), 1, ?srcType, QueryStringGet++QueryStringPost),
						   Reply = "<response><ErrorCode>1</ErrorCode><ErrorDesc>Success</ErrorDesc></response>",
						   Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryStringGet++QueryStringPost),
						   Reply = "<response><ErrorCode>1</ErrorCode><ErrorDesc>GameServerReturnFalse</ErrorDesc></response>",
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   ?ERR("ky check_order failed. reason:gameserver return false,order:~w",[QueryStringGet++QueryStringPost])
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount2, erlang:localtime(), 1, ?srcType, QueryStringGet++QueryStringPost),
					Reply = "<response><ErrorCode>0</ErrorCode><ErrorDesc>Retry</ErrorDesc></response>",
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("jf check_order failed. reason:game server no response,order:~w",[QueryStringGet++QueryStringPost])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryStringGet++QueryStringPost),
			Reply = "<response><ErrorCode>1</ErrorCode><ErrorDesc>SignWrong</ErrorDesc></response>",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("jf pay failed order:~w",[QueryStringGet++QueryStringPost])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(Sign,Time)->
	AppID = "26792675",
	Sign == md5( AppID ++ Time).

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

test()->
	QueryStringGet=[{"time","1393316276"},{"sign","1c0a8ba0efac1bce415521e55b8767c3"}],
		Sign = proplists:get_value("sign", QueryStringGet),
	Time = proplists:get_value("time", QueryStringGet),
	verify_info(Sign, Time).

%% test2()->
%% 	Texts="<response ><order_id>00002010090922</order_id><cost>10</cost><appkey>834266553</appkey><create_time>12345678</create_time></response>",
%% 	{ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(Texts),
%% 	[{_,_,_,_,Amount,_}] = xmerl_xpath:string("//cost/text()",ParsedDocumentRootElement),
%% 	[{_,_,_,_,GameRole,_}] = xmerl_xpath:string("//order_id/text()", ParsedDocumentRootElement),
%% 	Amount2 = trunc(list_to_integer(Amount) / 10).

