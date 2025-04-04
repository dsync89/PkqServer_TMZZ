%% @author admin
%% @doc @todo Add description to pay_mod_yyb


-module(mod_pay_yyb).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(srcType,25).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(Req, Method, Path)->
	QueryString = Req:parse_qs(),
	case verify_sign(QueryString, Method, Path) of
		true ->
			GameRole = proplists:get_value("appmeta", QueryString),
			[_ServerIDT,RoleID|_] = string:tokens(GameRole, "."),
			RoleID2 = erlang:list_to_integer(RoleID),		
			ServerID = (RoleID2 div 1000000) - 1,			
			Server = data_server_list:get(ServerID),		
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			Amount = get_amount(QueryString),
			SendQS = mochiweb_util:urlencode(QueryString),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payyyb?" ++ SendQS,

			Response = httpc:request(get, {Url, []}, [], []),
			
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result2 = get_value(Content2, <<"result">>),
					if Result2 == 1 ->
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = ejson:encode({[{<<"result">>,0}]}),
						   Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
						   Reply = ejson:encode({[{<<"result">>,94}]}),
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   ?ERR("yyb check_order failed. reason:game server return false,order:~w",[QueryString])
					end;
				_ ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?srcType, QueryString),
					Reply = ejson:encode({[{<<"result">>,2}]}), %% 订单未正确获取,需要重新获取订单
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("yyb check_order failed. reason:game server no response,order:~w",[QueryString])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?srcType, QueryString),
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("yyb check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("payitem", Notify_Data),
	[_ID,Price,_Num] = string:tokens(Value, "*"),
	trunc(erlang:list_to_float(Price)).

verify_sign(QueryString, Method, Path)->
	AppKey = <<"aX42IE7jyV7lWfOq&">>,
	SourceList = get_original_encode_list2(QueryString),
	SourceList2 = erlang:atom_to_list(Method)++ "&" ++
					  http_uri:encode(Path) ++ "&" ++
					  SourceList,
	SignTemp = crypto:hmac('sha', AppKey, SourceList2),
	Sign = binary_to_list(base64:encode(SignTemp)),
	CheckSign = proplists:get_value("sig", QueryString),
	Sign == CheckSign.


get_original_encode_list2(QS) ->
	lists:foldr(fun($*,Acc)->
						[$%,$2,$A|Acc];
				   (E,Acc)->
						[E|Acc]
				end, [], get_original_encode_list(QS)).

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[_H|L] = 
		lists:foldl(fun({A,B},Acc) ->
							case A of
								"sig" ->
									Acc;
								_ ->
									Acc++"&"++A++"="++ open_encode(B)
							end
					end, "", QS2),
	http_uri:encode(L).

open_encode(B) ->
	ExList = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!*()",
	lists:foldr(fun(E, Acc)->
						case lists:member(E, ExList) of
							true ->
								[E|Acc];
							_ ->
								"%" ++ integer_to_list(E, 16) ++ Acc
						end
				end, [], B).

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

test()->
	httpc:request(get,{"http://192.168.1.27:12380/payyyb?amt=600&appid=1000000437&appmeta=1.2001195.-269888279*qdqb*qq&billno=-APPDJSX18379-20140325-2100366172&clientver=android&openid=4419E40F66D1D574C640F623273A7EDB&payamt_coins=0&payitem=1*60.0*1&providetype=5&pubacct_payamt_coins=&token=CF37244D189D1762625A8C80E056398120291&ts=1395752436&version=v3&zoneid=1&sig=hI7wcprR9eO%2F9VoSqynqlF8TFoM%3D",[]},[],[]).


