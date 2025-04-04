%% @author admin
%% @doc @todo Add description to pay_mod_yyh.


-module(pay_mod_yyh).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("QS in yyh:~w",[QueryString]),
	Sign = proplists:get_value("sign", QueryString),
	TransData = proplists:get_value("transdata", QueryString),
	case check_sign(TransData,Sign) of
		true ->
			{DataList} = ejson:decode(TransData),
			case get_value(DataList,<<"result">>) of
				0 ->
					Amount = trunc(proplists:get_value(<<"money">>,DataList) div 10),
					Pt = erlang:binary_to_list(proplists:get_value(<<"cpprivate">>,DataList)),
					[_ServerIDT,RoleID|_] = string:tokens(Pt,"."),
					RoleID2 = erlang:list_to_integer(RoleID),
					RoleAccID = db_sql:get_role_accid(RoleID2),
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					QS = mochiweb_util:urlencode(QueryString),
					Sign2 = util:md5(proplists:get_value("sign", QueryString)),
					pay_gold2(RoleID2,Amount,QS,Sign2	,26);
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_yyh(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================


check_sign(TransData,Sign)->
	Key = "MzU0MDQwOTdFMzFEMjg4QzREM0Q4QTBBMUQzNjc3RDlCNUU4OTNFNE1UUTFPVEEwTmprNU56STFNRGswTmprNU1qTXJNVGN5T1RVek9URTVNalk0TWpFM056QXlNamcwTnpNeU5UVTFNVGszTkRFeE5UZ3hPRFV4",
	Key1 = erlang:binary_to_list(base64:decode(Key)),
	Key2 = lists:sublist(Key1, 41, length(Key1)),
	Key3 = erlang:binary_to_list(base64:decode(Key2)),
	[P_keyT,M_keyT] = string:tokens(Key3, "+"),
	P_key = erlang:list_to_integer(P_keyT),
	M_key = erlang:list_to_integer(M_keyT),
	Sign2 = string:tokens(Sign, " "),
	Md5Sign = lists:foldl(fun(H,Acc) ->
								  I = crypto:mod_exp(erlang:list_to_integer(H,16),P_key, M_key),
								  Acc++get_value_byte(I)
								  end,[],Sign2),
	Md5Sign2 = http_uri:decode(string:strip(Md5Sign)),
	Md5Data = util:md5(TransData),
	Md5Data == Md5Sign2.



get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

get_value_byte(Num)->
	get_value_byte(Num,"").
get_value_byte(0,S)->
	lists:flatten(S);
get_value_byte(In, S)->
	P = In rem 256,
	P1 = io_lib:format("~c", [P]),
	get_value_byte(erlang:trunc(In div 256), P1++S).

test()->
	[{"transdata",
  "{\"exorderno\":\"1.2000069.-1706845311\",\"transid\":\"01414022718153895073\",\"waresid\":1,\"appid\":\"10031200000004100312\",\"feetype\":2,\"money\":1,\"count\":1,\"result\":0,\"transtype\":0,\"transtime\":\"2014-02-27 18:18:17\",\"cpprivate\":\"1.2000069.-1706845311\",\"paytype\":401}"},
 {"sign",
  "2c3388c984a8430f6bb02926f250b3a 487cfb6594d341e914d7625ed0cb2fcb 4373a887efa9dc13480d39c6be634bf1 "}].