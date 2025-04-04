%% @author admin
%% @doc @todo Add description to pay_mod_vivo.


-module(pay_mod_vivo).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_post(),
	case verify_info(QueryString) of
		true ->
			Status = proplists:get_value("respCode", QueryString),
			case check_payStatus(Status) of
				true ->
					GameRole = proplists:get_value("storeOrder",QueryString),
					Amount = proplists:get_value("orderAmount",QueryString),
					Amount2 = trunc(list_to_float(Amount) * 10) ,
					
					[_ServerID,RoleID] = string:tokens(GameRole,"_"),
					RoleID2 = list_to_integer(RoleID),
					RoleAccID = db_sql:get_role_accid(RoleID2),
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					
					QS = mochiweb_util:urlencode(QueryString),
					pay_gold2(RoleID2,Amount2,QS,md5(QS),27);
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("vivo pay failed order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_vivo(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(QueryString)->
	Info = get_original_encode_list(QueryString),
	Key = "&" ++ util:md5("8EBBE49DD24776FAFB2F3F6BAB6FC48F"),
	Signature = proplists:get_value("signature", QueryString),
	Sign = md5(Info++Key),
	Signature == Sign.

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[H|L] = 
	lists:foldl(fun({A,B},Acc) ->
						case {A,B} of
							{"signature",B} ->
								Acc;
							{"signMethod",B} ->
								Acc;
							{_, ""} ->
								Acc;
							{_, ?undefined} ->
								Acc;
							_ ->
								Acc++"&"++A++"="++B
						end
						end, "", QS2),
	L.

check_payStatus(Status)->
	case Status of
		"0000" ->
			true;
		_ ->
			false
	end.
