%% @author caijunjun
%% @doc 爱苹果sdk充值处理.


-module(pay_mod_iiapple).

-include("common.hrl").

-define(SecretKey, "ea4f873a5ddfe3765bb092f39df182ac").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	?DEBUG("iiapple pay request :~p", [Req]),
	QueryString = Req:parse_qs(),
	case check_auth(QueryString) of
		true ->
			Amount = trunc(list_to_float(proplists:get_value("amount", QueryString))) * 10,
			Sign   = proplists:get_value("_sign", QueryString),	
			QS 	   = mochiweb_util:urlencode(QueryString),
			GameExtend = proplists:get_value("gameExtend", QueryString),
			[_, RoleID2|_] = string:tokens(GameExtend, "."),
			RoleID = erlang:list_to_integer(RoleID2),
            AccID = db_sql:get_role_accid(RoleID),
            
			pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_IIAPPLE),
            AccID = db_sql:get_role_accid(RoleID),
            Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        false ->
			?ERR("iiapple pay, query list error, QueryList = ~p~n", [QueryString]),
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_iiapple(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).


check_auth(QS) ->
	Amount 		= proplists:get_value("amount", QS),
	CardPoint	= proplists:get_value("cardPoint", QS),
	Currency	= proplists:get_value("currency", QS),
	GameExtend  = proplists:get_value("gameExtend", QS),
	GameUserId  = proplists:get_value("gameUserId", QS),
	PayType  	= proplists:get_value("payType", QS),
	Platform  	= proplists:get_value("platform", QS),
	ServerNo  	= proplists:get_value("serverNo", QS),
	Status  	= proplists:get_value("status", QS),
	Transaction = proplists:get_value("transaction", QS),
	TransTime 	= proplists:get_value("transactionTime", QS),
	UserId		= proplists:get_value("userId", QS),
	Sign  		= proplists:get_value("_sign", QS),	

	case lists:member(?undefined, [Amount, CardPoint, Currency, GameExtend,
								   GameUserId, PayType, Platform, ServerNo,
								   Status, Transaction, TransTime, UserId]) of
		false ->
			Sign =:= md5(sign(["amount=", Amount, "&cardPoint=", CardPoint,
								"&currency=", Currency, "&gameExtend=", GameExtend,
								"&gameUserId=", GameUserId, "&payType=", PayType,
								"&platform=", Platform, "&serverNo=", ServerNo,
								"&status=", Status, "&transaction=", Transaction,
								"&transactionTime=", TransTime, "&userId=", UserId]) ++ ?SecretKey);
		true ->
			false
	end.

sign(StrList) ->
	md5(sign2(StrList)).

sign2([A]) ->
	A;
sign2([A,B]) ->
	A ++ B;
sign2([]) ->
	"";
sign2([A|T]) ->
	A ++ sign2(T).
		
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
	



%% ====================================================================
%% Internal functions
%% ====================================================================


