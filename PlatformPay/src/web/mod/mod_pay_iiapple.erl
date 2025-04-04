%% @author caijunjun
%% @doc 爱苹果sdk充值处理


-module(mod_pay_iiapple).
-include("common.hrl").
-include("record.hrl").

-define(SecretKey, "ea4f873a5ddfe3765bb092f39df182ac").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).


handle(Req) ->
%% 	?DEBUG("iiapple request :~p", [Req]),
	QueryString = Req:parse_post(),
	case check_auth(QueryString) of
		true ->
			GameExtend = proplists:get_value("gameExtend", QueryString),
			[_, RoleID2|_] = string:tokens(GameExtend, "."),
			RoleID = erlang:list_to_integer(RoleID2),
			Amount 	 = erlang:trunc(erlang:list_to_float(proplists:get_value("amount", QueryString))) * 10,
			ServerID = (RoleID div 1000000) - 1,
			Server = data_server_list:get(ServerID),
			HttpIP = Server#server.serverIP,
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
			SendQS = mochiweb_util:urlencode(QueryString),
			URL = "http://"++HttpIP++":"++HttpPort++"/payiiapple?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_IIAPPLE, QueryString),
                            Reply = ejson:encode({[{<<"status">>,0}, {<<"transIDO">>, proplists:get_value("transaction", QueryString)}]}),
							platform_tool:return(Req, Reply);
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_IIAPPLE, QueryString),
                            ?ERR("pay iiapple, game server return false, QueryList = ~p~n", [QueryString]),
                            Reply = ejson:encode({[{<<"status">>,-1}, {<<"transIDO">>, 0}]}),
							platform_tool:return(Req, Reply)
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_IIAPPLE, QueryString),
                    ?ERR("pay iiapple, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = ejson:encode({[{<<"status">>,-1}, {<<"transIDO">>, 0}]}),
					platform_tool:return(Req, Reply)
            end;
		false ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_IIAPPLE, QueryString),
			?ERR("iiapple check_order failed. reason:sign wrong,order:~w",[QueryString]),
			Reply = ejson:encode({[{<<"status">>,-1}, {<<"transIDO">>, 0}]}),
			platform_tool:return(Req, Reply)
	end,
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

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

