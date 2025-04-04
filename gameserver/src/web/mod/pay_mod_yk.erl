%% @author liuqiang
%% @doc 优酷sdk充值处理

-module(pay_mod_yk).
-export([pay_gold/1]).
-include("common.hrl").

-define(APPID, "1115").
-define(APPKEY, "22793022f478dcb8").
-define(APPSECRET, "bcab99e72872ce076783803034b0bb6f").
-define(PAYKEY, "cea265b9c20fcfec8918acadbd6aec57").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,_,RoleID,Money,QueryList,Sign} ->
            Amount = Money div 10,
            QS = mochiweb_util:urlencode(QueryList),
            pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_YK),
            AccID = db_sql:get_role_accid(RoleID),
            Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_yk(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
%%     ?ERR("debug yk pay, QueryList = ~p~n", [QueryList]),
    ApporderID = proplists:get_value("apporderID", QueryList),
    Price = proplists:get_value("price", QueryList),
    Uid = proplists:get_value("uid", QueryList),
%%     Result = proplists:get_value("result", QueryList),
    Sign = proplists:get_value("sign", QueryList),
%%     SuccessAmount = proplists:get_value("success_amount", QueryList),
    case (not lists:member(?undefined, [ApporderID,Uid,Price,Sign])) of
        false ->
            ?ERR("yk pay, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            case sign(ApporderID, Price, Uid) =:= Sign of
                false ->
                    ?ERR("yk pay, check sign error, QueryList = ~p~n", [QueryList]), false;
                true ->
                    [ServerID, RoleID, _] = string:tokens(ApporderID, "."),
                    {true,list_to_integer(ServerID),list_to_integer(RoleID),list_to_integer(Price),QueryList,Sign}
            end
    end.

sign(ApporderID, Price, Uid) ->
    OriStr = get_callback()++"?"++mochiweb_util:urlencode([{apporderID,ApporderID},{price,Price},{uid,Uid}]),
    bin_to_hexstr(crypto:hmac('md5', list_to_binary(?PAYKEY), list_to_binary(OriStr))).

get_callback() ->
    "http://120.132.76.41/payyk".

bin_to_hexstr(Bin) ->
   lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).


%% -------------------------------------- test code ---------------------------------------------
