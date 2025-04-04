
-module(pay_mod_yw).
-include("common.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    AppID = proplists:get_value("app_id",QueryString),
    Sign = proplists:get_value("sign",QueryString),
    ServerID = proplists:get_value("server_id",QueryString),
    UserID = proplists:get_value("user_id",QueryString),
    SdkOrderNo = proplists:get_value("sdk_order_no",QueryString),
    CT = proplists:get_value("ct",QueryString),
    Money = proplists:get_value("money",QueryString),
    PayResult = proplists:get_value("pay_result",QueryString),
    RoleID = erlang:list_to_integer(proplists:get_value("custom_1",QueryString)),
    case AppID =:= "g00000017" of
        true ->
            LocalSign = sign([AppID,ServerID,UserID,SdkOrderNo,CT,"KdYGGameLDbLYAOWAN"]),
            Amount = trunc(erlang:list_to_float(Money) * 10),
            if Sign == LocalSign andalso PayResult =:= "1" ->
                   RoleAccID = db_sql:get_role_accid(RoleID),
                   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
                   Req:ok({"text/html; charset=utf-8", Reply}),
                   QS = mochiweb_util:urlencode(QueryString),
                   pay_gold2(RoleID,Amount,QS,Sign , ?ACCOUNT_TYPE_YAOWAN);
               true ->
                   Reply = ejson:encode({[{<<"result">>,0}]}),
                   Req:ok({"text/html; charset=utf-8", Reply})
            end;
        false ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_yw(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].
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

sign(StrList) ->
    md5(sign2(StrList)).

sign2([A]) ->
    A;
sign2([A,B]) ->
    A++B;
sign2([]) ->
    "";
sign2([A|T]) ->
    A++sign2(T).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
