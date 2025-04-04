%% @author admin
%% @doc @todo Add description to pay_mod_91.


-module(pay_mod_bd).
-include("common.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    AppID = proplists:get_value("AppID",QueryString),
    Sign = proplists:get_value("Sign",QueryString),
    OrderSerial = proplists:get_value("OrderSerial",QueryString),
    CooperatorOrderSerial = proplists:get_value("CooperatorOrderSerial",QueryString),
    Content = proplists:get_value("Content",QueryString),
    case AppID =:= "5009295" of
        true ->
            LocalSign = sign([AppID,OrderSerial,CooperatorOrderSerial,Content,"ZuEK6k8H6fPnG3E6GO6rDa0wBXjVM2FI"]),
            {Content2} = ejson:decode(base64:decode(Content)),
            Amount = trunc(erlang:list_to_float(erlang:binary_to_list(get_value(Content2, <<"OrderMoney">>))) * 10),
            OrderStatus = get_value(Content2, <<"OrderStatus">>),
            ExtInfo = get_value(Content2, <<"ExtInfo">>),
            RoleID = erlang:list_to_integer(erlang:binary_to_list(ExtInfo)),
            if Sign == LocalSign andalso OrderStatus =:= 1 ->
                   RoleAccID = db_sql:get_role_accid(RoleID),
                   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
                   Req:ok({"text/html; charset=utf-8", Reply}),
                   QS = mochiweb_util:urlencode(QueryString),
                   pay_gold2(RoleID,Amount,QS,Sign , ?ACCOUNT_TYPE_BAIDU);
               true ->
                   Reply = ejson:encode({[{<<"result">>,0}]}),
                   Req:ok({"text/html; charset=utf-8", Reply})
            end;
        false ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_bd(RoleID,Amount,Req,Sign,SrcType);
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
