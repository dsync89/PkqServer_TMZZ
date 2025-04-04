%% @author caijunjun
%% @doc 好接入充值


-module(pay_mod_hjr).

-include("common.hrl").

-define(SECURITYKEY, "tfXF66h0Jzvph1Zo6VqQ9f9Ze2YZ5OMS").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    Sign = proplists:get_value("sign", QueryString),
    ?DEBUG("handle hjr request ~p", [Req]),
    case verify_sign(QueryString) of
        true ->
            Extend = proplists:get_value("extend", QueryString),
            RoleID2 = erlang:list_to_integer(Extend),
            Amount2 = proplists:get_value("amount", QueryString),
            Amount = trunc(to_num(Amount2) * 10),
            
            RoleAccID = db_sql:get_role_accid(RoleID2),
            Req:ok({"text/html; charset=utf-8", ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]})}),
            QS = mochiweb_util:urlencode(QueryString),
            pay_gold2(RoleID2,Amount,QS,Sign,?ACCOUNT_TYPE_HJR);
        false ->
            Req:ok({"text/html; charset=utf-8", ejson:encode({[{<<"result">>,0}]})}),
            ?ERR("pay hjr pay failed, QueryString:~w",[QueryString])
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_hjr(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).


verify_sign(QS) ->
    Sign = proplists:get_value("sign", QS),
    QueryStr = mochiweb_util:urlencode(lists:keysort(1, lists:keydelete("sign", 1, QS))),

    Sign == util:md5(util:md5(QueryStr) ++ ?SECURITYKEY).

to_num(List) ->
    case catch erlang:list_to_float(List) of
        Num when erlang:is_float(Num) ->
            Num;
        _ ->
            erlang:list_to_integer(List)
    end.
