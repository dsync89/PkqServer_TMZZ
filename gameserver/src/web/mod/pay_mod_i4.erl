%% @author liuqiang
%% @doc 爱思助手sdk充值处理

-module(pay_mod_i4).
-export([pay_gold/1]).
-include("common.hrl").

-define(APPID, "502").
-define(APPKEY, "1c51a606d1264f86b0ede3260973071c").
-define(PUBKEY, "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDHUrvcubwvgDq8uQLAl7ZSlRZ7dfoeGs37+czKpVUg+iXWcSsCW3RUWxt5S0RDyGqWS4gDebRCxvBXhonF0BMtlWylLbETSTSl38RkcHQNuNyXC9eSHiLRHwxFLA/Sxpgr287v7LIk0m7UYSXc138Hx8vpwNlqolKuuZAPkxpWnwIDAQAB").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,_,RoleID,_,_,_,"1"} ->
            AccID = db_sql:get_role_accid(RoleID),
            Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,_,RoleID,Money,QueryList,Sign,"0"} ->
            Amount = Money * 10,
            QS = mochiweb_util:urlencode(QueryList),
            pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_I4),
            AccID = db_sql:get_role_accid(RoleID),
            Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_i4(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
%%     ?ERR("debug i4 pay, QueryList = ~p~n", [QueryList]),
    Account = proplists:get_value("account", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    AppID = proplists:get_value("app_id", QueryList),
    Billno = proplists:get_value("billno", QueryList),
    OrderID = proplists:get_value("order_id", QueryList),
    Status = proplists:get_value("status", QueryList),
    Role = proplists:get_value("role", QueryList),
    Zone = proplists:get_value("zone", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [Account,Amount,AppID,Billno,OrderID,Status,Role,Zone,Sign])) of
        false ->
            ?ERR("i4 pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "fail",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            try verify_sign(QueryList, Sign) of
                true ->
                    [ServerID, RoleID, _] = string:tokens(Billno, "."),
                    {true,list_to_integer(ServerID),list_to_integer(RoleID),list_to_integer(Amount),QueryList,Sign,Status};
                false ->
                    ?ERR("i4 pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8",Reply}), false
            catch _:_ ->
                    ?ERR("i4 pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8",Reply}), false
            end
    end.

verify_sign(QueryList, Sign) ->
    QueryStr = mochiweb_util:urlencode(lists:keydelete("sign", 1, QueryList)),
    DecodeStr = rsa_decrypt(base64:decode(Sign)),
    QsList1 = lists:keysort(1, mochiweb_util:parse_qs(QueryStr)),
    QsList2 = lists:keysort(1, mochiweb_util:parse_qs(DecodeStr)),
    QsList1 =:= QsList2.

get_rsaPubKey()->
    PubKey = erlang:list_to_binary("-----BEGIN PUBLIC KEY-----\n"++?PUBKEY++"\n-----END PUBLIC KEY-----"),
    PemEntries = public_key:pem_decode(PubKey),
    public_key:pem_entry_decode(erlang:hd(PemEntries)).

rsa_decrypt(Data) ->
    RSAPubKey = get_rsaPubKey(),
    lists:append(lists:map(
        fun(X) ->
            erlang:binary_to_list(
                public_key:decrypt_public(X, RSAPubKey, ['rsa_pkcs1_padding']))
        end, binary_split(Data))).

binary_split(Data) ->
    binary_split(Data, []).
binary_split(<<>>, Acc) ->
    lists:reverse(Acc);
binary_split(<<HD:128/binary, Rest/binary>>, Acc) ->
    binary_split(Rest, [HD|Acc]).
