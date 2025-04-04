%% @author caijunjun
%% @doc 酷派支付验证


-module(pay_mod_coolpad).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case check_sign(QueryString) of
        {true, Amount2, RoleID2} ->
            RoleAccID = db_sql:get_role_accid(RoleID2),
            Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            
            QS = mochiweb_util:urlencode(QueryString),
            Sign = proplists:get_value("sign", QueryString),
            pay_gold2(RoleID2,Amount2,QS,Sign,?ACCOUNT_TYPE_COOLPAD);
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_coolpad(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].

check_sign(QS) ->
    TransData = proplists:get_value("transdata", QS),
    Sign = proplists:get_value("sign", QS),
    RSAPubKey = get_pubKey(),
    Result = public_key:verify(list_to_binary(TransData), 'md5', base64:decode(Sign), RSAPubKey),
    case Result of
        true ->
            {DataList} = ejson:decode(TransData),
            AmountV = proplists:get_value(<<"money">>, DataList),
            Amount = erlang:trunc(to_num(AmountV) * 10),
            RoleID2 = proplists:get_value(<<"cpprivate">>, DataList),
            RoleID = erlang:binary_to_integer(RoleID2),
            {true, Amount, RoleID};
        false ->
            false
    end.

get_pubKey()->                                                   
    PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDMeZKk+cOozXuW+XSN9uj9O8UtDFq2o2k52xt57tlsUtpUHLeiwc4eBH0fgO5bEVETzpUHipsGNDqhxLgiZM60z2T/z+gG784GSzxF/CcrY/Ha+wV+tjIj3f6+CiCUKIwL/hjwCfJxnT24Tl/WQ6ISU19ee0RdnUreumvvq3uSPQIDAQAB\n-----END PUBLIC KEY-----">>,
    PemEntries = public_key:pem_decode(PubKey),
    public_key:pem_entry_decode(hd(PemEntries)).


to_num(Amount) when erlang:is_number(Amount) ->
    Amount;
to_num(Amount) when erlang:is_binary(Amount) ->
    case catch erlang:binary_to_float(Amount) of
        {'EXIT', _} ->
            erlang:binary_to_integer(Amount);
        Num ->
            Num
    end;
to_num(Amount) when erlang:is_list(Amount) ->
    case catch erlang:list_to_float(Amount) of
        {'EXIT', _} ->
            erlang:list_to_integer(Amount);
        Num ->
            Num
    end.


