%% @author caijunjun
%% @doc 点乐支付 验证


-module(pay_mod_dianjoy).

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
            pay_gold2(RoleID2,Amount2,QS,util:md5(Sign),?ACCOUNT_TYPE_DIANJOY);
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_dianjoy(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].

check_sign(QS) ->
    Content = proplists:get_value("content", QS),
    Sign = proplists:get_value("sign", QS),
    RSAPubKey = get_pubKey(),
    Result = public_key:verify(list_to_binary(Content), 'sha', base64:decode(Sign), RSAPubKey),
    case Result of
        true ->
            {DataList} = ejson:decode(Content),
            AmountV = proplists:get_value(<<"money">>, DataList),
            Amount = erlang:trunc(to_integer(AmountV) / 10),
            OutOrderID = proplists:get_value(<<"outOrderID">>, DataList),
            RoleID = to_integer(OutOrderID),
            {true, Amount, RoleID};
        false ->
            false
    end.

get_pubKey()->                                                   
    PubKey = <<"-----BEGIN PUBLIC KEY-----\nMFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAMizemYNKhdBK9WyQmnsqY3FtV/W8V9JPQGon6ZtDqcw7qAToSE9022W5AeofpgWACa5BzmeqY2qoqARyvy51PMCAwEAAQ==\n-----END PUBLIC KEY-----">>,
    PemEntries = public_key:pem_decode(PubKey),
    public_key:pem_entry_decode(hd(PemEntries)).

decode_data(Base64Data) ->
    Data = base64:decode(Base64Data),
    RSAPubKey = get_pubKey(),
    Data2 = public_key:decrypt_public(Data, RSAPubKey,['rsa_pkcs1_padding']),
    mochiweb_util:parse_qs(binary:bin_to_list(Data2)).

to_integer(V) when erlang:is_list(V) ->
	erlang:list_to_integer(V);
to_integer(V) when erlang:is_binary(V) ->
	erlang:binary_to_integer(V);
to_integer(V) when erlang:is_integer(V) ->
	V.


