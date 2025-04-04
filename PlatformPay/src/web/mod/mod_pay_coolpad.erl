%% @author caijunjun
%% @doc 酷派支付


-module(mod_pay_coolpad).

-include("common.hrl").
-include("record.hrl").


-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
handle(Req) ->
    QueryString = Req:parse_post(),
    case check_sign(QueryString) of
        {true, Amount, RoleID} ->
            ServerID2 = (RoleID div 1000000) - 1,
            Server = data_server_list:get(ServerID2),
            HttpPort = integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryString),
            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_coolpad?" ++ SendQS,
            Response = httpc:request(get, {Url, []}, [], []),   
            case Response of
                {ok, {_,_,RContent}} ->
                    {RContent2} = ejson:decode(RContent),
                    Result = get_value(RContent2, <<"result">>),
                    if Result == 1 ->
                           AccID = get_value(RContent2, <<"accid">>),
                           db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_COOLPAD, QueryString),
                           Reply = "SUCCESS",
                           Req:ok({"text/html; charset=utf-8", Reply});
                       true ->
                           ?ERR("coolpad pay failed order:~p,Reason:GameServer return",[QueryString]),
                           db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_COOLPAD, QueryString),
                           Reply = "FAILURE",
                           Req:ok({"text/html; charset=utf-8", Reply})
                    end;
                _ ->
                    ?ERR("coolpad pay failed order:~w,Reason:GameServer no response",[QueryString]),
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_COOLPAD, QueryString),
                    Reply = "FAILURE",
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;
        _ ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_COOLPAD, QueryString),
            Reply = "FAILURE",
            Req:ok({"text/html; charset=utf-8", Reply}), 
            ?ERR("coolpad check_order failed. reason:sign wrong,order:~p",[QueryString])
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

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
            OutOrderID = proplists:get_value(<<"cpprivate">>, DataList),
            RoleID = erlang:binary_to_integer(OutOrderID),
            {true, Amount, RoleID};
        false ->
            false
    end.

get_pubKey()->                                                   
    PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDMeZKk+cOozXuW+XSN9uj9O8UtDFq2o2k52xt57tlsUtpUHLeiwc4eBH0fgO5bEVETzpUHipsGNDqhxLgiZM60z2T/z+gG784GSzxF/CcrY/Ha+wV+tjIj3f6+CiCUKIwL/hjwCfJxnT24Tl/WQ6ISU19ee0RdnUreumvvq3uSPQIDAQAB\n-----END PUBLIC KEY-----">>,
    PemEntries = public_key:pem_decode(PubKey),
    public_key:pem_entry_decode(hd(PemEntries)).

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

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






