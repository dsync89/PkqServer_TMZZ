%% @author caijunjun
%% @doc 点乐支付


-module(mod_pay_dianjoy).

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
            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_dianjoy?" ++ SendQS,
            Response = httpc:request(get, {Url, []}, [], []),   
            case Response of
                {ok, {_,_,RContent}} ->
                    {RContent2} = ejson:decode(RContent),
                    Result = get_value(RContent2, <<"result">>),
                    if Result == 1 ->
                           AccID = get_value(RContent2, <<"accid">>),
                           db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_DIANJOY, QueryString),
                           Reply = "success",
                           Req:ok({"text/html; charset=utf-8", Reply});
                       true ->
                           ?ERR("dianjoy pay failed order:~w,Reason:GameServer return",[QueryString]),
                           db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_DIANJOY, QueryString),
                           Reply = "fail",
                           Req:ok({"text/html; charset=utf-8", Reply})
                    end;
                _ ->
                    ?ERR("dianjoy pay failed order:~w,Reason:GameServer no response",[QueryString]),
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_DIANJOY, QueryString),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;
        _ ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_DIANJOY, QueryString),
            Reply = "fail",
            Req:ok({"text/html; charset=utf-8", Reply}), 
            ?ERR("dianjoy check_order failed. reason:sign wrong,order:~w",[QueryString])
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

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

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

to_integer(V) when erlang:is_list(V) ->
	erlang:list_to_integer(V);
to_integer(V) when erlang:is_binary(V) ->
	erlang:binary_to_integer(V);
to_integer(V) when erlang:is_integer(V) ->
	V.

