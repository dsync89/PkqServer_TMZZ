%% @author liuqiang
%% @doc 爱思助手sdk充值处理

-module(mod_pay_i4).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(APPID, "502").
-define(APPKEY, "1c51a606d1264f86b0ede3260973071c").
-define(PUBKEY, "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDHUrvcubwvgDq8uQLAl7ZSlRZ7dfoeGs37+czKpVUg+iXWcSsCW3RUWxt5S0RDyGqWS4gDebRCxvBXhonF0BMtlWylLbETSTSl38RkcHQNuNyXC9eSHiLRHwxFLA/Sxpgr287v7LIk0m7UYSXc138Hx8vpwNlqolKuuZAPkxpWnwIDAQAB").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,Money,QueryList,_,_} ->
            Amount = Money * 10,
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/payi4?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_I4, QueryList),
                            Reply = "success",
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_I4, QueryList),
                            ?ERR("pay i4, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = "fail",
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_I4, QueryList),
                    ?ERR("pay i4, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ -> void
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_post(),
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
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_I4, QueryList),
                    ?ERR("i4 pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8",Reply}), false
            catch _:_ ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_I4, QueryList),
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


%% -------------------------------------- test code ---------------------------------------------
%% test_decode()->
%%     Base64Sign = "cr1W9Amng5h5HrmtwCKOfGpfRdHbaZL/UocVTjmRYxhCZqgTm2c7LZyaHalygbPxckAo0OIvGed+HFjJQmBT3k5Gpmikg2ceYIHPTDMiVKX1L3M50i+5S7zVhl2FOTHRxrovoNFhFCuCWLW87IUX3uGLErQg/o9He6FZB6GIKqaXv3rhWG3fb4u6vYTMZA1Lkg0sRPLZY21rZ88QNVunUnU5vwoIdaViMiJ3EtEURbmbIvWp5XfofcJRfY0UIrPstZL1KmUIkTbgYwygFh4fXYPWBJfXPnsIMB61pP24Xs7ofsLMYpuxC1hCNz90kLAKdOtyhlNf5gh4e+uG9OD0lQ==",
%%     PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAslyj5D604fHe2gmNRYIBRgQ7mWXbzOAt+gLk0NUP1Z02jMdbMbY2Tku9ow7NReZ7M3m2A1xtiVQ+f/pdNF6i2KLxM01sCJb32vAIlzPQEaVjWcSR2TY/6mkWtHqSVE7y0RmzLVi+DfFc7V2BjEQxw7iejlhpbhX9ez66yL4DXGg3IVi0z+zhdf3j/1oVKGCXpNzcyWrj0g88eqlfqqlWrx9UuJHoTDsMkxjpWpXDu2oxvlz/zKC7NrC2eZOSVhgMdkCvUCywq9fGwBgLW3fL9yhMWnXGd85GKtDmU/3fSxO7UFzjSgAh5g4qnAW9Q9MflAZKoH+nukfPWnftyeYYpwIDAQAB\n-----END PUBLIC KEY-----">>,
%%     Sign = base64:decode(Base64Sign),
%% 
%%     PemEntries = public_key:pem_decode(PubKey),
%% 
%%     RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),
%% 
%%     Data = public_key:decrypt_public(Sign, RSAPubKey,['rsa_pkcs1_padding']).
%% 
%% test_decode2()->
%%     Base64Sign = "FAxSDenzdwfKVekGlaSYnXJM3UTk66slADi1TwCm7/FGHKxUR8HMZ7FAEfyUSoP8NS/ABF1+ZiOD\nh1mqSo8Zdc2ZkahtTIE1XIYt3XlF+wN1LXbCJJgKUVExIG97YvzW968Lu4Cg0r2FSwco/5iKoUO8\nLKT+ftn/cLNLcWC20G01RRiTRxSL6J27HBKYXZJ6CihidS/e4/FgV19GnaSuWH1lmnCZ2dqqqEpG\nn/aHyMw15C0UgRB9rOYmIvtlVeEgaeKb/ZBDhNBbLVYIFogKtqmIxwZ4FuOR/mHTqjIGCVppqbBD\nRz7OR6/5S2lFdkxrRXWtQvVXc+FwXuI4o0G8NQ==\n",
%%     PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCtIPPt/Cqkub+kgh78Jt8HVJ+30YDsJqTqIBMtah+uHvdoe/NqGSmvXIo8gTBetolp5eKnU8NT3zp+yrPAn3CAxHKwuwEZUhnGoEFvJJdI6PM4NfCSq+67U97QSTIXO6JyrLKe88Z9O/ZLGE15TZCzW8T2XZa74pFPlV1ao21hTQIDAQAB\n-----END PUBLIC KEY-----">>,
%%     Sign = base64:decode(Base64Sign),
%%     Sign2 = binary_split(Sign),
%% 
%%     PemEntries = public_key:pem_decode(PubKey),
%% 
%%     RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),
%%     
%%     Fun = fun(X) -> public_key:decrypt_public(X, RSAPubKey,['rsa_pkcs1_padding']) end,
%%     
%%     lists:map(Fun, Sign2).