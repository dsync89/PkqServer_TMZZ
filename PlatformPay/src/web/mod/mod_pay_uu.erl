%% @author liuqiang
%% @doc 悠悠村sdk支付处理
%% Created 2014/6/11


-module(mod_pay_uu).
-include("common.hrl").
-include("record.hrl").

-export([handle/1]).

%% -define(APPKEY, "T3gwpRvXehfO9ErS9wzLKsBOQHqlPMc6").
-define(UUKEY, "3UYUQr").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req)->
    QueryString = Req:parse_post(),
%%    ?ERR("debug uu pay, the query_string is: ~p~n", [QueryString]),
    case check_auth(QueryString) of
        {query_string_error,QueryString} ->
            ?ERR("uu pay query_string_error, the query_string is: ~p~n", [QueryString]),
            Reply = "88082",
            Req:ok({"text/html; charset=utf-8", Reply});
        {callback_rsp_error,CallbackRsp} ->
            ?ERR("uu pay callback_rsp_error, the callback_rsp is: ~p~n", [CallbackRsp]),
            Reply = "88082",
            Req:ok({"text/html; charset=utf-8", Reply});
        {callback_user_error,CallbackUser} ->
            ?ERR("uu pay callback_user_error, the callback_user is: ~p~n", [CallbackUser]),
            Reply = "88082",
            Req:ok({"text/html; charset=utf-8", Reply});
        {sign_error,Sign} ->
            ?ERR("uu pay sign_error, the sign is: ~p~n", [Sign]),
            Reply = "200007",
            Req:ok({"text/html; charset=utf-8", Reply});
        {true,RoleID,Amount,_Receipt,_Sign} ->
            ServerID = (RoleID div 1000000) - 1,
            Server = data_server_list:get(ServerID),
            HttpPort = integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryString),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/payuu?"++SendQS,
            Response = httpc:request(get, {URL,[]}, [], []),
            case Response of
                {ok, {_,_,Content}} ->
                    {Content2}  =   try ejson:decode(Content) catch
                                        _:_ -> {[json_false]}
                                    end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    if Result =:= 1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_UU, QueryString),
                            Reply = "1",
                            Req:ok({"text/html; charset=utf-8", Reply});
                        true ->
                            ?ERR("uu check_order failed. reason:game server return false,order:~w",[QueryString]),
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_UU, QueryString),
                            Reply = "777777",
                            Req:ok({"text/html; charset=utf-8", Reply})
                    end;
                _ ->
                    ?ERR("uu pay game_server don't respond~n",[]),
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_UU, QueryString),
                    Reply = "200007",
                    Req:ok({"text/html; charset=utf-8", Reply})
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(QueryString) ->
    ECallbackRsp = proplists:get_value("callback_rsp", QueryString),
    ECallbackUser = proplists:get_value("callback_user", QueryString),
    ECallbackAppkey = proplists:get_value("callback_appkey", QueryString),
    case not lists:member(?undefined, [ECallbackRsp,ECallbackUser,ECallbackAppkey]) of
        false ->
            {query_string_error,QueryString};
        true ->
            CallbackRsp = des_cbc_decrypt(ECallbackRsp),
%%            ?ERR("debug uu pay, the callback_rsp is: ~p~n", [CallbackRsp]),
            CRList = split_callback_rsp(CallbackRsp),
%%            ?ERR("debug uu pay, the cr_list is: ~p~n", [CRList]),
            AppKey = proplists:get_value("app_key", CRList),
            TxnSeq = proplists:get_value("txn_seq", CRList),
            OrderID = proplists:get_value("order_id", CRList),
            RspCode = proplists:get_value("rsp_code", CRList),
            TxnTime = proplists:get_value("txn_time", CRList),
            ActualTxnAmt = proplists:get_value("actual_txn_amt", CRList),
            TimeStamp = proplists:get_value("time_stamp", CRList),
            SignMsg = proplists:get_value("signMsg", CRList),
            case not lists:member(?undefined, [AppKey,TxnSeq,OrderID,RspCode,TxnTime,ActualTxnAmt,TimeStamp,SignMsg]) of
                false ->
                    {callback_rsp_error,CallbackRsp};
                true ->
                    CallbackUser = des_cbc_decrypt(ECallbackUser),
%%                    ?ERR("debug uu pay, the callback_user is: ~p~n", [CallbackUser]),
                    CUList = split_callback_user(CallbackUser),
                    RoleID = proplists:get_value("roleid", CUList),
                    case not lists:member(?undefined, [RoleID]) of
                        false ->
                            {callback_user_error,CallbackUser};
                        true ->
                            Key = ?UUKEY,
                            Sign = sign(AppKey,TxnSeq,OrderID,RspCode,TxnTime,ActualTxnAmt,TimeStamp,Key),
                            case SignMsg =:= Sign of
                                false ->
                                    {sign_error,Sign};
                                true ->
                                    {true,list_to_integer(RoleID),(list_to_integer(ActualTxnAmt) div 10),mochiweb_util:urlencode(QueryString),Sign}
                            end
                    end
            end
    end.


split_callback_rsp(Str) ->
    WordList = string:tokens(Str, "&"),
    WordToTuple = fun(Word) -> list_to_tuple(string:tokens(Word, "=")) end,
    lists:map(WordToTuple, WordList).

split_callback_user(Str) ->
    WordList = string:tokens(Str, "#"),
    WordToTuple = fun(Word) -> list_to_tuple(string:tokens(Word, "=")) end,
    lists:map(WordToTuple, WordList).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0B",[N]) || N <- binary_to_list(erlang:md5(S))]).

sign(AppKey,TxnSeq,OrderID,RspCode,TxnTime,ActualTxnAmt,TimeStamp,Key) ->
    Text = "app_key="++AppKey++"&txn_seq="++TxnSeq++"&order_id="++OrderID++"&rsp_code="++RspCode
            ++"&txn_time="++TxnTime++"&actual_txn_amt="++ActualTxnAmt++"&time_stamp="++TimeStamp++"&key="++Key,
    md5(Text).


%% ----------------------------- DES 相关 -----------------------------------

-define(DESKEY, "G3jFuJRt"). % 正式用
%% -define(DESKEY, "2SoXIhFB"). % 测试用
-define(BLOCKSIZE, 8). % DES加解密块大小

%% DES cbc 加密
des_cbc_encrypt(Text0) ->
    Key = list_to_binary(?DESKEY),
    IVec = list_to_binary(?DESKEY),
    Text = list_to_binary(pkcs5Pad(Text0)),
    Cipher = crypto:des_cbc_encrypt(Key, IVec, Text),
    string:to_upper(bin_to_hexstr(Cipher)).

pkcs5Pad(Str) ->
    Pad = ?BLOCKSIZE - (erlang:length(Str) rem ?BLOCKSIZE),
    lists:append(Str, lists:duplicate(Pad, Pad)).

%% DES cbc 解密
des_cbc_decrypt(Cipher0) ->
    Key = list_to_binary(?DESKEY),
    IVec = list_to_binary(?DESKEY),
    Cipher = hexstr_to_bin(string:to_lower(Cipher0)),
    Text = crypto:des_cbc_decrypt(Key, IVec, Cipher),
    pkcs5Unpad(binary_to_list(Text)).

pkcs5Unpad(Str) ->
    Pad = lists:last(Str),
    lists:filter(fun(X) -> X =/= Pad end, Str).


bin_to_hexstr(Bin) ->
   lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
 
hexstr_to_bin(S) ->
   hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
   list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
   {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
   hexstr_to_bin(T, [V | Acc]).

%% ----------------------------- DES 相关 -----------------------------------