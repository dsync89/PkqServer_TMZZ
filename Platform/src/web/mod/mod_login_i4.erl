%% @author liuqiang
%% @doc 爱思助手sdk登录处理

-module(mod_login_i4).
-export([handle/1]).
-include("common.hrl").

-define(APPID, "502").
-define(APPKEY, "1c51a606d1264f86b0ede3260973071c").
-define(PUBKEY, "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCbQB0SdFEX1nHjgRVZ4vMJbd67Z2Tg97vdXBVv7Q+KNdRIUtSUJUu635xugNlnvQteq2ZMgcQMnvYa+G1+l/2jen/V+EU55oIyVIP+S1ZP3OUIblGxs4+pzP40d61jK8Y4lmBy6j1LPoLEI3OXMO8s3XAcdDkpKe3WuuiEoOvXnQIDAQAB").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_auth(Req) of
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,Uid,DevID,Version} ->
            case db_func:get_i4_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_I4, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_I4),
                    Reply = ejson:encode({[
                                            {<<"result">>,0},
                                            {<<"accid">>,Accid},
                                            {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                            {<<"valid_time">>,list_to_binary(ValidTime)},
                                            {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                            {<<"server_list">>,ServerList}
                                        ]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Req) ->
    QueryString = Req:parse_qs(),
%%     ?ERR("debug login i4, QueryString = ~p~n", [QueryString]),
    Token = proplists:get_value("sessionID", QueryString),
%%     ?ERR("debug login i4, Token = ~p~n", [Token]),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    case (not lists:member(?undefined, [Token,DevID,Version])) of
        false ->
            ?ERR("i4 login, query string error, QueryString = ~p~n", [QueryString]), false;
        true ->
            URL = "https://pay.i4.cn/member_third.action?token="++Token,
%%             ?ERR("debug login i4, URL = ~p~n", [URL]),
            case httpc:request(get, {URL,[]}, [], []) of
                {ok,{_,_,Ret}} ->
                    case unpack_response(Ret) of
                        {true,0,UserID} ->
                            {true,UserID,DevID,Version};
                        _ ->
                            ?ERR("i4 login, check ret error,Ret = ~p~n", [Ret]), false
                    end;
                _ ->
                    ?ERR("i4 login, url request error, URL = ~p~n", [URL]), false
            end
    end.

unpack_response(Ret) ->
    {Ret2} = try ejson:decode(Ret) catch
                 _:_ -> {[]}
             end,
    Status = proplists:get_value(<<"status">>, Ret2),
    UserID = proplists:get_value(<<"userid">>, Ret2),
    case (not lists:member(?undefined, [Status,UserID])) of
        false ->
            {false,Ret2};
        true ->
            {true,Status,erlang:integer_to_list(UserID)}
    end.

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").



%% ----------------------------test code----------------------------
