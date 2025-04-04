%% @author liuqiang
%% @doc 优酷sdk登录处理

-module(mod_login_yk).
-export([handle/1]).
-include("common.hrl").

-define(APPID, "1115").
-define(APPKEY, "22793022f478dcb8").
-define(APPSECRET, "bcab99e72872ce076783803034b0bb6f").
-define(PAYKEY, "cea265b9c20fcfec8918acadbd6aec57").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_auth(Req) of
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,Uid,DevID,Version} ->
%%             ?ERR("debug login i4, Uid = ~p~n", [Uid]),
            case db_func:get_yk_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_YK, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_YK),
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
    QueryList = Req:parse_qs(),
%%     ?ERR("debug login yk, QueryList = ~p~n", [QueryList]),
    SessionID = proplists:get_value("sessionID", QueryList),
%%     ?ERR("debug login yk, SessionID = ~p~n", [SessionID]),
    DevID = proplists:get_value("devid", QueryList),
    Version = proplists:get_value("version", QueryList),
    case (not lists:member(?undefined, [SessionID,DevID,Version])) of
        false ->
            ?ERR("yk login, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            Sign = sign(SessionID),
            URL = "http://sdk.api.gamex.mobile.youku.com/game/user/infomation",
            CType = "application/x-www-form-urlencoded",
            Body = mochiweb_util:urlencode([{appkey,?APPKEY},{sessionid,SessionID},{sign,Sign}]),
            ?DEBUG("debug yk login, Body = ~p~n", [Body]),
            case httpc:request(post, {URL,[],CType,Body}, [], []) of
                {ok,{_,_,Ret}} ->
                    case unpack_response(Ret) of
                        {true,<<"success">>,UserID} ->
                            {true,UserID,DevID,Version};
                        _ ->
                            ?ERR("yk login, check ret error,Ret = ~p~n", [Ret]), false
                    end;
                _ ->
                    ?ERR("yk login, url request error, URL = ~p~n", [URL]), false
            end
    end.

sign(SessionID) ->
    OriStr = mochiweb_util:urlencode([{appkey,?APPKEY},{sessionid,SessionID}]),
    bin_to_hexstr(crypto:hmac('md5', list_to_binary(?PAYKEY), list_to_binary(OriStr))).

bin_to_hexstr(Bin) ->
   lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

unpack_response(Ret) ->
    {Ret2} = try ejson:decode(Ret) catch
                 _:_ -> {[]}
             end,
    Status = proplists:get_value(<<"status">>, Ret2),
    UserID = proplists:get_value(<<"uid">>, Ret2),
    case (not lists:member(?undefined, [Status,UserID])) of
        false ->
            {false,Ret2};
        true ->
            {true,Status,erlang:integer_to_list(UserID)}
    end.

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").



%% ----------------------------test code----------------------------
test_ss(SessionID) ->
    Sign = sign(SessionID),
    URL = "http://sdk.api.gamex.mobile.youku.com/game/user/infomation",
    CType = "application/x-www-form-urlencoded",
    Body = mochiweb_util:urlencode([{appkey,?APPKEY},{sessionid,SessionID},{sign,Sign}]),
    httpc:request(post, {URL,[],CType,Body}, [], []).
