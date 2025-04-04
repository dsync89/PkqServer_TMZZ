%% @author liuqiang
%% @doc PPS sdk登录处理

-module(mod_login_pps).
-export([handle/1]).
-include("common.hrl").

-define(APPID, "2744").
-define(APPKEY, "74974bf301ff7e270d0e1e6860735f38").
-define(PAYKEY, "KDYG42c36777108aeHc387b102cfHm2744").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_auth(Req) of
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,Uid,DevID,Version} ->
            case db_func:get_pps_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_PPS, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_PPS),
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
%%     ?ERR("debug login pps, QueryList = ~p~n", [QueryList]),
    UserID = proplists:get_value("uid", QueryList),
    Time = proplists:get_value("time", QueryList),
    Sign = proplists:get_value("token", QueryList),
    DevID = proplists:get_value("devid", QueryList),
    Version = proplists:get_value("version", QueryList),
    case (not lists:member(?undefined, [UserID,Time,Sign,DevID,Version])) of
        false ->
            ?ERR("pps login, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            case sign(UserID, Time) =:= Sign of
                false ->
                    ?ERR("pps login, check sign error, QueryList = ~p~n", [QueryList]), false;
                true ->
                    {true,UserID,DevID,Version}
            end
    end.

sign(Uid, Time) ->
    md5(Uid++"&"++Time++"&"++?APPKEY).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").



%% ----------------------------test code----------------------------

