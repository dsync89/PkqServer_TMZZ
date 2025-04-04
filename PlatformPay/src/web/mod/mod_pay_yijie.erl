%% @author admin
%% @doc 易接SDK充值处理

-module(mod_pay_yijie).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

%%-define(APPKEY, "9PMY6NYEOTP292V50T61G4KNXLYYR8TB").
-define(APPKEY, "LZ16VO6EZV3L1TRDHAX1UW5QLK4JXQSC").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,Amount,QueryList,_} ->
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_yijie?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YIJIE, QueryList),
                            Reply = "SUCCESS",
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YIJIE, QueryList),
                            ?ERR("pay yijie, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = "FAILED",
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YIJIE, QueryList),
                    ?ERR("pay yijie, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = "FAILED",
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ -> void
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%app=1234567890ABCDEF&cbi=CBI123456&ct=1376578903&fee=100&pt=1376577801&sdk=09CE2B99C22E6D06
%&ssid=123456&st=1&tcd=137657AVDEDFS&uid=1234&ver=1

check_sign(Req) ->
    QueryList = Req:parse_qs(),
    ?DEBUG("debug yijie pay, QueryList = ~p~n", [QueryList]),
    App = proplists:get_value("app", QueryList),
    Cbi = proplists:get_value("cbi", QueryList),
    Ct = proplists:get_value("ct", QueryList),
    Fee = proplists:get_value("fee", QueryList),
    Pt = proplists:get_value("pt", QueryList),
    Sdk = proplists:get_value("sdk", QueryList),
    Ssid = proplists:get_value("ssid", QueryList),
    St = proplists:get_value("st", QueryList),
    Tcd = proplists:get_value("tcd", QueryList),
    Uid = proplists:get_value("uid", QueryList),
    Ver = proplists:get_value("ver", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [App,Cbi,Ct,Fee,Pt,Sdk,Ssid,St,Tcd,Uid,Ver,Sign])) of
        false ->
            ?ERR("yijie pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "FAILED",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            case sign(App,Cbi,Ct,Fee,Pt,Sdk,Ssid,St,Tcd,Uid,Ver) =:= Sign andalso St =:= "1" of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_YIJIE, QueryList),
                    ?ERR("yijie pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "FAILED",
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
                    [SServerID,SRoleID] = string:tokens(Cbi, "|"),
                    ServerID = erlang:list_to_integer(SServerID),
                    RoleID = erlang:list_to_integer(SRoleID),
                    Amount2 = erlang:round(erlang:list_to_integer(Fee) * 2),
                    {true,ServerID,RoleID,Amount2,QueryList,Sign}
            end
    end.

sign(App,Cbi,Ct,Fee,Pt,Sdk,Ssid,St,Tcd,Uid,Ver) ->
    OriStr = "app="++App++"&cbi="++Cbi++"&ct="++Ct++"&fee="++Fee++"&pt="++Pt++"&sdk="++Sdk
    ++"&ssid="++Ssid++"&st="++St++"&tcd="++Tcd++"&uid="++Uid++"&ver="++Ver++?APPKEY,
    ?DEBUG("OriStr = ~s~n", [OriStr]),
    md5(OriStr).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


