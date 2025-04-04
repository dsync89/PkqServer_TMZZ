%% @author liuqiang
%% @doc 金山sdk充值处理


-module(mod_pay_ks).
-include("common.hrl").
-include("record.hrl").

-export([handle/1]).

-define(GID, "104").
-define(PFID, "104").
-define(PFKEY, "IhOwSg1K9vMU8VpgHfr6sTPXQs5ywaw6").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req)->
    case check_sign(Req) of
        {true,RoleID,ServerID,Money,Oid,QueryString} ->
            Amount = Money div 10,
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryString),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/payks?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} =    try ejson:decode(Content) catch
                                        _:_ -> {json_false}
                                    end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_KS, QueryString),
                            Reply = pack_reply(1, "pay ks ok", Oid),
                            Req:ok({"text/html; charset=utf-8",Reply});
                        2 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(2, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_KS, QueryString),
                            Reply = pack_reply(0, "pay receipt duplicate", Oid),
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_KS, QueryString),
                            ?ERR("pay ks, game server return false, QueryString = ~p~n", [QueryString]),
                            Reply = pack_reply(-1, "game server return false", Oid),
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_KS, QueryString),
                    ?ERR("pay ks, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = pack_reply(-1, "game server doesn't response", Oid),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ -> void
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryString = Req:parse_qs(),
%%     ?ERR("debug ks pay, QueryString = ~p~n", [QueryString]),
    Pfid = proplists:get_value("pfid", QueryString),
    Gid = proplists:get_value("gid", QueryString),
    Sid = proplists:get_value("sid", QueryString),
    Uid = proplists:get_value("uid", QueryString),
    Oid = proplists:get_value("oid", QueryString),
    Payway = proplists:get_value("payway", QueryString),
    Gold = proplists:get_value("gold", QueryString),
    Money = proplists:get_value("money", QueryString),
    Paytime = proplists:get_value("paytime", QueryString),
    Cpparam = proplists:get_value("cpparam", QueryString),
    Time = proplists:get_value("time", QueryString),
    Sign = proplists:get_value("sign", QueryString),
    case (not lists:member(?undefined, [Pfid,Gid,Sid,Uid,Oid,Payway,Gold,Money,Paytime,Cpparam,Time,Sign])) of
        false ->
            ?ERR("ks pay, query string error, QueryString = ~p~n", [QueryString]),
            Reply = pack_reply(-1, "query string error", Oid),
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            {_,MySign} = sign(Pfid, Gid, Sid, Uid, Oid, Payway, Gold, Money, Paytime, Cpparam, Time),
            case MySign =:= Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_KS, QueryString),
                    ?ERR("ks pay, check sign error, MySign = ~p~n", [MySign]),
                    Reply = pack_reply(-1, "check sign error", Oid),
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
                    [ServerID, RoleID] = string:tokens(Cpparam, "{, }"),
                    {true,erlang:list_to_integer(RoleID),erlang:list_to_integer(ServerID),erlang:list_to_integer(Money),Oid,QueryString}
            end
    end.

sign(Pfid, Gid, Sid, Uid, Oid, Payway, Gold, Money, Paytime, Cpparam, Time) ->
    OriStr = mochiweb_util:urlencode([{cpparam,Cpparam},{gid,Gid},{gold,Gold},{money,Money},{oid,Oid}
                                     ,{paytime,Paytime},{payway,Payway},{pfid,Pfid},{sid,Sid},{time,Time},{uid,Uid}]),
    {OriStr,md5(OriStr ++ ?PFKEY)}.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- erlang:binary_to_list(erlang:md5(S))]).


pack_reply(Code, Msg, Oid) ->
    ejson:encode({[
                   {<<"code">>,Code}
                  ,{<<"msg">>,erlang:list_to_binary(Msg)}
                  ,{<<"data">>,{[{<<"oid">>,erlang:list_to_binary(Oid)}]}}
                  ]}).
