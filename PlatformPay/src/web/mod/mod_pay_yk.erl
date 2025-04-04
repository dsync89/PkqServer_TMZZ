%% @author liuqiang
%% @doc 优酷sdk充值处理

-module(mod_pay_yk).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(APPID, "1115").
-define(APPKEY, "22793022f478dcb8").
-define(APPSECRET, "bcab99e72872ce076783803034b0bb6f").
-define(PAYKEY, "cea265b9c20fcfec8918acadbd6aec57").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,Money,QueryList,_} ->
            Amount = Money div 10,
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/payyk?"++SendQS,
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YK, QueryList),
                            Reply = pack_reply("success", "success"),
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YK, QueryList),
                            ?ERR("pay yk, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = pack_reply("failed", "internal error"),
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YK, QueryList),
                    ?ERR("pay yk, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = pack_reply("failed", "internal error"),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ -> void
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_post(),
%%     ?ERR("debug yk pay, QueryList = ~p~n", [QueryList]),
    ApporderID = proplists:get_value("apporderID", QueryList),
    Price = proplists:get_value("price", QueryList),
    Uid = proplists:get_value("uid", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [ApporderID,Uid,Price,Sign])) of
        false ->
            ?ERR("yk pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = pack_reply("failed", "query list error"),
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            case sign(ApporderID, Price, Uid) =:= Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_YK, QueryList),
                    ?ERR("yk pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = pack_reply("failed", "check sign error"),
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
                    [ServerID, RoleID, _] = string:tokens(ApporderID, "."),
                    {true,list_to_integer(ServerID),list_to_integer(RoleID),list_to_integer(Price),QueryList,Sign}
            end
    end.

pack_reply(Status, Desc) ->
    ejson:encode({[
                   {<<"status">>,erlang:list_to_binary(Status)}
                  ,{<<"desc">>,erlang:list_to_binary(Desc)}
                  ]}).

sign(ApporderID, Price, Uid) ->
    OriStr = get_callback()++"?"++mochiweb_util:urlencode([{apporderID,ApporderID},{price,Price},{uid,Uid}]),
    bin_to_hexstr(crypto:hmac('md5', list_to_binary(?PAYKEY), list_to_binary(OriStr))).

get_callback() ->
    "http://120.132.76.41/payyk".

bin_to_hexstr(Bin) ->
   lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).


%% -------------------------------------- test code ---------------------------------------------
