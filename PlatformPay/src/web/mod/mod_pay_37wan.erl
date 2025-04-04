%% @author admin
%% @doc @todo Add description to pay_mod_37wan.


-module(mod_pay_37wan).
-include("common.hrl").
-include("record.hrl").

-export([handle/1]).

-define(APPKEY, "cf207e9ba5993ab24d07ba8c98560eda").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req)->
    QueryString = Req:parse_post(),
    Username = proplists:get_value("username", QueryString),
    ChangeID = proplists:get_value("change_id", QueryString),
    Money = proplists:get_value("money", QueryString),
    Hash = proplists:get_value("hash", QueryString),
    Object = proplists:get_value("object", QueryString),
    case lists:member(?undefined, [Username, ChangeID, Money, Hash, Object]) of
        false ->
            case check_sign(Username, ChangeID, Money, Hash) of
                true ->
                    Amount = trunc(erlang:list_to_integer(Money) * 10),
                    RoleID = erlang:list_to_integer(Object),
                    ServerID = (RoleID div 1000000) - 1,
                    Server = data_server_list:get(ServerID),
                    HttpPort = integer_to_list(Server#server.serverHttpPort),
                    SendQS = mochiweb_util:urlencode(QueryString),
                    URL = "http://" ++ Server#server.serverIP ++ ":" ++ HttpPort ++ "/pay37wan?" ++ SendQS,
                    Response = httpc:request(get, {URL, []}, [], []),
                    case Response of
                        {ok, {_, _, Content}} ->
                            {Content2} =    try ejson:decode(Content) catch
                                                _:_ -> {json_false}
                                            end,
                            Result = get_value(Content2, <<"result">>),
                            if Result =:= 1 ->
                                    AccID = get_value(Content2, <<"accid">>),
                                    db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_37WAN, QueryString),
                                    Reply = "1",
                                    Req:ok({"text/html; charset=utf-8", Reply});
                                true ->
                                    db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_37WAN, QueryString),
                                    Reply = "0",
                                    Req:ok({"text/html; charset=utf-8", Reply}),
                                    ?ERR("37wan check_order failed. reason:game server return false,order:~w",[QueryString])
                            end;
                        _ ->
                            db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_37WAN, QueryString),
                            Reply = "0",
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("37wan check_order failed. reason:game Server not response not 0,order:~w",[QueryString])
                    end;
                _ ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_37WAN, QueryString),
                    Reply = "0",
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("37wan check_order failed. reason:sign wrong,order:~w",[QueryString])
            end;
        _ ->
            ?ERR("the pay message is not complete~n", []),
            Reply = "0",
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Username, ChangeID, Money, Hash) ->
    sign(Username, ChangeID, Money) =:= Hash.

sign(Username, ChangeID, Money) ->
    md5(Username ++ "|" ++ ChangeID ++ "|" ++ Money ++ "|" ++ ?APPKEY).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_value(Response, Key) when is_list(Response) andalso is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false -> false;
        {Key, Value} -> Value
    end;
get_value(_Response, _Key) -> false.

