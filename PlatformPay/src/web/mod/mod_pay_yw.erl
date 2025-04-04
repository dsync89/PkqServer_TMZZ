

-module(mod_pay_yw).
-include("common.hrl").
-include("record.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
    QueryString = decode_string(Req:recv_body()),
    AppID = proplists:get_value("app_id",QueryString),
    Sign = proplists:get_value("sign",QueryString),
    ServerID = proplists:get_value("server_id",QueryString),
    UserID = proplists:get_value("user_id",QueryString),
    SdkOrderNo = proplists:get_value("sdk_order_no",QueryString),
    CT = proplists:get_value("ct",QueryString),
    Money = proplists:get_value("money",QueryString),
    PayResult = proplists:get_value("pay_result",QueryString),
    RoleID = erlang:list_to_integer(proplists:get_value("custom_1",QueryString)),
    case AppID =:= "g00000017" of
        true ->
            LocalSign = sign([AppID,ServerID,UserID,SdkOrderNo,CT,"KdYGGameLDbLYAOWAN"]),
            Amount = trunc(erlang:list_to_float(Money) * 10),
            if
                Sign == LocalSign ->
                    if
                        PayResult =:= "1" ->
                            ServerID2 = (RoleID div 1000000) - 1,
                            Server = data_server_list:get(ServerID2),
                            HttpPort = integer_to_list(Server#server.serverHttpPort),
                            SendQS = mochiweb_util:urlencode(QueryString),
                            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payyw?" ++ SendQS,
                            Response = httpc:request(get, {Url, []}, [], []),   
                            case Response of
                                {ok, {_,_,Content}} ->
                                    {Content2} = ejson:decode(Content),
                                    Result = get_value(Content2, <<"result">>),
                                    if Result == 1 ->
                                           AccID = get_value(Content2, <<"accid">>),
                                           db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YAOWAN, QueryString),
                                           Req:ok({"text/html; charset=utf-8", SdkOrderNo});
                                       true ->
                                           ?ERR("yaowan pay failed order:~w,Reason:GameServer return",[QueryString]),
                                           db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YAOWAN, QueryString),
                                           Req:ok({"text/html; charset=utf-8", "GameServer error"})
                                    end;
                                _ ->
                                    ?ERR("yaowan pay failed order:~w,Reason:GameServer no response",[QueryString]),
                                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YAOWAN, QueryString),
                                    Req:ok({"text/html; charset=utf-8", "GameServer down"})
                            end;
                        true ->
                            ?ERR("yaowan pay failed order:~w,Reason:pay status",[QueryString]),
                            db_func:log_pay_info(5, RoleID, 0, Amount, erlang:localtime(), 0, ?ACCOUNT_TYPE_YAOWAN, QueryString),
                            Req:ok({"text/html; charset=utf-8", "pay_result error"})
                    end;
                true ->
                    ?ERR("yaowan pay failed order:~w,Reason:sign wrong",[QueryString]),
                    db_func:log_pay_info(2, RoleID, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_YAOWAN, QueryString),
                    Req:ok({"text/html; charset=utf-8", "sign error"})
            end;
        _ ->
            ?ERR("yaowan pay failed order:~w,reason:appid:~w wrong",[QueryString, AppID]),
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_YAOWAN, QueryString),
            Req:ok({"text/html; charset=utf-8", "appid error"})
    end.




%% ====================================================================
%% Internal functions
%% ====================================================================
get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

sign(StrList) ->
    md5(sign2(StrList)).

sign2([A]) ->
    A;
sign2([A,B]) ->
    A++B;
sign2([]) ->
    "";
sign2([A|T]) ->
    A++sign2(T).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

decode_string(Body) ->
    lists:foldr(fun({A, B}, Acc) ->
                        case A of
                            " name" ->
                                [Data1, Data2|_] = string:tokens(B, "\r\n"),
                                [DataKey|_] = string:tokens(Data1, "\""),
                                [{DataKey, Data2}|Acc];
                            _ ->
                                Acc
                        end
                end, [], mochiweb_util:parse_qs(erlang:binary_to_list(Body))).

test_data() ->
    <<"------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"app_id\"\r\n\r\ng00000017\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"server_id\"\r\n\r\n3\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"user_id\"\r\n\r\n53696557\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"order_no\"\r\n\r\nserverid=3roleid=4010018time=1419063007\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"sdk_order_no\"\r\n\r\n719720141220304600\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"money\"\r\n\r\n0.01\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"game_coin\"\r\n\r\n0.1\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"pay_result\"\r\n\r\n1\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"pay_time\"\r\n\r\n1419064121\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"custom_1\"\r\n\r\n4010018\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"custom_2\"\r\n\r\n\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"custom_3\"\r\n\r\n\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"ct\"\r\n\r\n1419064380\r\n------------------------------be374b936647\r\nContent-Disposition: form-data; name=\"sign\"\r\n\r\n97d15f0eaaaff57fb6671b2855b3e362\r\n------------------------------be374b936647--\r\n">>.

