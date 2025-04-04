%% @author wenshuai

-module(mod_pay_mmy).
-include("common.hrl").
-include("record.hrl").

-define( APPKEY, "890aa7f45a5bcf32A4CNO88UtR6qYNrifKR3fkwsZ08FS3ts4oRXmPpDGCMrXKGF" ).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

%% handle处理成功返回"success"给渠道
handle( Req ) ->
    QueryString = Req:parse_post(),
    %?ERR("info:~w",[QueryString]),
    case proplists:get_value( "tradeState", QueryString ) of
        "success" ->
            case check_verify( QueryString ) of
                {true, Amount} ->
                    ExtraInfo = proplists:get_value("productDesc", QueryString), 
                    [RoleID, ServerID] = string:tokens(ExtraInfo, "."),		
                    RoleID2 = erlang:list_to_integer(RoleID),
                    ServerID2 = erlang:list_to_integer(ServerID),
                    Server = data_server_list:get(ServerID2),			
                    HttpPort = integer_to_list(Server#server.serverHttpPort),
                    SendQS = mochiweb_util:urlencode(QueryString),
                    Url = "http://"++Server#server.serverIP++":"++HttpPort++"/paymmy?"++SendQS,
                    %?ERR("url:~s",[Url]),
                    Response = httpc:request(get, {Url, []}, [], []),
                    case Response of 
                        {ok,{_,_,Content}} ->
                            {Content2} = ejson:decode(Content),
                            Result = get_value(Content2, <<"result">>),
                            case Result of
                                1 ->
                                    AccID = get_value(Content2, <<"accid">>),
                                    db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_MMY, QueryString),
                                    Reply = "success",
                                    Req:ok({"text/html; charset=utf-8", Reply});
                                _ ->
                                    db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_MMY, QueryString),
                                    Reply = "success",
                                    Req:ok({"text/html; charset=utf-8", Reply}),
                                    ?ERR("mmy pay failed. reason:game server return false,order:~w",[QueryString])
                            end;
                        _ ->
                            db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_MMY, QueryString),
                            Reply = "fail",   %%游戏服务没响应,返回fail,等渠道重新发
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("mmy pay failed. reason:game Server not response not 0,order:~w",[QueryString])
                    end;
                _ ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_MMY, QueryString),
                    Reply = "success",
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("mmy pay failed. reason:sign wrong,order:~w",[QueryString])
            end;
        _ ->
            db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_MMY, QueryString),
            Reply = "success",                       
            Req:ok({"text/html; charset=utf-8",Reply}),
            ?ERR("mmy pay failed. reason:success wrong,order:~w",[QueryString])	
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_verify( QueryString ) ->
    Sign = proplists:get_value("tradeSign", QueryString),
    OrderIdStr = proplists:get_value("orderID", QueryString),
    if 
        length( Sign ) < 14 ->
            false;
        true ->
            VerifyStr = string:sub_string( Sign, 1, 8 ),
            SignStr = string:sub_string( Sign, 9 ),
            TempStr = md5( SignStr ),
            case VerifyStr =:= string:sub_string( TempStr, 1, 8 ) of
                true ->
                    KeyStr = string:sub_string( SignStr, 1, 6 ),
                    RandKey = md5( KeyStr ++ ?APPKEY ),
                    Base64KeyStr = base64:decode_to_string( string:sub_string( SignStr, 7 )  ),
                    case OrderIdStr =:= calcOrderId( Base64KeyStr, RandKey, 0, "" ) of 
                        true ->
                            Amount = proplists:get_value("productPrice", QueryString),
                            {true, Amount};
                        _ -> false
                    end;
                _ -> false
            end
    end.

calcOrderId( [H|T], RandKey, I, OrderIdStr ) ->
    Pos = I rem 32 + 1,
    KeyRan = lists:nth( Pos, RandKey ),
    calcOrderId( T, RandKey, 1 + I, OrderIdStr ++ erlang:integer_to_list( H band KeyRan ) );

calcOrderId( [], _, _, OrderIdStr ) -> OrderIdStr.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

