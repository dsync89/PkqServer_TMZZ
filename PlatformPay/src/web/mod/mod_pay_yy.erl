%% @author caijunjun
%% @doc YY充值.


-module(mod_pay_yy).



%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-compile(export_all).

-include("common.hrl").
-include("record.hrl").

-define(APPKEY, "4a22c6c60e64e33e1a3ae9a69cb2601d").

handle(Req) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString) of
        true ->
            RoleID = proplists:get_value("cparam", QueryString),
            RoleID2 = erlang:list_to_integer(RoleID),     
            ServerID = (RoleID2 div 1000000) - 1,       
            Server = data_server_list:get(ServerID),    
            HttpPort = integer_to_list(Server#server.serverHttpPort),
            Amount = get_amount(QueryString),
            SendQS = mochiweb_util:urlencode(QueryString),
            Url = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_yy?" ++ SendQS,
            
            Response = httpc:request(get, {Url, []}, [], []),
            
            case Response of
                {ok, {_,_,Content}} ->
                    {Content2} = ejson:decode(Content),
                    Result2 = get_value(Content2, <<"result">>),
                    if Result2 == 1 ->
                           AccID = get_value(Content2, <<"accid">>),
                           db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YY, QueryString),
                           Reply = ejson:encode({[{<<"code">>,1}, 
                                                  {<<"data">>, {[{<<"orderid">>, list_to_binary(proplists:get_value("orderid", QueryString))},
                                                                 {<<"rmb">>, to_num(proplists:get_value("rmb", QueryString))}, 
                                                                 {<<"account">>, to_num(proplists:get_value("account", QueryString))} ]}}]}),
                           Req:ok({"text/html; charset=utf-8", Reply});
                       true ->
                           db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YY, QueryString),
                           Reply = ejson:encode({[{<<"code">>,-11}, {<<"data">>, null}]}),
                           Req:ok({"text/html; charset=utf-8", Reply}),
                           ?ERR("yy check_order failed. reason:game server return false,order:~w",[QueryString])
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_YY, QueryString),
                    Reply = ejson:encode({[{<<"code">>,-100}, {<<"data">>, null}]}), %% 订单未正确获取,需要重新获取订单
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("yy check_order failed. reason:game server no response,order:~w",[QueryString])
            end;
        _ ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_YY, QueryString),
            Reply = ejson:encode({[{<<"code">>,-11}, {<<"data">>, null}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("yy check_order failed. reason:sign wrong,order:~w",[QueryString])
    end.
    

%% ====================================================================
%% Internal functions
%% ====================================================================

verify_sign(QS) ->
    Sign = proplists:get_value("sign", QS),
    Account = proplists:get_value("account", QS),   
    OrderID = proplists:get_value("orderid", QS),
    RMB = proplists:get_value("rmb", QS),
    Num = proplists:get_value("num", QS),
    Type = proplists:get_value("type", QS),
    Time = proplists:get_value("time", QS),
    Game = proplists:get_value("game", QS),
    Server = proplists:get_value("server", QS),
    Role = proplists:get_value("role", QS),
    ItemID = proplists:get_value("itemid", QS),
    Price = proplists:get_value("price", QS),
    CParam = proplists:get_value("cparam", QS),
    DataList = Account ++ OrderID ++ RMB ++ Num ++ Type ++ Time 
                   ++ Game ++ Server ++ Role ++ ItemID ++ Price ++ CParam,
    util:md5(DataList ++ ?APPKEY) =:= Sign.

get_amount(QS) ->
    RMB = proplists:get_value("rmb", QS),
    erlang:trunc(to_num(RMB) * 10).

to_num(List) ->
    case catch erlang:list_to_integer(List) of
        Int when erlang:is_integer(Int) ->
            Int;
        _Err ->
            erlang:list_to_float(List)
    end.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

%% to_unicode(Text) ->
%%     erlang:binary_to_list(unicode:characters_to_binary(Text)).
