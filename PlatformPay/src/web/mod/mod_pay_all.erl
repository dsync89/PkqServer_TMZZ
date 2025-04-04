
-module(mod_pay_all).
-include("common.hrl").
-include("record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
    QueryString = Req:parse_qs(),
    OrderId = proplists:get_value("orderId", QueryString),
    Price = proplists:get_value("price", QueryString),
    CallbackInfo = proplists:get_value("callbackInfo", QueryString),
    ChannelCode = proplists:get_value("channelCode", QueryString),
    Sign = proplists:get_value("sign", QueryString),
    case check(OrderId, Price, CallbackInfo, ChannelCode, Sign) of
        {true, SrcType} ->
            RoleID = erlang:list_to_integer(erlang:binary_to_list(base64:decode(CallbackInfo))),
            ServerID = (RoleID div 1000000) - 1,
            Amount = trunc(erlang:list_to_integer(Price) / 10),
            Server = data_server_list:get(ServerID),
            SendQS = mochiweb_util:urlencode(QueryString),
            HttpPort = integer_to_list(Server#server.serverHttpPort),
            Url = "http://"++Server#server.serverIP ++":"++HttpPort++"/payall/?"++SendQS,
            Response = httpc:request(get, {Url, []}, [], []),
            case Response of
               {ok, {_,_,Content}} ->
                   {Content2} = ejson:decode(Content),
                   Result = get_value(Content2, <<"result">>),
                   if Result == 1 ->
                          AccID = get_value(Content2, <<"accid">>),
                          db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, SrcType, QueryString),
                          Req:ok({"text/html; charset=utf-8", "success"});
                      true ->
                          db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, SrcType, QueryString),
                          Req:ok({"text/html; charset=utf-8", ""}),
                          ?ERR("all pay check_order failed. reason:gameserver return false,order:~w",[QueryString])
                   end;
               _ ->
                   db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, SrcType, QueryString),
                   Req:ok({"text/html; charset=utf-8", "no response"}),
                   ?ERR("all pay check_order failed. reason:gameserver no response,order:~w",[QueryString])
                   
           end;
        {false,Reason, SrcType} ->
            db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, SrcType, QueryString),
            Req:ok({"text/html; charset=utf-8", Reason}),
            ?ERR("all pay check_order failed. reason:~s,QueryString:~w",[Reason,QueryString])
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

check(OrderId, Price, CallbackInfo, ChannelCode, Sign) ->
    case data_sdk_id:get(ChannelCode) of
        ?undefined ->
            {false, "channel error", 255};
        {SrcType, _, ProductSecret} ->
            case util:md5(OrderId++Price++CallbackInfo++ProductSecret) =:= Sign of
                true ->
                    {true, SrcType};
                false ->
                    {false, "sign error", SrcType}
            end
    end.


