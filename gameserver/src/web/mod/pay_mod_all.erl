
-module(pay_mod_all).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    OrderId = proplists:get_value("orderId", QueryString),
    Price = proplists:get_value("price", QueryString),
    CallbackInfo = proplists:get_value("callbackInfo", QueryString),
    ChannelCode = proplists:get_value("channelCode", QueryString),
    Sign = proplists:get_value("sign", QueryString),
    case check(OrderId, Price, CallbackInfo, ChannelCode, Sign) of
        {true, SrcType} ->
           RoleID = erlang:list_to_integer(erlang:binary_to_list(base64:decode(CallbackInfo))),
           RoleAccID = db_sql:get_role_accid(RoleID),
           Req:ok({"text/html; charset=utf-8", ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]})}),

           Amount = trunc(erlang:list_to_integer(Price) / 10),
           QS = mochiweb_util:urlencode(QueryString),
           
           pay_gold2(RoleID,Amount,QS,Sign,SrcType);
       {false, Reason} ->
           Req:ok({"text/html; charset=utf-8", ejson:encode({[{<<"result">>,0}]})}),
           ?ERR("pay all pay failed ,Reason:~s,QueryString:~w",[Reason, QueryString])
    end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_all(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w",[RoleID,Req]),
    [].
%% ====================================================================
%% Internal functions
%% ====================================================================

check(OrderId, Price, CallbackInfo, ChannelCode, Sign) ->
    case data_sdk_id:get(ChannelCode) of
        ?undefined ->
            {false, "channel error"};
        {SrcType, _, ProductSecret} ->
            case util:md5(OrderId++Price++CallbackInfo++ProductSecret) =:= Sign of
                true ->
                    {true, SrcType};
                false ->
                    {false, "sign error"}
            end
    end.