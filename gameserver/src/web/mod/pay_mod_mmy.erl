%% @author wenshuai

-module(pay_mod_mmy).
-include("common.hrl").

-define(srcType,54).
-define( APPKEY, "890aa7f45a5bcf32A4CNO88UtR6qYNrifKR3fkwsZ08FS3ts4oRXmPpDGCMrXKGF" ).

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    %% ?ERR("QS=~p\n",[QueryString]),
    case proplists:get_value( "tradeState", QueryString ) of
        "success" ->
            case check_verify( QueryString ) of
                {true, Amount,Sign} ->
                    ExtraInfo = proplists:get_value("productDesc", QueryString), 
                    [RoleID, _ServerID] = string:tokens(ExtraInfo, "."),		
                    RoleID2 = erlang:list_to_integer(RoleID),
                    RoleAccID = db_sql:get_role_accid(RoleID2),

                    Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),

                    Amount2 = list_to_integer(Amount) div 10,
                    QS = mochiweb_util:urlencode(QueryString),
                    pay_gold2(RoleID2,Amount2,QS,Sign,?srcType);
                false ->
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("mmy check sign failed.Order:~w.",[QueryString])
            end;
        _ -> 
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("mmy pay failed order:~w",[QueryString])
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
                    Base64KeyStr = base64:decode_to_string( string:sub_string( SignStr, 7 ) ),
                    case OrderIdStr =:= calcOrderId( Base64KeyStr, RandKey, 0, "" ) of 
                        true ->
                            Amount = proplists:get_value("productPrice", QueryString),
                            {true, Amount, Sign};
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

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_mmy(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
