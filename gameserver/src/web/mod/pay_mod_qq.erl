%% @author admin

-module(pay_mod_qq).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/3]).

pay_gold(Req,Method,Path) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString, Method, Path) of
        true ->
            GameRole = proplists:get_value("appmeta", QueryString),
            [_ServerIDT,RoleID|_] = string:tokens(GameRole, "."),

            RoleID2 = erlang:list_to_integer(RoleID),
            RoleAccID = db_sql:get_role_accid(RoleID2),
            Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            
            Amount = get_amount(QueryString),
            QS = mochiweb_util:urlencode(QueryString),
            Sign = "28_"++proplists:get_value("sig", QueryString),
            pay_gold2(RoleID2,Amount,QS,Sign    ,28);
        
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_qq(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].
%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
    %Notify_Data2 = decode_data(Notify_Data),
    Value = proplists:get_value("payitem", Notify_Data),
    [_ID,Price,_Num] = string:tokens(Value, "*"),
    trunc(erlang:list_to_float(Price)).

verify_sign(QueryString, Method, Path)->
    AppKey = <<"aX42IE7jyV7lWfOq&">>,
    SourceList = get_original_encode_list2(QueryString),
    SourceList2 = erlang:atom_to_list(Method)++ "&" ++
                      http_uri:encode(Path) ++ "&" ++
                      SourceList,
    SignTemp = crypto:hmac('sha', AppKey, SourceList2),
    Sign = binary_to_list(base64:encode(SignTemp)),
    CheckSign = proplists:get_value("sig", QueryString),
    Sign == CheckSign.


get_original_encode_list2(QS) ->
    lists:foldr(fun($*,Acc)->
                        [$%,$2,$A|Acc];
                   (E,Acc)->
                        [E|Acc]
                end, [], get_original_encode_list(QS)).

get_original_encode_list(QS)->
    QS2 = lists:keysort(1, QS),
    [_H|L] = 
        lists:foldl(fun({A,B},Acc) ->
                            case A of
                                "sig" ->
                                    Acc;
                                _ ->
                                    Acc++"&"++A++"="++ open_encode(B)
                            end
                    end, "", QS2),
    http_uri:encode(L).

open_encode(B) ->
    ExList = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!*()",
    lists:foldr(fun(E, Acc)->
                        case lists:member(E, ExList) of
                            true ->
                                [E|Acc];
                            _ ->
                                "%" ++ integer_to_list(E, 16) ++ Acc
                        end
                end, [], B).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.