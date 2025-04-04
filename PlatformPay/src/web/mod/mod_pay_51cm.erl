%% @author wenshuai

-module(mod_pay_51cm).
-include("common.hrl").
-include("record.hrl").

-define(SECRET,"2132735e02a70bdeceae06f4c248ed9e").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle( Req ) ->
    QueryString = Req:parse_qs(),
    %% ?ERR("info:~p.~n",[QueryString]),
    Success = proplists:get_value( "success", QueryString ),
    if Success =:= "0" ->
            case check_sign( QueryString, Success ) of
                {true, ExtraInfo, Amount} ->
                    [RoleID, ServerID] = string:tokens(ExtraInfo, "."),
                    RoleID2 = erlang:list_to_integer(RoleID),
                    ServerID2 = erlang:list_to_integer( ServerID ),
                    Server = data_server_list:get(ServerID2),
                    HttpPort = integer_to_list(Server#server.serverHttpPort),
                    SendQS = mochiweb_util:urlencode(QueryString),
                    Url = "http://"++Server#server.serverIP++":"++HttpPort++"/pay51cm?"++SendQS,
                    %% ?ERR("url:~p.~n",[Url]),
                    Response = httpc:request(get, {Url, []}, [], []),
                    case Response of 
                        {ok,{_,_,Content}} ->
                            {Content2} = ejson:decode(Content),
                            Result = get_value(Content2, <<"result">>),
                            case Result of
                                1 ->
                                    AccID = get_value(Content2, <<"accid">>),
                                    db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_51CM, QueryString),
                                    Reply = "success",
                                    Req:ok({"text/html; charset=utf-8", Reply});
                                _ ->
                                    db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_51CM, QueryString),
                                    Reply = "failure",
                                    Req:ok({"text/html; charset=utf-8", Reply}),
                                    ?ERR("51cm pay failed. reason:game server return false,order:~w",[QueryString])
                            end;
                        _ ->
                            db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_51CM, QueryString),
                            Reply = "failure",
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("51cm pay failed. reason:game Server not response not 0,order:~w",[QueryString])
                    end;
                _ ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_51CM, QueryString),
                    Reply = "errorsign",
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("51cm pay failed. reason:sign wrong,order:~w",[QueryString])
            end;
        true ->
            db_func:log_pay_info(5, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_51CM, QueryString),
            Reply = "failure",
            Req:ok({"text/html; charset=utf-8",Reply}),
            ?ERR("51cm pay failed. reason:success wrong,order:~w",[QueryString])	
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign( QueryString, Success ) ->
    Sign = proplists:get_value("sign", QueryString),
    OrderId = proplists:get_value("orderid", QueryString),
    Uid = proplists:get_value("uid", QueryString),
    Amount = proplists:get_value("amount", QueryString),
    ExtraInfo = proplists:get_value("extrainfo", QueryString),
    CheckStr = "orderid="++OrderId++"&uid="++Uid++"&amount="++Amount++"&actualamount="++Amount++
               "&extrainfo="++ExtraInfo++"&success="++Success++"&secret="++?SECRET,
    %% ?ERR("CheckStr:~p, Sign:~p.~n", [CheckStr, Sign] ),
    Sign2 = md5(CheckStr),
    if Sign =:= Sign2 ->
            Amount2 = erlang:list_to_integer(Amount) * 10,
            {true, ExtraInfo, Amount2 };
        true -> 
            false
    end.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

