%% @author caijunjun
%% @doc 好接入登录.


-module(mod_login_hjr).

-include("common.hrl").


-define(GAMEKEY, "4524f7abe6af5e13173e654ffde13c3a").
-define(SECURITYKEY, "tfXF66h0Jzvph1Zo6VqQ9f9Ze2YZ5OMS").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

-compile(export_all).

handle(Req) ->
    QueryString = Req:parse_qs(),
    
    case check_auth(QueryString) of
        {true, DevID, Version, UID} ->
            case db_func:get_hjr_accid(UID, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    platform_tool:return(Req, Reply);
                [Accid] ->
                    {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_HJR, Version),
                    ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_HJR),
                    Reply = ejson:encode({[{<<"result">>,0},
                                           {<<"accid">>,Accid},
                                           {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                           {<<"valid_time">>,list_to_binary(ValidTime)},
                                           {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                           {<<"server_list">>,ServerList}]}),
                    platform_tool:return(Req, Reply)
            end;
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            platform_tool:return(Req, Reply)
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(QS) ->
    DevID = proplists:get_value("devid", QS),
    Version = proplists:get_value("version", QS),
    UID = proplists:get_value("userid", QS),
    Token = proplists:get_value("token", QS),
    TimeStamp = timestamp_str(),
    
    Result = case proplists:get_value("openid", QS) of
                 ?undefined ->
                     case proplists:get_value("cp", QS) of
                         ?undefined ->
                             false;
                         CP ->
                             {"http://anyapi.mobile.youxigongchang.com/foreign/oauth/verification.php",
                              lists:keysort(1, [{gamekey,?GAMEKEY}, {cp, CP}, {timestamp, TimeStamp}, {token, Token}])}
                     end;
                 OpenID ->
                     {"http://anyapi.mobile.youxigongchang.com/foreign/oauth/verification2.php",
                      lists:keysort(1, [{gamekey,?GAMEKEY}, {openid, OpenID}, {timestamp, TimeStamp}, {token, Token}])}
             end,
    case Result of
        {Url, TransData} ->
            Sign = decode_data(mochiweb_util:urlencode(TransData)),
            case check_auth2(Url, TransData, Sign) of
                true ->
                    {true, DevID, Version, UID};
                false ->
                    false
            end;
        false ->
            false
    end.
        
check_auth2(URL, TransData, Sign) ->
    Message = mochiweb_util:urlencode(TransData ++ [{'_sign',Sign}]),
    
    Response = httpc:request(post, {URL,[], "application/x-www-form-urlencoded",Message}, [], []),
    
    case Response of
        {ok,{_,_,Ret}} ->
            unpack_response(Ret);
        _ ->
            ?ERR("hjr login, url request error, URL = ~p~n", [URL]),
            false
    end.

unpack_response(Ret) ->
    {Ret2} = try 
                 ejson:decode(Ret)
             catch
                 _:_ -> {[]}
             end,
    Result = proplists:get_value(<<"result">>, Ret2),
    case Result of
        <<"0">> ->
            true;
        _ ->
            ?ERR("hjr login ret ~p ret2 ~p~n", [Ret, Ret2]),
            false
    end.

latestSL(LatestSL) ->
    string:join([integer_to_list(E)||E<-LatestSL],",").

decode_data(Data) ->
    md5(md5(Data) ++ ?SECURITYKEY).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

timestamp_str() ->
    erlang:integer_to_list(util:now()).


