%% @author caijunjun
%% @doc 酷派登录.


-module(mod_login_coolpad).

-include("common.hrl").

-define(APPID, "5000000801").
-define(APPKEY, "2a21233dd3eb4c0984179bc9dc67580e").
-define(LOGIN_URL, "https://openapi.coolyun.com/oauth2/token?").

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

handle(Req) ->
    QueryString = Req:parse_qs(),
    case check_auth(QueryString) of
        {true, Uid, DevID, Version, AccessToken} ->
            case db_func:get_coolpad_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_COOLPAD, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_COOLPAD),
                    Reply = ejson:encode({[
                                            {<<"result">>,0},
                                            {<<"accid">>,Accid},
                                            {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                            {<<"valid_time">>,list_to_binary(ValidTime)},
                                            {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                            {<<"server_list">>,ServerList},
                                            {<<"open_id">>, list_to_binary(Uid)},
                                            {<<"access_token">>, list_to_binary(AccessToken)}
                                        ]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        {false, Reason} ->
            ?ERR("coolpad login check failed, Reason:~p",[Reason]),
            Reply = ejson:encode({[{<<"result">>,2}]}),
            platform_tool:return(Req, Reply)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(QS) ->
    Code = proplists:get_value("uid", QS),
    DevID = proplists:get_value("devid", QS),
    Version = proplists:get_value("version", QS),
    
    Url = ?LOGIN_URL ++ mochiweb_util:urlencode([{grant_type, "authorization_code"},
                                                 {client_id, ?APPID},{redirect_uri, ?APPKEY},
                                                 {client_secret, ?APPKEY}, {code, Code}]),
    case httpc:request(get, {Url, []}, [], []) of
        {ok,{_,_,Ret}} ->
            case unpack_response(Ret) of
                {true, Uid, AccessToken} ->
                    {true, Uid, DevID, Version, AccessToken};
                Err ->
                    Err
            end;
        _ ->
            {false, "http error :" ++ Url}
    end.


unpack_response(Ret) ->
    {Ret2} = try ejson:decode(Ret) catch
                 _:_ -> {[]}
             end,
    case proplists:is_defined(<<"openid">>, Ret2) of
        true ->
            Uid = proplists:get_value(<<"openid">>, Ret2),
            AccessToken = proplists:get_value(<<"access_token">>, Ret2),
            
            {true, erlang:binary_to_list(Uid), erlang:binary_to_list(AccessToken)};
        false ->
            Error = proplists:get_value(<<"error">>, Ret2),
            Des = proplists:get_value(<<"error_description">>, Ret2),
            {false, {Error, Des}}
    end.


latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").
    





