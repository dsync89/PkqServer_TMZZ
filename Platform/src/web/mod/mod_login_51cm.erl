%% @author wenshuai
%% @doc 51畅梦登录处理
%% Created 2014-8-4


-module(mod_login_51cm).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_auth( Req ) of
        {true, Uid, DevId, Version } ->
            case db_func:get_51cm_accid( Uid, DevId ,util:get_ip_from_req(Req)) of 
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    platform_tool:return(Req, Reply);
                [Accid] ->
                    {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_51CM, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),  

                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_51CM),
                    Reply = ejson:encode({[	{<<"result">>,0},
                                           {<<"accid">>,Accid},
                                           {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                           {<<"valid_time">>,list_to_binary(ValidTime)},
                                           {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                           {<<"server_list">>,ServerList}]}),
                    platform_tool:return(Req, Reply)
            end;
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

latestSL(LatestSL) ->
    string:join([integer_to_list(E)||E<-LatestSL],",").

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Req) ->
    QueryString = Req:parse_qs(),
    %% ?ERR("QS=~p\n",[QueryString]),
    Uid = proplists:get_value("uid", QueryString),
    UserName = proplists:get_value("username", QueryString),
    Time = proplists:get_value("time", QueryString),
    Token = proplists:get_value("token", QueryString),
    SvrUrl = "http://api.gc.51.com/2/auth/server",
    Message = mochiweb_util:urlencode([{uid,Uid}, {username,UserName}, {time,Time}, {token,Token}]),
    %% ?ERR("URL:~p",[Message]),
    Response = httpc:request(post, {SvrUrl,[], "application/x-www-form-urlencoded",Message}, [], []),
    case Response of
        {ok, {_,_,R} } ->
            {RS} = ejson:decode(R),
            ?ERR("RS:~p",[RS]),
            case get_value( RS, <<"errno">> ) of
                0 ->
                    Version = proplists:get_value( "version", QueryString ),
                    DevId = proplists:get_value("devid", QueryString),
                    {true, Uid, DevId, Version };
                _ ->
                     ?ERR("51 login fail: check fail"),
                    false
            end;
        _ ->
            false
    end.


