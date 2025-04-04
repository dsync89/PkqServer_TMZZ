%% @author admin
%% @doc @todo Add description to mod_login_xy.


-module(mod_login_xy).
-include("common.hrl").
-include("record.hrl").

-define(APPID, "100005057").
-define(APPKEY, "YPfgti0EXXDsg2OhoOjA4RnBKX9ZUjzM").

%% API functions
-export([handle/1]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 获取角色基本信息
handle(Req) ->
    QueryString = Req:parse_qs(),
    Token = proplists:get_value("token", QueryString),
    UID = proplists:get_value("uid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    if 
        Token =:= ?undefined orelse DevID =:= ?undefined orelse Version =:= ?undefined orelse UID =:= ?undefined ->
           Reply = ejson:encode({[{<<"result">>,2}]}),
           platform_tool:return(Req, Reply);
       true ->
           case check_auth(Token,  UID) of
               true ->
                   case db_func:get_xy_accid(UID,DevID,util:get_ip_from_req(Req)) of
                       [] ->
                           Reply = ejson:encode({[{<<"result">>,1}]}),
                           platform_tool:return(Req, Reply);
                       [Accid] ->
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_XY, Version),
                           ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                           ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                           LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_XY),
                           Reply = ejson:encode({[{<<"result">>,0},
                                                  {<<"accid">>,Accid},
                                                  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                                  {<<"valid_time">>,list_to_binary(ValidTime)},
                                                  {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                                  {<<"server_list">>,ServerList}]}),
                           platform_tool:return(Req, Reply)
                   end;
               false  ->
                   Reply = ejson:encode({[{<<"result">>,2}]}),
                   platform_tool:return(Req, Reply)
           end
    end.

latestSL(LatestSL) ->
    string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Token, UID) ->
    URL = "http://passport.xyzs.com/checkLogin.php",
    Content = mochiweb_util:urlencode([{uid,UID},{appid,?APPID},{token,Token}]),
    case httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Content}, [], []) of
        {ok, {_,_,R}} ->
            {RS} = ejson:decode(R),
            case get_value(RS, <<"ret">>) of
                0 ->
                    true;
                Ret ->
                   ?ERR("xy login auth fail,ret:~w", [Ret]),
                   false
            end;
        _ ->
            ?ERR("xy login check request fail..."),
            false
    end.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.
    
