%% @author admin
%% @doc @todo Add description to mod_login_hw.


-module(mod_login_wx).
-include("common.hrl").
-include("record.hrl").

-define(APPID, "wx309a981dc5c73798").
-define(APPKEY, "4667c41ba988de38907c84e8ea5eff3a").

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
    DevID = proplists:get_value("devid", QueryString),
    Openid = proplists:get_value("openid", QueryString),
    AccessToken = proplists:get_value("openkey", QueryString),
    if Openid =:= ?undefined orelse DevID =:= ?undefined orelse AccessToken =:= ?undefined ->
           Reply = ejson:encode({[{<<"result">>,3}]}),
           platform_tool:return(Req, Reply);
       true ->
           case check_auth(Openid, AccessToken) of
               true ->
                       case db_func:get_wx_accid(Openid,DevID,util:get_ip_from_req(Req)) of
                       [] ->
                           Reply = ejson:encode({[{<<"result">>,1}]}),
                           platform_tool:return(Req, Reply);
                       [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_WEIXIN, Version),
                           ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                           ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                           LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_WEIXIN),
                           Reply = ejson:encode({[{<<"result">>,0},
                                                  {<<"accid">>,Accid},
                                                  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                                  {<<"valid_time">>,list_to_binary(ValidTime)},
                                                  {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                                  {<<"server_list">>,ServerList}]}),
                           platform_tool:return(Req, Reply)
                   end;
               {false,Reason}  ->
                   ?ERR("weixin login check failed, Reason:~w",[Reason]),
                   Reply = ejson:encode({[{<<"result">>,Reason}]}),
                   platform_tool:return(Req, Reply)
           end
    end.

latestSL(LatestSL) ->
    string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Openid, AccessToken) ->
    Timestamp = erlang:integer_to_list(util:now()),
    Sig = md5(?APPKEY ++ Timestamp),
    URL = "http://msdktest.qq.com/auth/check_token/?" ++ "timestamp=" ++ Timestamp ++  "&appid=" ++ ?APPID ++ "&sig=" ++ Sig ++ "&openid=" ++ Openid ++ "&encode=1",
    Body = erlang:binary_to_list(ejson:encode({[{<<"accessToken">>, erlang:list_to_binary(AccessToken)}, {<<"openid">>, erlang:list_to_binary(Openid)}]})),
    case httpc:request(post, {URL, [{"urlencode, utf-8"}], "application/x-www-form-urlencoded",Body}, [], []) of
        {ok,{_,_,RS}} ->
            {R} = ejson:decode(RS),
            case get_value(R, <<"ret">>) of
                0 ->
                    true;
                _ ->
                    {false, 2}
            end;
        Err ->
            ?ERR("Err:~w", [Err]),
            {false,4}
    end.
    
get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

sign(StrList) ->
    md5(sign2(StrList)).

sign2([A]) ->
    A;
sign2([A,B]) ->
    A++B;
sign2([]) ->
    "";
sign2([A|T]) ->
    A++sign2(T).
        
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


