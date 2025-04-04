
-module(mod_login_yw).
-include("common.hrl").
-include("record.hrl").
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
    UID = proplists:get_value("uid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
    SessionID = proplists:get_value("sessionid", QueryString),
    if UID =:= ?undefined orelse SessionID =:= ?undefined orelse DevID =:= ?undefined ->
           Reply = ejson:encode({[{<<"result">>,3}]}),
           platform_tool:return(Req, Reply);
       true ->
           case check_auth(SessionID, UID) of
               true ->
                   case db_func:get_yw_accid(UID, DevID,util:get_ip_from_req(Req)) of
                       [] ->
                           Reply = ejson:encode({[{<<"result">>,1}]}),
                           platform_tool:return(Req, Reply);
                       [Accid] ->
                           Version = proplists:get_value("version", QueryString),
                           {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_YAOWAN, Version),
                           ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                           ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                           LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_YAOWAN),
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

check_auth(SessionID, UID) ->
    AppID = "g00000017",
    AppKey = "KdYGGameLDbLYAOWAN",
    Ts = erlang:integer_to_list(util:now()),
    Sign = sign([SessionID, UID, AppID, Ts, AppKey]),
    URL = "http://member.yaowan.com/?m=appUser&action=checkLoginStatusKey",
    Body = "sessionid=" ++ SessionID ++ "&uid=" ++ UID ++ "&appid=" ++ AppID ++ "&ts=" ++ Ts ++ "&sign=" ++ Sign,
    case httpc:request(post, {URL, [{"urlencode, utf-8"}],"application/x-www-form-urlencoded",Body}, [], []) of
        {ok,{_,_,R}} ->
            {RS} = ejson:decode(R),
            case get_value(RS, <<"status">>) of
                1 ->
                    true;
                _ ->
                    false
            end;
        _Err ->
            false
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
