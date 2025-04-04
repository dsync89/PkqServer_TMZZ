%% @author admin
%% @doc @todo Add description to mod_login_gg.


-module(mod_login_gg).
-include("common.hrl").
-include("record.hrl").


-define(GAME_ID,"15704").
-define(APP_KEY,"0c7da86a2f095eda21b7d76e6b4f8aa4").

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
    Timestamp = proplists:get_value("ts", QueryString),
    Sign = proplists:get_value("sign", QueryString),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    if 
        UID =:= ?undefined orelse DevID =:= ?undefined
            orelse Version =:= ?undefined orelse Timestamp =:= ?undefined orelse Sign =:= ?undefined ->
           Reply = ejson:encode({[{<<"result">>,2}]}),
           platform_tool:return(Req, Reply);
       true ->
           case check_auth(UID, Timestamp, Sign) of
               true ->
                   case db_func:get_gg_accid(UID,DevID,util:get_ip_from_req(Req)) of
                       [] ->
                           Reply = ejson:encode({[{<<"result">>,1}]}),
                           platform_tool:return(Req, Reply);
                       [Accid] ->
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_GG, Version),
                           ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                           ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                           LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_GG),
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
           end
    end.

latestSL(LatestSL) ->
    string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(UID, Timestamp,Sign) ->
    MySign = md5(UID ++ "&" ++ Timestamp ++ "&" ++ ?APP_KEY),
    case MySign =:= Sign of
        true ->
            true;
        false ->
            ?ERR("gg login check sign fail"),
            false
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
    
