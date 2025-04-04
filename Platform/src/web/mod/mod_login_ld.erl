%% @author admin
%% @doc @todo Add description to mod_login_ld.


-module(mod_login_ld).
-include("common.hrl").
-include("record.hrl").


-define(SECRET,"9c40d6e2250a0276b1ef").

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
    ?ERR("QS=~p\n",[QueryString]),
    GameID = proplists:get_value("game_id", QueryString),
    OpenID = proplists:get_value("open_id", QueryString),
    SessionID = proplists:get_value("sessionid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    ?ERR("GameID:~w,OpenID:~w,SessionID:~w,DevID:~w,Version:~w", [GameID,OpenID,SessionID,DevID,Version]),
    if 
        GameID =:= ?undefined orelse DevID =:= ?undefined
            orelse Version =:= ?undefined orelse OpenID =:= ?undefined
            orelse SessionID =:= ?undefined ->
           Reply = ejson:encode({[{<<"result">>,2}]}),
           platform_tool:return(Req, Reply);
       true ->
           case check_auth(SessionID, OpenID, GameID) of
               true ->
                   case db_func:get_ld_accid(OpenID,DevID,util:get_ip_from_req(Req)) of
                       [] ->
                           Reply = ejson:encode({[{<<"result">>,1}]}),
                           platform_tool:return(Req, Reply);
                       [Accid] ->
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_LD, Version),
                           ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                           ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                           LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_LD),
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

check_auth(SessionID, OpenID, GameID) ->
    URL = "http://ly.feed.uu.cc/account/verify_osid",
    Sign = md5("openid=" ++ OpenID ++ "&sessionid=" ++ SessionID ++ "&secret=" ++ ?SECRET),
    Content = mochiweb_util:urlencode([{game_id,GameID},{open_id,OpenID},{sessionid,SessionID},{sign,Sign}]),
    case httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Content}, [], []) of
        {ok, {_,_,R}} ->
            case R of
                "ok" ->
                    true;
                Ret ->
                   ?ERR("ld login auth fail,ret:~w", [Ret]),
                   false
            end;
        _ ->
            ?ERR("ld login check request fail..."),
            false
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
    
