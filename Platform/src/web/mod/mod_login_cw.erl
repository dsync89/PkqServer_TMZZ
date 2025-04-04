%% @author admin
%% @doc @todo Add description to mod_login_cw.


-module(mod_login_cw).
-include("common.hrl").
-include("record.hrl").

-define(APP_ID,"10117").
-define(PACKET_ID,"25331").
-define(APP_KEY,"wAU0pCdyF2IcMYvs").
-define(SIGN_KEY,"rXTt9bJWlzN3inR6").

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
    ?DEBUG("QS=~p\n",[QueryString]),
    Token = proplists:get_value("token", QueryString),
    UID = proplists:get_value("uid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    ?DEBUG("Token:~w,UID:~w,DevID:~w,Version:~w", [Token,UID,DevID,Version]),
    if 
        Token =:= ?undefined orelse DevID =:= ?undefined orelse Version =:= ?undefined orelse UID =:= ?undefined ->
           Reply = ejson:encode({[{<<"result">>,2}]}),
           platform_tool:return(Req, Reply);
       true ->
           case check_auth(Token,  UID) of
               true ->
                   case db_func:get_cw_accid(UID,DevID,util:get_ip_from_req(Req)) of
                       [] ->
                           Reply = ejson:encode({[{<<"result">>,1}]}),
                           platform_tool:return(Req, Reply);
                       [Accid] ->
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_CW, Version),
                           ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                           ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                           LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_CW),
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
    %% 测试地址
%%     URL = "http://test.sdk.123cw.cn/UnionLogin/verifyToken?",
    %%正式地址
    URL = "http://unionlogin.123cw.cn/verifyToken?",
    Sign = md5(UID ++ "|" ++ Token ++ "|" ++ ?APP_KEY),
    Content = mochiweb_util:urlencode([{openid,UID},{token,Token},{sign,Sign}]),
    case httpc:request(get, {URL ++ Content, []}, [], []) of
        {ok,{_,_,R}} ->
            ?DEBUG("R:~w", [R]),
            case R of
                "success" ->
                    true;
                _ ->
                    ?ERR("cw auth fail"),
                    false
            end;
        Err ->
            ?ERR("Err:~w", [Err]),
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
    
