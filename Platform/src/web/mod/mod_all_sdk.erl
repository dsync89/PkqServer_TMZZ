
-module(mod_all_sdk).
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
    Token = proplists:get_value("token", QueryString),
    Channel = proplists:get_value("channel", QueryString),
    UserId = proplists:get_value("uid", QueryString),
    case proplists:get_value("devid", QueryString) of
        ?undefined ->
            DevID = "";
        DevID ->
            next
    end,
    Version = proplists:get_value("version", QueryString),
    case check_login(Token, Channel, UserId) of
        {true, AccountType} ->
            [Accid] = db_func:get_all_sdk_accid(UserId, AccountType, DevID,util:get_ip_from_req(Req)),
            {DelServerIDList,ServerList} = tk_config:get_server_list(AccountType, Version),
            ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
            ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
            LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
            pm_platform_server:add_loglogin(Accid, AccountType),
            Reply = ejson:encode({[
                                   {<<"result">>,0},
                                   {<<"accid">>,Accid},
                                   {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                   {<<"valid_time">>,list_to_binary(ValidTime)},
                                   {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                   {<<"server_list">>,ServerList}
                                  ]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        false ->
            Reply = ejson:encode({[{<<"result">>,1}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================
latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").

check_login(Token, Channel, UserId) ->
    case data_sdk_id:get(Channel) of
        ?undefined ->
            ?ERR("Channel:~s", [Channel]),
            false;
        {AccountType, ProductCode, _} ->
            URL = "http://gameproxy.xinmei365.com/game_agent/checkLogin" ++ "?productCode=" ++ ProductCode ++ "&token=" ++ Token ++ "&channel=" ++ Channel ++ "&userId=" ++ UserId,
            case httpc:request(get, {URL, []}, [], []) of
                {ok,{_,_,R}} ->
                    case R of
                        "true" ->
                            {true, AccountType};
                        "false" ->
                            false
                    end;
                Err ->
                    ?ERR("Err:~w", [Err]),
                    false
            end
    end.

