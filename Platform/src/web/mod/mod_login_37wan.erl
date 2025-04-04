%% @author liuqiang
%% @doc 魅族sdk登录处理


-module(mod_login_37wan).

-include("common.hrl").
-include("record.hrl").

-export([handle/1]).

-define(APPKEY, "cf207e9ba5993ab24d07ba8c98560eda").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    QueryString = Req:parse_qs(),
    AppID = proplists:get_value("appid", QueryString),
    Token = proplists:get_value("token", QueryString),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    Uname = proplists:get_value("uname", QueryString),
    case lists:member(?undefined, [AppID, Token, DevID, Version, Uname]) of
        false ->
            case check_auth(AppID, Token) of
                {true, Uname} ->
                    case db_func:get_37wan_accid(Uname, DevID,util:get_ip_from_req(Req)) of
                        [] ->
                            Reply = ejson:encode({[{<<"result">>, 1}]}),
                            Req:ok({"text/html; charset=utf-8", Reply});
                        [Accid] ->
                            {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_37WAN, Version),
                            ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                            ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                            LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                            pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_37WAN),
                            Reply = ejson:encode({[
                                                    {<<"result">>, 0},
                                                    {<<"accid">>, Accid},
                                                    {<<"login_ticket">>, list_to_binary(ServerLoginKey)},
                                                    {<<"valid_time">>, list_to_binary(ValidTime)},
                                                    {<<"login_history">>, list_to_binary(LatestSLInfo)},
                                                    {<<"server_list">>, ServerList}
                                                  ]}),
                            Req:ok({"text/html; charset=utf-8", Reply})
                    end;
                {false, Reason} ->
                    ?ERR("37wan login check failed, Reason:~w", [Reason]),
                    Reply = ejson:encode({[{<<"result">>, 2}]}),
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;
        _ ->
            Reply = ejson:encode({[{<<"result">>, 2}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(AppID, Token) ->
    URL = "http://app.5gwan.com:9000/user/info.php?sign=" ++ sign(Token) ++ "&token=" ++ Token ++ "&app_id=" ++ AppID,
    case httpc:request(get, {URL, []}, [], []) of
        {ok, {_, _, Ret}} ->
            {Ret2} = try ejson:decode(Ret) catch
                         _:_ -> {json_false}
                     end,
            case Ret2 of
                [{<<"state">>, <<"1">>}, {<<"data">>, {[{<<"userid">>,_Uid},{<<"username">>,Uname}]}}] ->
                    {true, binary_to_list(Uname)};
                Err -> {false, Err}
            end;
        Err -> {false, Err}
    end.

sign(Token) ->
    md5(md5(?APPKEY ++ "_" ++ Token)).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

latestSL(LatestSL) ->
    string:join([integer_to_list(E) || E <- LatestSL], ",").



%% -----------------------------------------------------------------------------------------------------------------------
test() ->
    AppID = "a001",
    Token = "b3066822e277f30638966f3e23719de2",
    {Ret, _TMP} = check_auth(AppID, Token),
    ?ERR("the check result is: ~w~n", [Ret]).
