%% @author : admin
%% @doc : web mod

-module(change_vip_mod).
-export([change_vip/1]).
-include("common.hrl").
%% type 0:gag one by roleID, 1:free one by roleID 2:get gag list,message=000


change_vip(Req)->
    case parse_Req(Req) of
        {ok, VipLevel, RoleID}->
            case lists:member(VipLevel, data_vip:get_list()) of
                true ->
                    case db_sql:get_role_accid(RoleID) of
                        0 ->
                            Info = <<"err1">>;
                        _ ->
                            case erlang:whereis(role_lib:regName(RoleID)) of
                                ?undefined ->
                                    db_sql:sql_execute_with_log(io_lib:format("update gRole set vipLevel = ~w where roleID = ~w", [VipLevel, RoleID]));
                                _ ->
                                    role_lib:send_server(RoleID, {route, role_gm, {set_vip, VipLevel}})
                            end,
                            Info = <<"succ">>
                    end;
                false ->
                    Info = <<"err2">>
            end,
            Reply =ejson:encode({[{<<"result">>,Info}]}),
            Req:ok({"text/html; charset=utf-8", Reply});
        _ ->
            Reply = ejson:encode({[{<<"result">>,<<"err3">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

parse_Req(Req)->
    QueryString = Req:parse_post(), 
    Pass = proplists:get_value("pass", QueryString),
    Message = proplists:get_value("message", QueryString),
    VipLevel = proplists:get_value("viplevel", QueryString),
    RoleID = erlang:list_to_integer(Message),
    {can_pass(Pass, Message),erlang:list_to_integer(VipLevel), RoleID}.

can_pass(Pass, Message) ->
    %% 认证消息是否有效,认证失败,则消息不进行公告广播
    {Auth, _, _} = data_setting:get(passinfo),
    Auth3 = util:md5(Auth ++ Message),
    if Pass =:= Auth3 -> ok;
       true -> false
    end.

%% --------------------------------------------------------------------------------------------------------------------

web_test(RoleID, VipLevel) ->
    inets:start(),
    Message=erlang:integer_to_list(RoleID),
    Pass = util:md5("passed"++Message),
    Arg = lists:flatten(io_lib:format("pass=~s&message=~s&viplevel=~s", 
                                      [Pass,Message, erlang:integer_to_list(VipLevel)])),
    io:format("~s\n",[Arg]),
    httpc:request(post, {"http://192.168.0.124:8089/vip",
                         [], "application/x-www-form-urlencoded",Arg}, [], []).


