-module(change_activity_list_mod).

-export([change_activity_list/1]).

-include("common.hrl").
-include("data.hrl").


change_activity_list(Req)->
    case parse_Req(Req) of
        {ok, Message}->
            case catch get_activity_id_list(Message) of
                {true, ActivityIDList} ->
                    ets:insert(?ETS_ETC, {activityIDList, ActivityIDList}),
                    Reply =ejson:encode({[{<<"result">>,<<"succ">>}]}),
                    Req:ok({"text/html; charset=utf-8", Reply});
                {false, Err} ->
                    Reply =ejson:encode({[{<<"result">>,Err}]}),
                    Req:ok({"text/html; charset=utf-8", Reply});
                Err ->
                    ?ERR("Err:~w", [Err]),
                    Reply =ejson:encode({[{<<"result">>,<<"message_error">>}]}),
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,<<"auth_fail">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

get_activity_id_list(Message) ->
    {List} = ejson:decode(Message),
    ConfigIDList = data_activity:get_list() ++  data_festival:get_list(),
    ActivityIDList =
        lists:foldr(fun({_,ActivityID}, Acc) ->
                            case lists:member(ActivityID, ConfigIDList) of
                                true ->
                                    [ActivityID|Acc];
                                false ->
                                    erlang:throw({false, <<"id_error">>})
                            end
                    end, [], List),
    {true, ActivityIDList}.

parse_Req(Req)->
    QueryString = Req:parse_post(),
    Pass = proplists:get_value("pass", QueryString),
    Message = proplists:get_value("message", QueryString),
    {can_pass(Pass, Message),Message}.

can_pass(Pass, Message) ->
    %% 认证消息是否有效,认证失败,则消息不进行公告广播
    {Auth, _, _} = data_setting:get(passinfo),
    Auth3 = util:md5(Auth ++ Message),
    if Pass =:= Auth3 -> ok;
       true -> false
    end.

%% --------------------------------------------------------------------------------------------------------------------

web_test() ->
    inets:start(),
    List = [{<<"activityID">>,ActivityID}||ActivityID<- lists:seq(162, 166) ++  data_festival:get_list()],
    Message = erlang:binary_to_list(ejson:encode({util:random_list_quick(List)})),
    {Auth, _, _} = data_setting:get(passinfo),
    Pass = util:md5(Auth++Message),
    Arg = lists:flatten(io_lib:format("pass=~s&message=~s", [Pass,Message])),
    io:format("~s~n",[Arg]),
    httpc:request(post, {"http://192.168.0.124:8089/change_activity_list",
                         [], "application/x-www-form-urlencoded",Arg}, [], []).
