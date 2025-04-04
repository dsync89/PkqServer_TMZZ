-module(change_pay_list_mod).

-export([change_pay_list/1]).

-include("common.hrl").
-include("data.hrl").


change_pay_list(Req)->
    case parse_Req(Req) of
        {ok, Message}->
            case catch get_pay_list(Message) of
                {true, PayList} ->
                    ets:insert(?ETS_ETC, {pay_list, PayList}),
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

get_pay_list(Message) ->
    {List} = ejson:decode(Message),
    ConfigPayGoldList = [(data_pay:get(PayID))#data_pay.payGold||PayID<-data_pay:get_list()],
    PayList =
        lists:foldr(fun({_,PayGold}, Acc) ->
                            case lists:member(PayGold, ConfigPayGoldList) of
                                true ->
                                    [PayGold|Acc];
                                false ->
                                    erlang:throw({false, <<"gold_error">>})
                            end 
                    end, [], List),
    case erlang:length(PayList) =:= erlang:length(ConfigPayGoldList) of
        true ->
            {true, PayList};
        false ->
            {false, <<"count_error">>}
    end.

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
    Message = erlang:binary_to_list(ejson:encode({[{<<"gold">>,(data_pay:get(PayID))#data_pay.payGold}||PayID<-data_pay:get_list()]})),
    {Auth, _, _} = data_setting:get(passinfo),
    Pass = util:md5(Auth++Message),
    Arg = lists:flatten(io_lib:format("pass=~s&message=~s", [Pass,Message])),
    io:format("~s~n",[Arg]),
    httpc:request(post, {"http://192.168.0.124:8089/change_pay_list",
                         [], "application/x-www-form-urlencoded",Arg}, [], []).
