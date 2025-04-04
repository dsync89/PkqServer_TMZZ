-module(clear_world_talk_mod).


-include("common.hrl").
-include("data.hrl").

get_list(Req) ->
    case parse_Req(Req) of
        ok ->
            catch erlang:send(talk_server, clear),
            Reply = ejson:encode({[{<<"result">>,<<"succ">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply});
        _ ->
            Reply = ejson:encode({[{<<"result">>,<<"fail">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

parse_Req(Req)->
    QueryString = Req:parse_post(), 
    Pass = proplists:get_value("pass", QueryString),
    Message = proplists:get_value("message", QueryString),
    can_pass(Pass, Message).

can_pass(Pass, Message) ->
    %% 认证消息是否有效,认证失败,则消息不进行公告广播
    {Auth, _, _} = data_setting:get(passinfo),
    Auth3 = util:md5(Auth ++ Message),
    if Pass =:= Auth3 -> ok;
       true -> false
    end.

test() ->
    inets:start(),
    Message = "clear_world_talk",
    Pass = util:md5("passed" ++ Message),
    Arg = lists:flatten(io_lib:format("pass=~s&message=~s", [Pass,Message])),
    httpc:request(post, {"http://192.168.0.124:8089/clear_world_talk",
                         [], "application/x-www-form-urlencoded",Arg}, [], []).

