-module(activity_list_mod).


-include("common.hrl").
-include("data.hrl").

get_list(Req) ->
    case parse_Req(Req) of
        ok ->
            ConfigList =
                lists:foldr(
                  fun(ActivityID, Acc) ->
                          #data_activity{type=Type, isForever=IsForever, isDailyRefresh=IsDailyRefresh, startTime=StartTime, stopTime=StopTime,
                                         activityName=ActivityName, description=Description, drawList=DrawList} = data_activity:get(ActivityID),
                          [{ActivityID, [{<<"type">>,to_bin(Type)},
                                        {<<"isForever">>,to_bin(IsForever)},
                                        {<<"isDailyRefresh">>,to_bin(IsDailyRefresh)},
                                        {<<"startTime">>,configtime_to_seconds(StartTime)},
                                        {<<"stopTime">>,configtime_to_seconds(StopTime)},
                                        {<<"activityName">>,to_bin(ActivityName)},
                                        {<<"description">>,to_bin(Description)},
                                        {<<"drawList">>, trans_draw_list(DrawList)}]}|Acc]
                  end, [], data_activity:get_list()),
            Reply = mochijson2:encode({struct,ConfigList}),
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

to_bin(Data) when erlang:is_atom(Data) ->
    erlang:list_to_binary(erlang:atom_to_list(Data));
to_bin(Data) when erlang:is_list(Data) ->
    erlang:list_to_binary(Data);
to_bin(Data) when erlang:is_binary(Data) ->
    Data.

trans_draw_list(DrawList) ->
    lists:foldr(
      fun(#data_activity_draw{drawID=DrawID, description=Description, reward=Reward}, Acc) ->
              [{DrawID,[{<<"Description">>, to_bin(Description)}, {<<"reward">>, get_reward_type(Reward)}]}|Acc]
      end, [], DrawList).

get_reward_type(#sell_reward{coin=Coin
                      ,roleExp=RoleExp
                      ,gerExp=GerExp
                      ,gold=Gold
                      ,item=Item
                      ,reputation=Reputation
                      ,newGer=NewGer}) ->
    IsCoin = (Coin > 0),
    IsRoleExp = (RoleExp > 0),
    IsGerExp = (GerExp > 0),
    IsGold = (Gold > 0),
    IsReputation = (Reputation > 0),
    IsItem = (Item =/= [] andalso Item =/= 0),
    IsGer = (NewGer =/= [] andalso NewGer =/= 0),
    [{<<"IsCoin">>, to_bin(IsCoin)},
     {<<"IsRoleExp">>, to_bin(IsRoleExp)},
     {<<"IsGerExp">>, to_bin(IsGerExp)},
     {<<"IsGold">>, to_bin(IsGold)},
     {<<"IsReputation">>, to_bin(IsReputation)},
     {<<"IsItem">>, to_bin(IsItem)},
     {<<"IsGer">>, to_bin(IsGer)}].

configtime_to_seconds(SpecTime) ->
    {SpecDay,SpecSec} = SpecTime,
    if erlang:is_tuple(SpecDay) ->
           util:datetime_to_seconds(SpecTime);
       true ->
           {ServerOpenDate,_} = data_setting:get(serverOpenTime),
           util:datetime_to_seconds({ServerOpenDate,SpecSec}) + (SpecDay - 1) * ?ONE_DAY_SECONDS
    end.

test() ->
    inets:start(),
    Message = "activitylist",
    Pass = util:md5(lists:merge("passed", Message)),
    Arg = lists:flatten(io_lib:format("pass=~s&message=~s", [Pass,Message])),
%%     Url = "http://192.168.0.124:8089/activity_list?" ++ Arg,
%%     ?ERR("~s", [Url]),
%%     httpc:request(get, {Url,[]},[], []).
    httpc:request(post, {"http://192.168.0.124:8089/activity_list",
                         [], "application/x-www-form-urlencoded",Arg}, [], []).

