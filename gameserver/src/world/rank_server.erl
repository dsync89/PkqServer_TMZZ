-module(rank_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

start() ->
    {ok,_} =
        supervisor:start_child(world_sup, 
                               {?MODULE,
                                {?MODULE, start_link, []},
                                permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).
-define(level_rank_list, level_rank_list).
-define(fight_power_rank_list, fight_power_rank_list).

-define(UPDATE_INTERVAL, (1000 * 60 * 30)).


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    process_flag(trap_exit,true),
    erlang:put(?level_rank_list, db_sql:get_etc(?DB_ETC_KEY_RANK_SERVER)),
    update_fight_power(),
    erlang:send_after(?UPDATE_INTERVAL, erlang:self(), update_data),
	NowSeconds = util:now(),
	{ServerOpenDays, Time} = data_common:get(level_rank_reward_datetime),
    {ServerOpenDate,_} = data_setting:get(serverOpenTime),
    LevelRankRewardSeconds = util:datetime_to_seconds({ServerOpenDate,Time}) + (ServerOpenDays - 1) * ?ONE_DAY_SECONDS,
	case LevelRankRewardSeconds > NowSeconds of
		true ->
            AfterMS = (LevelRankRewardSeconds - NowSeconds) * 1000,
            case AfterMS > 4294967295 of
                true ->
                    next;
                false ->
			         erlang:send_after(AfterMS, erlang:self(), send_level_rank_reward)
            end;
		false ->
			next
	end,
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    db_sql:set_etc(?DB_ETC_KEY_RANK_SERVER,erlang:get(?level_rank_list)),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================


update_fight_power() ->
    List = db_sql:get_fight_power_rank_list(),
    List2 = lists:foldr(fun([RoleID, FightPower], Acc) ->
                              case tk_id:is_robot(RoleID) of
                                  false ->
                                      [{RoleID, FightPower}|Acc];
                                  true ->
                                      Acc
                              end
                      end, [], List),
    List3 = lists:sublist(lists:reverse(lists:keysort(2, List2)), 50),
    {List4, _} = lists:foldr(fun({RoleID, _}, {Acc, AccRank}) ->
                                     #rolePublic{
                                                 roleName=RoleName,
                                                 level=Level,
                                                 isMale=IsMale,
                                                 title=Title,
                                                 fightPower=FightPower,
                                                 head=Head
                                                } = role_lib:get_rolePublic(RoleID),
                                     RankInfo =
                                         #p_rank_info{
                                                      rank=AccRank
                                                      ,roleID=RoleID
                                                      ,level=Level
                                                      ,name=RoleName
                                                      ,head=Head
                                                      ,title=Title
                                                      ,isMale=IsMale
                                                      ,fightPower=FightPower
                                                      ,chapterID=0
                                                     },
                                     {[RankInfo|Acc], AccRank - 1}
                             end, {[], erlang:length(List3)}, List3),
    erlang:put(?fight_power_rank_list, List4).

send_level_rank_reward() ->
	case erlang:get(?level_rank_list) of
		LevelRankList when erlang:is_list(LevelRankList) ->
			TemplateID = data_common:get(level_rank_reward_mail_id),
			lists:foreach(fun({RankHigh, RankLow, RankReward}) ->
								  case erlang:length(LevelRankList) >= RankHigh of
									  true ->
										  RewardRankList = lists:sublist(LevelRankList, RankHigh, RankLow - RankHigh + 1),
										  lists:foldl(fun({RoleID, _, _}, AccRankArg) ->
															  mail_server:send_sys_mail(RoleID, TemplateID, [AccRankArg], "", RankReward),
															  AccRankArg + 1
													  end, RankHigh, RewardRankList);
									  false ->
										  next
								  end
				          end, data_common:get(level_rank_reward));
		_ ->
			ignore
	end.

update_level_rank_list(RoleID, NewLevel) ->
    OldList = erlang:get(?level_rank_list),
    case check_is_need_update(NewLevel, OldList) of
        true ->
            List =
                case lists:keyfind(RoleID, 1, OldList) of
                    false ->
                        [{RoleID, NewLevel, util:now()}|OldList];
                    _ ->
                        lists:keyreplace(RoleID, 1, OldList, {RoleID, NewLevel, util:now()})
                end,
            case OldList of
                [] ->
                    List2 = List;
                _ ->
                    List2 = lists:sort(fun({_RoleID1, Level1, Now1}, {_RoleID2, Level2, Now2}) ->
                                               if
                                                   Level1 > Level2 ->
                                                       true;
                                                   Level1 =:= Level2 ->
                                                       Now1 < Now2;
                                                   true ->
                                                       false
                                               end
                                       end, List)
            end,
            erlang:put(?level_rank_list, List2);
        false ->
            next
    end.

check_is_need_update(NewLevel, OldList) ->
    case erlang:length(OldList) >= 50 of
        true ->
            {_, LastLevel, _} = lists:last(OldList),
            case NewLevel > LastLevel of
                true ->
                    true;
                false ->
                    false
            end;
        false ->
            true
    end.

do_handle_info({level_up, RoleID, NewLevel}, State) ->
    update_level_rank_list(RoleID, NewLevel),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_rank_info{rankType=RankType,start=Start,num=Num}}, State) ->
    case RankType of
        ?RANK_TYPE_FIGHT_POWER ->
            get_fight_power_rank(RoleID, Start, Num);
        ?RANK_TYPE_LEVEL ->
            get_level_rank(RoleID, Start, Num)
    end,
    {noreply, State};
do_handle_info(update_data, State) ->
    erlang:send_after(?UPDATE_INTERVAL, erlang:self(), update_data),
    update_fight_power(),
    db_sql:set_etc(?DB_ETC_KEY_RANK_SERVER,erlang:get(?level_rank_list)),
    {noreply, State};
do_handle_info(send_level_rank_reward, State) ->
	send_level_rank_reward(),
	{noreply, State};
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.



get_fight_power_rank(RoleID, Start, Num) ->
    case erlang:get(?fight_power_rank_list) of
        ?undefined ->
            ?unicast(RoleID, #sc_rank_info{rankType=?RANK_TYPE_FIGHT_POWER, type=0, list=[]});
        RankList ->
            ?unicast(RoleID, #sc_rank_info{rankType=?RANK_TYPE_FIGHT_POWER, type=0, list=lists:sublist(RankList, erlang:max(Start, 1), erlang:min(Num, 50))})
    end.

get_level_rank(RoleID, Start, Num) ->
    case erlang:get(?level_rank_list) of
        ?undefined ->
            ?unicast(RoleID, #sc_rank_info{rankType=?RANK_TYPE_LEVEL, type=0, list=[]});
        RankList ->
            ?unicast(RoleID, #sc_rank_info{rankType=?RANK_TYPE_LEVEL, type=0, list=trans_level_rank_list(lists:sublist(RankList, erlang:max(Start, 1), erlang:min(Num, 50)))})
    end.


trans_level_rank_list(List) ->
    {NewList, _} =
        lists:foldr(fun({RoleID, Level, _}, {Acc, AccRank}) ->
                            #rolePublic{
                                        roleName=RoleName,
                                        isMale=IsMale,
                                        title=Title,
                                        fightPower=FightPower,
                                        head=Head
                                       } = role_lib:get_rolePublic(RoleID),
                            RankInfo =
                                #p_rank_info{
                                             rank=AccRank
                                             ,roleID=RoleID
                                             ,level=Level
                                             ,name=RoleName
                                             ,head=Head
                                             ,title=Title
                                             ,isMale=IsMale
                                             ,fightPower=FightPower
                                             ,chapterID=0
                                            },
                            {[RankInfo|Acc], AccRank - 1}
                    end, {[], erlang:length(List)}, List),
    NewList.




