
-module(festival_server).

-include("def_role.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_activity_info() ->
    case catch gen_server:call(?MODULE, get_activity_info) of
        {ok, List} ->
            lists:foldr(fun({ID, #data_festival{stopTime=StopTime, title=Title, content=Content}}, Acc) ->
                                EndTimestamp = util:configtime_to_seconds(StopTime),
                                case util:now() < EndTimestamp of
                                    true ->
                                        [#p_activity_ext{activityID=ID,endTimestamp=EndTimestamp,title=Title,content=Content}|Acc];
                                    false ->
                                        Acc
                                end     
                        end, [], List);
        Exception ->
            ?ERR("Exception:~w", [Exception]),
            []
    end.

role_festival(RoleID, ID) ->
    case catch gen_server:call(?MODULE, {role_festival, RoleID, ID}) of
        {ok, RoleFestival, Config} ->
            {ok, RoleFestival, Config};
        Exception ->
            ?ERR("Exception:~w", [Exception]),
            ?undefined
    end.

update_role_festival(RoleID, RoleFestival, ID) when erlang:is_record(RoleFestival, role_festival) ->
    case catch gen_server:call(?MODULE, {update_role_festival, RoleID, RoleFestival, ID}) of
        ok ->
            ok;
        Exception ->
            ?ERR("Exception:~w", [Exception]),
            ok
    end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).
-record(rank_info, {roleID=0,rank=0,totalCount=0,timestamp=0}).

-define(TICK_STATE_INTERVAL, 600).
-define(festival_config, festival_config).
-define(rank_list, rank_list).

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
    case db_sql:get_etc(?DB_ETC_KEY_FESTIVAL) of
        [{state,State}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            State = #state{}
    end,
    tick_state(),
    reload_config(),
    {ok, State}.


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
handle_call(get_activity_info, _From, State) ->
    {reply, {ok, [{ID, get_festival_config(ID)}||ID<- data_festival:get_list()]}, State};
handle_call({role_festival, RoleID, ID}, _From, State) ->
    {reply, {ok, get_role_festival(RoleID, ID), get_festival_config(ID)}, State};
handle_call({update_role_festival, RoleID, RoleFestival, ID},  _From, State) ->
    set_role_festival(RoleID, RoleFestival, ID),
    ?CATCH(update_rank_list(RoleID, RoleFestival, ID)),
    {reply, ok, State};
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
terminate(_Reason, State) ->
    do_persist(State),
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


do_handle_info({client_msg, RoleID, #cs_festival_info{id=ID}}, State) ->
    cs_festival_info(RoleID,ID),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_festival_rank{id=ID,start=Start,num=Num}}, State) ->
    cs_festival_rank(RoleID, Start, Num,ID),
    {noreply, State};
do_handle_info(tick_state, State) ->
    tick_state(),
    reload_config(),
    do_persist(State),
    {noreply, State};
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_FESTIVAL,Info2).

is_persist({{Key,_}, _}) when erlang:is_integer(Key) ->
    true;
is_persist({{?festival_config,_}, _}) ->
    true;
is_persist({{?rank_list,_}, _}) ->
    true;
is_persist(_) ->
    false.

tick_state() ->
    Sec = ?TICK_STATE_INTERVAL - (util:now() rem ?TICK_STATE_INTERVAL), 
    erlang:send_after(Sec*1000,self(),tick_state).

get_role_festival(RoleID, ID) ->
    erlang:get({RoleID, ID}).

set_role_festival(RoleID, RoleFestival, ID) ->
    erlang:put({RoleID, ID}, RoleFestival).

set_rank_list(List, ID) ->
    erlang:put({?rank_list, ID}, List).

get_rank_list(ID) ->
    case erlang:get({?rank_list, ID}) of
        ?undefined ->
            [];
        List ->
            List
    end.

get_festival_config(ID) ->
    case erlang:get({?festival_config,ID}) of
        Config when erlang:is_record(Config, data_festival) ->
            Config;
        _ ->
            #data_festival{}
    end.


set_festival_config(Config, ID) ->
    erlang:put({?festival_config, ID}, Config).

reload_config() ->
    tk_config:reload_config(data_festival),
    lists:foreach(fun(ID) ->
                          OldConfig = get_festival_config(ID),
                          NowConfig = gen_now_config(ID),
                          case NowConfig#data_festival.id =/= OldConfig#data_festival.id of
                              true ->
                                  do_reset(ID, OldConfig, NowConfig);
                              false ->
                                  set_festival_config(NowConfig, ID)
                          end
                  end,  data_festival:get_list()).

gen_now_config(ID) ->
    NowSeconds = util:now(),
    case util:fun_find(fun(#data_festival{startTime=StartTime, stopTime=StopTime}) ->
                               StartSeconds = util:configtime_to_seconds(StartTime),
                               StopSeconds = util:configtime_to_seconds(StopTime),
                               NowSeconds >= StartSeconds andalso NowSeconds < StopSeconds
                       end, data_festival:get(ID)) of
        Config when erlang:is_record(Config, data_festival) ->
            Config;
        _ ->
            #data_festival{}
    end.

do_reset(ID, #data_festival{id=OldID}=OldConfig, #data_festival{id=NowID}=NowConfig) ->
    case OldID =:= 0 of
        true ->
            case NowID =:= 0 of
                true ->
                    next;
                false ->
                    reset_role_info(NowConfig, ID)
            end;
        false ->
            rank_reward_reset_rank(OldConfig, ID),
            reset_role_info(NowConfig, ID)
    end,
    set_festival_config(NowConfig, ID).

%% 重置玩家信息，并广播客户端
reset_role_info(#data_festival{startTime=StartTime,stopTime=StopTime,freeTimes=FreeTimes,
                               onePrice=OnePrice,tenPrice=TenPrice,content=Content,
                               rankRewardList=RankRewardList,accBoxIDList=AccBoxIDList}, ID) ->
    lists:foreach(fun({{RoleID, IDT}, _}) when erlang:is_integer(RoleID) ->
                          case IDT of
                              ID ->
                                  erlang:erase({RoleID, ID});
                              _ ->
                                  ignore
                          end;
                     (_) ->
                          ignore
                  end, erlang:get()),
    broadcast_server:bc(#sc_festival_info{
                                          id=ID
                                          ,startTimestamp= util:configtime_to_seconds(StartTime)
                                          ,stopTimestamp= util:configtime_to_seconds(StopTime)
                                          ,totalCount=0
                                          ,totalGetCount=0
                                          ,freeCount=FreeTimes
                                          ,nextTotalCount=get_next_total_count(0, AccBoxIDList)
                                          ,price=OnePrice
                                          ,tenPrice=TenPrice
                                          ,info=Content
                                          ,rewardList=trans2p_festival_rank_reward(RankRewardList)}).

get_next_total_count(NowCount, AccBoxIDList) ->
    AccBoxIDList2 = lists:keysort(1, AccBoxIDList),
    case lists:filtermap(fun({Val, _}) ->
                                 Val > NowCount
                         end, AccBoxIDList2) of
        [] ->
            0;
        [{H, _}|_] ->
            H - NowCount
    end.

trans2p_festival_rank_reward(List) ->
    [#p_festival_rank_reward{rankStart=Min,rankEnd=Max,reward=role_reward:transform2p_mail_reward(SellReward)}||{Min, Max, SellReward}<-List].

%% 发送排行榜奖励,并重置排行榜
rank_reward_reset_rank(#data_festival{title=Title,rankRewardList=RankRewardList}, ID) ->
    RankList = get_rank_list(ID),
    TemplateID = data_common:get(festival_rank_mail_id),
    lists:foreach(fun({Min, Max, SellReward}) ->
                          send_reward_rank(Min, Max, SellReward, RankList, TemplateID, Title)
                  end, RankRewardList),
    set_rank_list([], ID).

send_reward_rank(Min, Max, SellReward, RankList, TemplateID, Title) ->
    RankList2 =
        lists:filter(fun(#rank_info{rank=Rank}) ->
                             Rank >= Min andalso Rank =< Max
                     end, RankList),
    lists:foreach(fun(#rank_info{roleID=RoleID,rank=Rank}) ->
                          mail_server:send_sys_mail(RoleID, TemplateID, [Title, Rank], "", SellReward)
                  end, RankList2).

cs_festival_info(RoleID, ID) ->
    #data_festival{freeTimes=FreeTimes, startTime=StartTime, stopTime=StopTime,
                   onePrice=OnePrice,tenPrice=TenPrice,content=Content,
                   rankRewardList=RankRewardList,accBoxIDList=AccBoxIDList} = get_festival_config(ID),
    case get_role_festival(RoleID, ID) of
        #role_festival{timestamp=ResetTimestamp} = RoleFestivalT ->
            {ResetDate, _} = util:seconds_to_datetime(ResetTimestamp),
            case ResetDate =/= erlang:date() of
                true ->
                    RoleFestival = RoleFestivalT#role_festival{freeCount = FreeTimes, timestamp = util:now()},
                    set_role_festival(RoleID, RoleFestival, ID);
                false ->
                    RoleFestival = RoleFestivalT
            end;
        _ ->
            RoleFestival = #role_festival{freeCount = FreeTimes,timestamp = util:now(),totalCount = 0,activeList = [], dropList = []},
            set_role_festival(RoleID, RoleFestival, ID)
    end,
    ?unicast(RoleID, #sc_festival_info{
                                       id=ID
                                       ,startTimestamp= util:configtime_to_seconds(StartTime)
                                       ,stopTimestamp= util:configtime_to_seconds(StopTime)
                                       ,totalCount= RoleFestival#role_festival.totalCount
                                       ,totalGetCount=calc_now_total_get(RoleFestival#role_festival.activeList, RoleFestival#role_festival.dropList)
                                       ,freeCount= RoleFestival#role_festival.freeCount
                                       ,nextTotalCount=get_next_total_count(RoleFestival#role_festival.totalCount, AccBoxIDList)
                                       ,price=OnePrice
                                       ,tenPrice=TenPrice
                                       ,info=Content
                                       ,rewardList=trans2p_festival_rank_reward(RankRewardList)}).

calc_now_total_get(ActiveList, DropList) ->
    case util:fun_find(fun(ActiveVal) ->
                               not lists:member(ActiveVal, DropList)
                       end, lists:sort(ActiveList)) of
        false ->
            0;
        Val ->
            Val
    end.

cs_festival_rank(RoleID, Start, Num, ID) ->
    RankList = get_rank_list(ID),
    SelfRank =
        case lists:keyfind(RoleID, #rank_info.roleID, RankList) of
            false ->
                0;
            #rank_info{rank=Rank} ->
                Rank
        end,
    RankList2 = 
        case erlang:length(RankList) >= Start of
            true ->
                lists:sublist(RankList, Start, Num);
            false ->
                []
        end,
    ?unicast(RoleID, #sc_festival_rank{id=ID,minRankCount=data_common:get(festival_rank_need_times),
                                       selfRank=SelfRank,
                                       rankList=trans2p_festival_rank(RankList2)}).

trans2p_festival_rank(RankList) ->
    lists:map(fun(#rank_info{roleID=RoleID,rank=Rank,totalCount=TotalCount}) ->
                      case role_lib:get_rolePublic(RoleID) of
                          [] ->
                              #p_festival_rank{
                                               roleID=RoleID
                                               ,isMale=false
                                               ,title=0
                                               ,head=0
                                               ,level=1
                                               ,roleName= <<"">>
                                               ,totalCount=TotalCount
                                               ,rank=Rank
                                              };
                          #rolePublic{isMale=IsMale,level=Level,title=Title,roleName=RoleName,head=Head} ->
                              #p_festival_rank{
                                               roleID=RoleID
                                               ,isMale=IsMale
                                               ,title=Title
                                               ,head=Head
                                               ,level=Level
                                               ,roleName=RoleName
                                               ,totalCount=TotalCount
                                               ,rank=Rank
                                              }
                      end
              end, RankList).

update_rank_list(RoleID, #role_festival{totalCount=TotalCount}, ID) ->
    case TotalCount >= data_common:get(festival_rank_need_times) of
        true ->
            RankList = get_rank_list(ID),
            case lists:keyfind(RoleID, #rank_info.roleID, RankList) of
                false ->
                    RankList2 = [#rank_info{roleID=RoleID,rank=0,totalCount=TotalCount,timestamp=util:now()}|RankList];
                _ ->
                    RankList2 = lists:keyreplace(RoleID, #rank_info.roleID, RankList, #rank_info{roleID=RoleID,rank=0,totalCount=TotalCount,timestamp=util:now()})
            end,
            RankList3 = lists:sort(fun(#rank_info{totalCount=TC1,timestamp=TS1}, #rank_info{totalCount=TC2,timestamp=TS2}) ->
                                           if
                                               TC1 > TC2 ->
                                                   true;
                                               TC1 =:= TC2 ->
                                                   TS1 =< TS2;
                                               true ->
                                                   false
                                           end
                                   end, RankList2),
            {RankList4, _} = lists:foldr(fun(RankInfo, {AccList, Rank}) ->
                                                 {[RankInfo#rank_info{rank=Rank}|AccList], Rank - 1}
                                         end, {[], erlang:length(RankList3)}, RankList3),
            set_rank_list(RankList4, ID);
        false ->
            ignore
    end.




