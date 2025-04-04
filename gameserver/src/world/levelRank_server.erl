%% @author admin
%% @doc 冲级排行榜进程
%% Created 2013-5-10



-module(levelRank_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).
-define(DUMP_INTERVAL_SECS, (1 * 60)). %% 定时dump当前排行榜的数据的时间间隔
%% ===================Dict Key Begin =========================
-define(levelrank_firstN,levelrank_firstN).
-define(levelRank,levelRank).
-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
-record(rankerInfo, {rank, roleID, roleName, exp}).
%% ===================Dict Key End   =========================
is_persist({?levelrank_firstN,_}) -> true;
is_persist({?levelRank, _}) -> true ;
is_persist(_) ->false.


get_rank_info(AT, ST, RoleID)->
	erlang:send(?MODULE, {get_rank_info, AT, ST, RoleID}).

get_rank_info(RoleID) ->
	erlang:send(?MODULE, {get_rank_info2,RoleID}).

refresh_ranker_info(RoleID, RoleName, Exp)->
	erlang:send(?MODULE, {refresh_ranker_info, RoleID, RoleName, Exp}).
	
i() ->
	gen_server:call(?MODULE, i).


start() ->
	{ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	init_state(),
    erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
	{ok, ok}.

time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).


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
%% @doc gen_server:init/1
handle_call(i, _From, State) ->
	{reply, State, State};
%% handle_call(Request, _From, State) ->
%% 	Reply = 
%% 	case ?CATCH(do_handle_call(Request, State)) of
%% 		{'EXIT', _} ->
%% 			ok;
%% 		X ->
%% 			X
%% 	end,
%% 	{reply, Reply, State}.
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.


-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_cast/2
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_info/2

handle_info(dump_cur_rank_data, State) ->
	persist_info(),
	erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
	{noreply, State};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
    {noreply, State}.

-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	persist_info(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.


-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

do_handle_info(send_reward)->
	RewardList = get_levelRankRewardList(),
	lists:foreach(fun(#rankerInfo{rank=Rank,roleID=RoleID})->
						{_,Reward} = lists:keyfind(Rank, 1, RewardList),
						mail_server:send_sys_mail(RoleID, ?MAIL_LEVEL_RANK_REWARD, [Rank], "", Reward)
						end, get_curRank(?levelRank));
do_handle_info({refresh_ranker_info, RoleID, RoleName, Exp})->
	refresh_rankerList(RoleID, RoleName, Exp);
do_handle_info({get_rank_info, AT, ST, RoleID})->
	RankList = get_firstN(?levelRank),
	?unicast(RoleID, #sc_activity_levelRank_open{stopTime=AT, endTime=ST, rankerInfoList=RankList});
do_handle_info({get_rank_info2,RoleID})->
	RankList = get_firstN(?levelRank),
	?unicast(RoleID, #sc_activity_levelRank_refresh{rankerInfoList=RankList}).

refresh_rankerList(RoleID, RoleName, Exp)->
			Ranker=#rankerInfo{rank=0,roleID=RoleID, roleName=RoleName, exp=Exp},
			OldRankList = get_curRank(?levelRank),
			{NewRank, NewRankList} =  insert_rank3(Exp, Ranker,OldRankList,[],0),
				LevelRank_Num = data_levelRank:get(rankNum),
			NewRankList2 = lists:sublist(update_levelRank_rankList(NewRankList),LevelRank_Num),
			LevelRank_Num = data_levelRank:get(rankNum),
			set_curRank(?levelRank, NewRankList2),
			if NewRank =< LevelRank_Num ->
				   refresh_firstN(?levelRank, NewRankList2);
			   true ->
				   ignore
			end.

update_levelRank_rankList(NewRankList)->
	{_,L}=lists:foldl(fun(Ranker, {Rank,RankerList})->{Rank+1,[Ranker#rankerInfo{rank=Rank}|RankerList]} end, {1,[]}, NewRankList),
	lists:reverse(L).

refresh_firstN(?levelRank, RankList) ->
	LevelRank_Num = data_levelRank:get(rankNum),
	RewardList = get_levelRankRewardList(),
	FirstNList = lists:sublist(RankList, LevelRank_Num),
	FirstNInfoList = lists:foldr(fun(Rank,Acc)->
										 {_,Reward} = lists:keyfind(Rank, 1, RewardList),
										 One=
											 case lists:keyfind(Rank, #rankerInfo.rank, FirstNList) of
												 false ->
													 #levelRank_rankerInfo{roleName="", roleExp=0,rankNum=Rank
																		 ,rewardInfo=activity_server:sell_reward2p_reward_info(Reward)};
												 #rankerInfo{exp=Exp,roleName=Name}->
													 #levelRank_rankerInfo{roleName=Name, roleExp=Exp,rankNum=Rank
																		 ,rewardInfo=activity_server:sell_reward2p_reward_info(Reward)}
											 end,
										 [One|Acc]
										 end,[], lists:seq(1, LevelRank_Num)),
	erlang:put(?levelrank_firstN, FirstNInfoList);
refresh_firstN(_,_) ->
	ignore.

get_curRank(?levelRank)->
	case get(?levelRank) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end.

set_curRank(?levelRank, RankList) ->
	erlang:put(?levelRank, RankList).

get_firstN(?levelRank) ->
	case erlang:get(?levelrank_firstN) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end;
get_firstN(_) ->
	[].

insert_rank3(Score, Ranker, [#rankerInfo{exp=Score2}=H|RankList]=E,Tail,N) ->
	if Score2 >= Score ->
	insert_rank3(Score, Ranker, RankList, [H|Tail],N+1);
	   Score2 < Score ->
		   %% 删除旧的信息
			%?ERR("info:~w,~w",[E,Tail]),
		   NewRankList = lists:reverse(Tail,[Ranker|lists:keydelete(Ranker#rankerInfo.roleID, #rankerInfo.roleID, E)]),
		   {N + 1, NewRankList}
	end;	   
insert_rank3(_Score, Ranker, [],Tail, N) ->
	NewRankList = lists:reverse([Ranker|Tail]),
	{N+1, NewRankList}.

persist_info() ->
		InfoList = lists:filter(fun({Key, Val})->is_persist({Key, Val}) end,erlang:get()),
		db_sql:set_etc(?DB_ETC_KEY_LEVELRANK, InfoList).

init_state() ->
	InfoList = db_sql:get_etc(?DB_ETC_KEY_LEVELRANK),
	lists:foreach(fun({Key, Val})->
						  erlang:put(Key, Val)
				  end, InfoList),
	refresh_firstN(?levelRank, get_curRank(?levelRank)),
	{ServerDate, _} = data_setting:get(serverOpenTime),
	TimePoint = data_levelRank:get(timePoint),
	%SPeriod = data_levelRank:get(showPeriod),
	APeriod = data_levelRank:get(activityPeriod),
	STime = util:datetime_to_seconds({ServerDate,TimePoint}) + APeriod * ?ONE_DAY_SECONDS,
	NowSec = util:now(),
	case NowSec < STime of
		true ->
			erlang:send_after((STime-NowSec) * 1000 , self(), send_reward);
		_ ->
			ignore
	end,
	ok.

get_levelRankRewardList()->
	InfoList = data_levelRank:get(rank_reward),
	lists:foldl(fun({{A,B},R},Acc)->
						RankList = lists:seq(A,B),
						lists:foldl(fun(E,Acc2)->
											 [{E,R}|Acc2]
											 end, [], RankList)
							++Acc
						end, [], InfoList).
	