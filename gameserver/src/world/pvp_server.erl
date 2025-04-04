%% @author admin
%% @doc pvp进程
%% Created 2013-4-7
-module(pvp_server).

-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-compile(export_all).

%% ===================Dict Key Begin =========================
-define(lock, lock). % 战斗锁
-define(first10, first10). % 前10命
-define(lowest_rank,lowest_rank). % 最低排名 
-define(firstRank, firstRank). % 第一的上线状态(10min)
-define(first8replay,first8replay). %八强战报
-define(replayRecord, replayRecord).%八强战报缓存
%% ===================Dict Key End   =========================
-define(MAX_RANK, 9999).
-define(MAX_EIGHT_REPLAY_NUM, 10).% 最大八强战报数量

-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([stop/0]).

-export([i/0]).
-export([call_get_rank/1, call_get_title_rank/1, update_level/2, update_title/2, unlock2/1]).

-define(dump_interval_tick, 3*60).
stop() ->
	gen_server:cast(?MODULE, stop).

unexpecte_unlock(RoleID)->
	gen_server:cast(?MODULE, {unexpect_unlock, RoleID}).


call_get_rank(RoleID) ->
	gen_server:call(?MODULE, {call_get_rank, RoleID}).

call_get_title_rank(RoleID) ->
	gen_server:call(?MODULE, {call_get_title_rank, RoleID}).

init_role_pvp(RoleInfo) when is_record(RoleInfo, role)->
	gen_server:call(?MODULE, {init_role_pvp, RoleInfo}).

update_roleName(RoleID,RoleName) ->
    erlang:send(?MODULE, {update_name,RoleID,RoleName}).

update_fightPower(RoleID, FightPower) ->
	erlang:send(?MODULE, {update_fightPower,RoleID,FightPower}).

update_level(RoleID, Level) ->
	erlang:send(?MODULE, {update_level, RoleID, Level}).

update_title(RoleID, Title) ->
	erlang:send(?MODULE, {update_title, RoleID, Title}).

update_head(RoleID,Head) ->
	erlang:send(?MODULE,  {update_head, RoleID, Head}).

do_get_list2(RoleID) ->
    Rank = get_rank(RoleID),
    PvpList = get_challenge_list(Rank),
    {ok, #sc_pvp_get_list{pvpList=PvpList,rank=Rank}}.

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
	init_data(),
	NextSec = get_next_refresh_title_sec(),
	?ERR("nextSec..=~w",[NextSec]),
	erlang:send_after(NextSec*1000, self(), refresh_title),
	dump_interval_tick(),
    {ok, ?undefined}.


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
handle_call({call_get_rank, RoleID}, _From, State) ->
	{reply, get_rank(RoleID), State};
handle_call({call_get_title_rank, RoleID}, _From, State) ->
	Rank = get_rank(RoleID),
	%% 通告世界第一上线
	if Rank =:= 1 ->
		   case get(?firstRank) of
			   ?undefined ->
				   RoleName = role_lib:get_name(RoleID),
				   erlang:put(?firstRank, 1),
				   erlang:send_after(600 * 1000, ?MODULE, clear_first_state),
				   broadcast_server:bc_msgID(10020, [RoleName]);
			   _ ->
				   ignore
		   end;
	   true ->
		   ignore
	end,
	case get_pvp(Rank) of
		#p_pvp{title=Title} ->
			next;
		_ ->
			Title=0
	end,
	{reply, {Rank, Title}, State};
handle_call({init_role_pvp, RoleInfo}, _From, State) when is_record(RoleInfo, role) ->
	Rank = get_rank(RoleInfo),
	{reply, Rank, State};
handle_call({unlock,RoleID}, _From, State) ->
	unlock(RoleID),
	{reply,ok,State};
handle_call({pvp_get_list,RoleID}, _From, State) ->
    {reply,catch do_get_list2(RoleID),State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.

new_pvp(RoleInfo, Rank)  ->
	#p_pvp{fightPower=RoleInfo#role.fightPower,
		   isMale=(RoleInfo#role.isMale),
		   roleID=RoleInfo#role.roleID,
		   level=RoleInfo#role.level,
		   title=RoleInfo#role.title,
		   rank=Rank,
		   roleName=RoleInfo#role.roleName,
		   head=RoleInfo#role.head}.

-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_cast/2
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast({unexpect_unlock, RoleID}, State)->
	TarRoleID = erlang:get({pvp_sign, RoleID}),
	case TarRoleID of
		?undefined ->
			ignore;
		_ ->
			finish_fight(RoleID, TarRoleID)
	end,
	{noreply, State};
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
	do_persist(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[pvp_server, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.

do_persist() ->
	List = lists:foldl(fun({Int, #p_pvp{roleID=RoleID,title=Title}}, Acc) when Int =< ?MAX_RANK ->
							   [{Int, RoleID, Title}|Acc];
						  ({Int, {RoleID, Title}}, Acc) when Int =< ?MAX_RANK ->
							   [{Int, RoleID, Title}|Acc];
						  (_,Acc) ->
							   Acc
					   end, [], erlang:get()),
	db_sql:set_etc(?DB_ETC_KEY_PVP, List),
	db_sql:set_etc(?DB_ETC_KEY_EIGHT_REPLAY, get_record_first_eight_replay()).



-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-define(test_ranker, [
#p_pvp{fightPower=100999,isMale=true, level=12,rank= 1,roleID= 1,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 2,roleID= 2,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 3,roleID= 3,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 4,roleID= 4,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 5,roleID= 5,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 6,roleID= 6,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 7,roleID= 7,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 8,roleID= 8,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank= 9,roleID= 9,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank=10,roleID=10,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank=11,roleID=11,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank=12,roleID=12,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank=13,roleID=13,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank=14,roleID=14,roleName="艳姬",title=1},
#p_pvp{fightPower=100000,isMale=false,level=13,rank=15,roleID=15,roleName="艳姬",title=1}]).


%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info({update_fightPower,RoleID,FightPower})->
	case get(RoleID) of
		Rank when is_integer(Rank) ->
			case get(Rank) of
				#p_pvp{}=PVP ->
					PVP2 = PVP#p_pvp{fightPower=FightPower},
					put(Rank, PVP2);
				_ ->
					ignore
			end;
		_ ->
			ignore
	end;
do_handle_info({update_level, RoleID, Level}) ->
	case get(RoleID) of
		Rank when is_integer(Rank) ->
			case get(Rank) of
				#p_pvp{}=PVP ->
					PVP2 = PVP#p_pvp{level=Level},
					put(Rank, PVP2);
				_ ->
					ignore
			end;
		_ ->
			ignore
	end;
do_handle_info({update_head, RoleID, Head})->
	case get(RoleID) of
		Rank when is_integer(Rank) ->
			case get(Rank) of
				#p_pvp{}=PVP ->
					PVP2 = PVP#p_pvp{head=Head},
					put(Rank,PVP2);
				_->
					ignore
			end;
		_->
			ignore
	end;
do_handle_info({update_title, RoleID, Level}) ->
	case get(RoleID) of
		Rank when is_integer(Rank) ->
			case get(Rank) of
				#p_pvp{}=PVP ->
					PVP2 = PVP#p_pvp{title=Level},
					put(Rank, PVP2);
				_ ->
					ignore
			end;
		_ ->
			ignore
	end;
do_handle_info({update_name,RoleID,RoleName})->
    case get(RoleID) of
        Rank when is_integer(Rank) ->
            case get(Rank) of
                #p_pvp{}=PVP ->
                    PVP2 = PVP#p_pvp{roleName=RoleName},
                    put(Rank, PVP2);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end;
do_handle_info(refresh_title) ->
	RefreshTime = data_common:get(title_refresh_time),
	Interval = (to_sec(RefreshTime)-to_sec(erlang:time())+?ONE_DAY_SECONDS)*1000,
	erlang:send_after(Interval, self(), refresh_title),
	do_refresh_title(),
    send_reward();
do_handle_info(test_send_reward) ->
    send_reward();
do_handle_info(clear_first_state) ->
	erlang:erase(?firstRank);
do_handle_info({fight, RoleID, #cs_pvp_fight{rank=TarRank,roleID=TarRoleID}}) ->
	case check_fight(TarRank, TarRoleID, RoleID) of
		true ->
			begin_fight(RoleID, TarRoleID),
			role_state:add_pvp_sign(RoleID, TarRoleID),
			role_lib:send_server(RoleID, {route, role_pvp, {do_pvp_fight, TarRoleID}});
		{false,Reason} ->
			?unicast(RoleID, #sc_pvp_fight{fightInfo=[],newRank=0,addRepu=0,result=Reason})
	end;
do_handle_info({client_msg, RoleID, #cs_pvp_get_first_eight_replays{}})->
	ReplayList = get_record_first_eight_replay(),
	?unicast(RoleID, #sc_pvp_get_first_eight_replays{infoList=ReplayList});
do_handle_info({client_msg, RoleID, #cs_pvp_eight_replay{replayUID=ReplayUID}})->
	{Info, Result} = get_replay_record(ReplayUID),
	?unicast(RoleID, #sc_pvp_eight_replay{result=Result, fightInfo=Info});
do_handle_info({client_msg, RoleID, #cs_rank_info{start=Start,num=Num}})->
    get_pvp_rank_info(RoleID, Start, Num);
do_handle_info({fight_win, FightRecord, SrcRoleID, TarRoleID, SrcPVP, TarPVP, AddRep}) ->
	finish_fight(SrcRoleID, TarRoleID),
	SrcRank = get_rank(SrcRoleID),
	TarRank = get_rank(TarRoleID),
	?INFO("TarPVP:~w\n",[TarPVP]),
	if TarPVP =:= [] ->
			TarPVP2 = get_pvp(TarRank);
		true ->
			TarPVP2 = TarPVP
	end,
	if SrcRank =< TarRank ->
		   SrcNewRank = SrcRank,
		   TarNewRank = TarRank,
		   SrcTitle = get_title_by_rank(SrcRank),
		   TarTitle = get_title_by_rank(TarRank),
		   SrcPVP2 = SrcPVP#p_pvp{rank=SrcRank,title=SrcTitle},
		   TarPVP3 = TarPVP2#p_pvp{rank=TarRank,title=TarTitle},
		   ?INFO("fight win rank nochange:\nSrcNewRank:~w TarNewRank:~w SrcPVP2:~w TarPVP3:~w\n ",[SrcNewRank,TarNewRank,SrcPVP2,TarPVP3]),
		   set_pvp(SrcNewRank, SrcPVP2),
		   set_pvp(TarNewRank, TarPVP3);
	   true ->
		   SrcNewRank = TarRank,
		   TarNewRank = SrcRank,
		   SrcTitle = get_title_by_rank(SrcRank),
		   TarTitle = get_title_by_rank(TarRank),
		   SrcPVP2 = SrcPVP#p_pvp{rank=TarRank,title=SrcTitle},
		   TarPVP3 = TarPVP2#p_pvp{rank=SrcRank,title=TarTitle},
		   ?INFO("fight win rank change:\nSrcNewRank:~w TarNewRank:~w SrcPVP2:~w TarPVP3:~w\n ",[SrcNewRank,TarNewRank,SrcPVP2,TarPVP3]),
		   set_pvp(SrcNewRank, SrcPVP2),
		   set_pvp(TarNewRank, TarPVP3),
		   set_rank(SrcRoleID,TarRank),
		   set_rank(TarRoleID,SrcRank)
	end,
	NewRank = erlang:min(SrcRank, TarRank),
	if NewRank =< 10 andalso NewRank < SrcRank ->
		   broadcast_server:bc_msgID(10019, [SrcPVP#p_pvp.roleName, NewRank]);
	   true ->
		   ignore
	end,
	%8强发生变动，记录战报
	if NewRank =< 10 andalso NewRank < SrcRank ->
		   record_first_eight_replay(SrcPVP#p_pvp.roleName, TarPVP#p_pvp.roleName, SrcNewRank, TarNewRank, FightRecord);
	   true ->
		   ignore
	end,
	Reply = #sc_pvp_fight{result=1,fightInfo=[FightRecord],newRank=NewRank,addRepu=AddRep},
	%% 战报记录
	hist_server:hist_pvp(SrcRoleID, SrcPVP#p_pvp.roleName, TarRoleID, TarPVP#p_pvp.roleName, FightRecord, SrcNewRank, TarNewRank, AddRep),
	%% 添加仇人
	friend_server:add_enemy(TarRoleID, SrcRoleID),
	
	?unicast(SrcRoleID, Reply),
	PvpList = get_challenge_list(NewRank),
	?unicast(SrcRoleID, #sc_pvp_get_list{pvpList=PvpList,rank=NewRank}),
	?CATCH(role_task:send_dispach(SrcRoleID, {dispach_task,role_pvp_fight,NewRank})),
	behavior_pvp_fight:log( SrcRoleID,1,NewRank);

do_handle_info({fight_fail, FightRecord, SrcRoleID, SrcName, TarRoleID, SrcPVP, TarPVP,AddRep}) ->
	finish_fight(SrcRoleID, TarRoleID),	
	TarRank = get_rank(TarRoleID),
	TarPVPOld = get_pvp(TarRank),
	TarTitle = get_title_by_rank(TarRank),
	if TarPVP =:= [] ->
			TarPVP2 = TarPVPOld#p_pvp{rank=TarRank,title=TarTitle};
		true ->
			TarPVP2 = TarPVP#p_pvp{rank=TarRank,title=TarTitle}
	end,
	SrcRank = get_rank(SrcRoleID),
	SrcTitle = get_title_by_rank(SrcRank),
	SrcPVP2 = SrcPVP#p_pvp{rank=SrcRank,title=SrcTitle},
	set_pvp(TarRank, TarPVP2),
	set_pvp(SrcRank, SrcPVP2),
	?INFO("new SrcPVP:~w TarPVP:~w\n",[SrcPVP2,TarPVP2]),
	%% 战报记录
	hist_server:hist_pvp(SrcRoleID, SrcName, TarRoleID, TarPVP#p_pvp.roleName, FightRecord, 0, 0, AddRep),
	
	Reply = #sc_pvp_fight{result=1,fightInfo=[FightRecord],newRank=0,addRepu=AddRep},
	?unicast(SrcRoleID, Reply),
	PvpList = get_challenge_list(SrcRank),
	?unicast(SrcRoleID, #sc_pvp_get_list{pvpList=PvpList,rank=SrcRank}),
	?CATCH(role_task:send_dispach(SrcRoleID, {dispach_task,role_pvp_fight,0})),
	behavior_pvp_fight:log( SrcRoleID,0,SrcRank);

do_handle_info(dump_interval_tick) ->
    refresh_first10(),
	?CATCH(do_persist()),
	dump_interval_tick();
do_handle_info({func, Fun, Args}) ->
	?CATCH(apply(Fun,Args));
do_handle_info({init_data,List})->
	init_data(List,[]).
	

begin_fight(RoleID, TarRoleID) ->
	lock(RoleID),
	lock(TarRoleID).

finish_fight(RoleID, TarRoleID) ->
	unlock(RoleID),
	unlock(TarRoleID).

check_fight(TarRank, TarRoleID, RoleID) ->
	CurTarRank = get_rank(TarRoleID),
	?DEBUG("check fight TarRank:~w\n",[TarRank]),
	%% -1 无法识别,改成65535
	case (CurTarRank == TarRank orelse TarRank == 65535) of 
		true ->
			case is_lock(TarRoleID) of
				true ->
					{false,5};
				false ->
					case is_lock(RoleID) of
						true ->
							{false,4};
						false ->
							if TarRoleID =:= RoleID ->
								   {false, 6};
							   true ->
								   true
							end
					end
			end;
		false  ->
			{ok, Record} = do_get_list2(RoleID),
            ?unicast(RoleID, Record),
			{false,2}
	end.
												 

%% 初始化pvp排行榜数据
init_data() ->	
	EigthReplayList = db_sql:get_etc(?DB_ETC_KEY_EIGHT_REPLAY),
	List = db_sql:get_etc(?DB_ETC_KEY_PVP),
	init_data(List,EigthReplayList).

init_data([],EigthReplayList)->
	clear_data(),
	set_record_first_eight_replay(EigthReplayList);
init_data(List,EigthReplayList)->
	clear_data(),
	set_record_first_eight_replay(EigthReplayList),
	lists:foreach(fun({Rank, RoleID, Title}) ->
						  erlang:put(Rank, {RoleID, Title}),
						  erlang:put(RoleID, Rank)
				  end,
				  List),
	{KeyMax,_} = util:keymax(List, 1),
	?DEBUG("put lowerrank=~w",[KeyMax]),
	put(?lowest_rank,KeyMax),
	init_first10().


clear_data()->
	Ancestors = get('$ancestors'),
	InitialCall = get('$initial_call'),
	erase(),
	put('$ancestors',Ancestors),
	put('$initial_call',InitialCall),
	ok.
	

%% 注意：Rank与RoleID绝对不会重复，这个在tk_id中有保证
%% 获取玩家的排名
get_rank(RoleID) when is_integer(RoleID)->	
	case get(RoleID) of
		?undefined ->
			Rank = gen_lowest_rank(),
			if Rank > ?MAX_RANK ->
				   ignore;
			   true ->
					set_rank(RoleID, Rank),
				   RoleInfo = db_sql:get_roleInfo(RoleID),
				   Pvp=new_pvp(RoleInfo, Rank),
				   set_pvp(Rank, Pvp)
			end,
			Rank;
		Rank->
			Rank
	end;
get_rank(RoleInfo) when is_record(RoleInfo, role) ->
	#role{roleID=RoleID} = RoleInfo,
	case get(RoleID) of
		?undefined ->
			Rank = gen_lowest_rank(),
			if Rank > ?MAX_RANK ->
				   ignore;
			   true ->
					set_rank(RoleID, Rank),
				   Pvp=new_pvp(RoleInfo, Rank),
				   set_pvp(Rank, Pvp)
			end,
			Rank;
		Rank->
			Rank
	end.
	
set_rank(RoleID, Rank) ->
	catch role_lib:send_server(RoleID, {pvp_rank, Rank}),
	put(RoleID, Rank).

	
get_challenge_list(MyRank) when MyRank =< 6 ->
	case get_first10() of
		TenList when length(TenList) < 10 ->
			TenList;
		TenList ->
			lists:sublist(TenList, 6)++
				[E||#p_pvp{roleID=ID}=E<-lists:sublist(TenList, 7,4),not(tk_id:is_robot(ID))]
	end;
get_challenge_list(MyRank) when MyRank =< 10 ->
	case get_first10() of
		TenList when length(TenList) < 10 ->
			TenList;
		TenList ->
			[E||#p_pvp{roleID=ID}=E<-lists:sublist(TenList, MyRank-6), not(tk_id:is_robot(ID))] ++ 
				lists:sublist(TenList, MyRank-5, 5) ++
				[E||#p_pvp{roleID=ID}=E<-lists:sublist(TenList, MyRank, 11-MyRank), not(tk_id:is_robot(ID))]
	end;
get_challenge_list(MyRank) when MyRank =:= 11 ->
	[E||#p_pvp{roleID=ID}=E<-lists:sublist(get_first10(), MyRank-6), not(tk_id:is_robot(ID))] ++
		lists:sublist(get_first10(), 6, 5);
get_challenge_list(MyRank) when MyRank =< 15 ->
	[E||#p_pvp{roleID=ID}=E<-lists:sublist(get_first10(), MyRank-6), not(tk_id:is_robot(ID))] ++
		lists:sublist(get_first10(), MyRank-5, 16-MyRank) ++
		[get_pvp(E)||E<-lists:seq(11, MyRank)];
get_challenge_list(MyRank) when MyRank =< 9999 ->
	Interval = data_pvp:get(MyRank),
	F = fun(N) -> TarRank = MyRank - N*Interval, get_pvp(TarRank) end,
	First10 = [E||#p_pvp{roleID=ID}=E<-get_first10(), not(tk_id:is_robot(ID))],
	First10 ++ [F(5),F(4),F(3),F(2),F(1)];
get_challenge_list(_) ->
	RankList = random_fighter([]),
	First10 = [E||#p_pvp{roleID=ID}=E<-get_first10(), not(tk_id:is_robot(ID))],
	First10 ++ [get_pvp(E)||E<-RankList].

random_fighter([_,_,_,_,_]=E) ->
	lists:sort(E);
random_fighter(List) ->
	Rank = util:random_int(9900, 9999),
	case lists:member(Rank, List) of
		true ->
			random_fighter(List);
		false ->
			random_fighter([Rank|List])
	end.

init_pvp(RoleID, Title, Rank) ->
	case role_lib:get_rolePublic(RoleID) of
		#rolePublic{} = RolePublic ->
			#p_pvp{
				   head=RolePublic#rolePublic.head,
				   roleID=RoleID,
				   rank=Rank,
				   roleName=RolePublic#rolePublic.roleName,
				   level=RolePublic#rolePublic.level,
				   isMale=RolePublic#rolePublic.isMale,
				   title=Title,
				   fightPower=RolePublic#rolePublic.fightPower};
		_ ->
			#p_pvp{fightPower=0,isMale=true,level=1,rank=Rank,roleID=RoleID,roleName="",title=Title}
	end.


get_first10() ->
	case get(?first10) of
		?undefined ->
			First10 = [PVP || Rank <- lists:seq(1,10), PVP <- [get_pvp(Rank)], PVP=/= ?undefined],
			put(?first10, First10),
			First10;
		First10 ->
			First10
	end.

set_first10(First10) ->
	erlang:put(?first10, First10).

init_first10() ->
	First10 = [begin
				   Role = init_pvp(RoleID, Title, Rank),
				   put(Rank,Role),
				   Role
				   end
			  || Rank <- lists:seq(1,10), {RoleID,Title} <- [get(Rank)]],
	set_first10(First10).

refresh_first10() ->
    First10 = [PVP || Rank <- lists:seq(1,10), PVP <- [get_pvp(Rank)], PVP=/= ?undefined],
    put(?first10, First10).
	
	

%% 获取第Rank名主公的信息
get_pvp(Rank) ->
	?DEBUG("get_pvp..~w",[Rank]),
	case get(Rank) of
		?undefined ->
			?DEBUG("cjf get_pvp when check..~w",[Rank]),
			?undefined;
		{RoleID, Title} ->
			?DEBUG("get_pvp_info:~w,~w,~w\n", [Rank,RoleID,Title]),
			PVP = init_pvp(RoleID, Title,Rank),
			set_pvp(Rank, PVP),
			PVP;
		PVP ->
			PVP
	end.

set_pvp(Rank, PVP) when Rank =< 10 ->
	First10 = get_first10(),
	First10_2 = lists:keystore(Rank, #p_pvp.rank, First10, PVP),
	set_first10(First10_2),
	put(Rank, PVP);
set_pvp(Rank, PVP) ->
	put(Rank, PVP).			


%% 战斗锁，正在战斗中的单位是不能被请求挑战的
is_lock(RoleID) ->
	(get({?lock, RoleID})) =:= true.
		
lock(RoleID) ->
	put({?lock, RoleID}, true).
unlock(RoleID) ->
	erase({?lock, RoleID}).

unlock2(RoleID) ->
	gen_server:call(?MODULE, {unlock, RoleID}).

%% 获取当前最低排名
gen_lowest_rank() ->
	case get(?lowest_rank) of
		?undefined ->
			put(?lowest_rank,1),
			1;
		Rank ->		
			if Rank >= ?MAX_RANK ->
				   ?MAX_RANK+1;
			   true ->
				   NewRank = Rank+1,
				   put(?lowest_rank, NewRank),
				   NewRank
			end
	end.
			
%% 计算下次刷新官爵时间
get_next_refresh_title_sec() ->
	RefreshTime = data_common:get(title_refresh_time),
	Time=erlang:time(),
	if RefreshTime > Time ->
		   to_sec(RefreshTime)-to_sec(Time);
	   true ->
		   to_sec(RefreshTime)-to_sec(Time)+?ONE_DAY_SECONDS
	end.

to_sec({A,B,C}) ->
	A*3600+B*60+C.

do_refresh_title() ->
	Config = data_title:get(title_condition),
	lists:foreach(fun({Key,#p_pvp{roleID=RoleID,level=Level,rank=Rank,title=Title}=PVP}) ->
						  case cacl_title(Level, Rank, Config) of
							  Title ->
								  %%?ERR("Key:~w,pvp:~w",[Key,PVP]),
								  ignore;
							  Title2 ->
								  PVP2 = PVP#p_pvp{title=Title2},
								  put(Key, PVP2),
								  %%?ERR("Key:~w,pvp2:~w",[Key,PVP2]),
								  catch role_lib:send_server(RoleID, {title,Title2})
						  end;
					 ({Rank, {RoleID, Title}}) when Rank =< ?MAX_RANK ->
						  case role_lib:get_rolePublic(RoleID) of
							  #rolePublic{level=Level} ->
								  case cacl_title(Level, Rank, Config) of
									  Title ->
										%%?ERR("Rank:~w,RoleID:~w,Title:~w",[Rank,RoleID,Title]),
										  ignore;
									  Title2 ->								  
										  %%?ERR("Rank:~w,RoleID:~w,Title2:~w",[Rank,RoleID,Title2]),
										  put(Rank, {RoleID, Title2}),
										  catch role_lib:send_server(RoleID, {title,Title2})
								  end;
							  _ ->
								  ignore
						  end;
					 ({OtherKey,OtherCont}) ->
						  ?DEBUG("OtherKey:~w, OtherCont:~w",[OtherKey,OtherCont]),
						  ignore
				  end, erlang:get()).


cacl_title(Level, Rank, [{Title, NeedLevel, MinRank}|_Config]) when Level >= NeedLevel andalso Rank =< MinRank->
	Title;
cacl_title(Level, Rank, [_|Config]) ->
	cacl_title(Level, Rank, Config);
cacl_title(_,_,[]) ->
	0.

send_reward() ->
    LowestRank = gen_lowest_rank(),
    lists:foreach(
      fun(RoleID) ->
              case RoleID >= tk_id:robot_roleID_max() of
                  true ->
                      case erlang:get(RoleID) of
                          ?undefined ->
                              Rank = LowestRank,
                              Title = 0;
                          Rank ->
                              case erlang:get(Rank) of
                                  ?undefined ->
                                      Title = 0;
                                  #p_pvp{title=Title} ->
                                      next;
                                  {RoleID, Title} ->
                                      next
                              end
                      end,
                      send_day_tilte_reward(RoleID, Title, Rank);
                  false ->
                      next
              end
      end, db_sql:get_level_role_id_list(15, 240)).

send_day_tilte_reward(RoleID, Title, Rank) ->
    case data_title_reward:get(Title) of
        SellReward when erlang:is_record(SellReward, sell_reward) ->
            mail_server:send_sys_mail(RoleID, ?MAIL_PVP_DAY_REWARD, [Rank], "", SellReward);
        _ ->
            ?ERR("RoleID:~w, Title:~w, Rank:~w", [RoleID, Title, Rank])
    end.

dump_interval_tick() ->
	erlang:send_after( ?dump_interval_tick*1000,self(),dump_interval_tick). 

get_title_by_rank(Rank) ->
	case get_pvp(Rank) of
		#p_pvp{title=Title} ->
			Title;
		_ ->
			0
	end.

 record_first_eight_replay(AttackerName, DefenderName, SrcNewRank, TarNewRank, FightRecord)->
	ReplayUID = tk_id:gen_replayUID(?REPLAY_TYPE_PVP),
	?INFO("id=~1000p,result=~10000p",[ReplayUID,FightRecord]), 
	db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_PVP),
	Info = #p_pvp_replay_info{attackerName=AttackerName, defenderName=DefenderName, attackerNewRank=SrcNewRank, defenderNewRank=TarNewRank, time=util:now(), replayUID=ReplayUID},
	add_record_first_eight_replay(Info).

get_record_first_eight_replay()->
	case erlang:get(?first8replay) of
		X  when is_list(X)->
			X;
		_ ->
			[]
	end.

set_record_first_eight_replay(InfoList)->
	erlang:put(?first8replay, InfoList).

add_record_first_eight_replay(Info)->
	InfoList = get_record_first_eight_replay(),
	InfoList2 = 
		case length(InfoList) < ?MAX_EIGHT_REPLAY_NUM of
			true ->
				[Info|InfoList];
			_ ->
				{InfoList3, DelList} = lists:split(?MAX_EIGHT_REPLAY_NUM-1, InfoList),
				lists:foreach(fun(Elem) -> 
									  ReplayUID = Elem#p_pvp_replay_info.replayUID,
									  erlang:erase({?replayRecord, ReplayUID}),		%%清除缓存中多余战报
									  db_sql:del_fightReplayList(integer_to_list(ReplayUID), ?REPLAY_TYPE_PVP)		%%清除数据库中多余战报
							  end,DelList),
				[Info|InfoList3]
		end,
	set_record_first_eight_replay(InfoList2).

%%战报查询缓存
get_replay_record(ReplayUID)->
	case erlang:get({?replayRecord, ReplayUID}) of
		undefined ->
			case db_sql:get_fightReplay(ReplayUID, ?REPLAY_TYPE_PVP) of
				[]->
					{#sc_fight_request{actionList=[],fighterList=[],result=true}, 2};
				Rec->
					erlang:put({?replayRecord, ReplayUID}, Rec),
					{Rec, 1}
			end;
		Cached ->
			{Cached, 1}
	end.

get_pvp_rank_info(RoleID, Start, Num) ->
    List =
        lists:foldr(
          fun(Rank, Acc) ->
                  case get_pvp(Rank) of
                      ?undefined ->
                          Acc;
                      #p_pvp{roleID=RoleIDT,isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower,head=Head} ->
                          case tk_id:is_robot(RoleIDT) of
                              false ->
                                  [#p_rank_info{
                                                rank=Rank
                                                ,roleID=RoleIDT
                                                ,level=Level
                                                ,name=RoleName
                                                ,head=Head
                                                ,title=Title
                                                ,isMale=IsMale
                                                ,fightPower=FightPower
                                                ,chapterID=0
                                               }|Acc];
                              true ->
                                  Acc
                          end
                  end
          end, [], lists:seq(erlang:max(Start, 1), erlang:min(Num, 50))),
    ?unicast(RoleID, #sc_rank_info{rankType=?RANK_TYPE_PVP, type=0, list=List}).



