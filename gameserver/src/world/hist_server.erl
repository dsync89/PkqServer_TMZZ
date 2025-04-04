%% @author admin
%% @doc 战报进程
%% Created 2013-5-24

-module(hist_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_hist.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).

-export([hist_pvp/8,hist_rule/6]).

hist_pvp(SrcRoleID, SrcName, TarRoleID, TarRoleName, FightInfo,SrcNewRank,TarNewRank, AddRep) ->
	erlang:send(?MODULE, {log_pvp_fight,SrcRoleID, SrcName, TarRoleID, TarRoleName, FightInfo,SrcNewRank,TarNewRank, AddRep}).

hist_rule(TarRoleID, TarName, RoleID, FightInfo, AddScore, NewScore) ->
	%% @todo 过滤机器人
	case tk_id:is_robot(RoleID) of
		true ->
			ignore;
		false ->
			erlang:send(?MODULE, {log_rule, TarRoleID, TarName, RoleID, FightInfo, AddScore, NewScore})
	end.

clear_hist(RoleID)->
	erlang:send(?MODULE,{clear_hist,RoleID}).
role_login(RoleID)->
	erlang:send(?MODULE,{role_login,RoleID}).

get_rule_hist(RoleID) ->
    case catch gen_server:call(?MODULE, {role_rule_hist, RoleID}) of
        {ok, List} ->
            List;
        Exception ->
            ?ERR("Exception:~w", [Exception]),
            []
    end.

-define(DUMP_INTERVAL, 10).%写数据库的间隔,单位：秒
-define(TICK_INTERVAL, 60). % 检查进程buff大小的时间间隔，单位：秒
-define(HIST_SYNC_NUM, 20).% 一次发送的战报数量
-define(MAX_HIST_NUM, 20).% 最大战报数量
%% ===================Dict Key Begin =========================
-define(READ_LIST, read_list).%进程字典中缓存起来的等待写数据库的已读标记
-define(DEL_LIST, del_list).%进程字典中缓存起来的等待写入删除记录的列表
-define(ADD_LIST, add_list).%进程字典缓存起来的，新增邮件，等待写入数据库
-define(offlineList,offlineList). % 下线列表
%% ===================Dict Key End   =========================

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
	tick(),
	dump_tick(),
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
handle_call({role_rule_hist, RoleID}, _From, State) ->
    Reply = role_rule_hist(RoleID),
    {reply, Reply, State};
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
handle_info(tick, State) ->
	{memory,Memory }= erlang:process_info(self(),memory),
	MemoryByM = Memory div (1024*1024),
	MaxBuffSizeByM = data_setting:get(hist_process_max_buff_size),
	if MemoryByM >= MaxBuffSizeByM ->
		   clear_buff();
	   true ->
		   ignore
	end,
	erlang:garbage_collect(),
	tick(),
	{noreply, State, hibernate};
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
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.



-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


transform_list(List) ->
	[hist2p_hist(E) || E<-List].
hist2p_hist(#hist{histUID=HistUID,histType=HistType,name=Name,enemyID=EnemyID,time=Time,arg=Arg,addRepu=AddRepu}) ->
	#p_hist{histUID=HistUID,
			histType=HistType,
			name=Name,
			roleID=EnemyID,
			time=Time,
			arg=Arg,
            addRepu=AddRepu}.

%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info({client_msg, RoleID, #cs_hist_get_list{type=Type}}) ->
    if
        Type =:=?TYPE_PVP orelse Type =:= ?TYPE_RULE ->
            {ok, SendList, UnReadNum} = hist_get_list(RoleID, Type),
            ?unicast(RoleID,#sc_hist_get_list{type=Type,historyList=SendList,isDiscardData=false,unreadNum=UnReadNum});
        true ->
            ?unicast(RoleID, #sc_hist_get_list{type=Type,historyList=[]})
    end;
do_handle_info({client_msg, RoleID, #cs_hist_replay{histUID=HistUID, type=Type}}) ->
	if Type =:=?TYPE_PVP orelse Type =:= ?TYPE_RULE ->
		   case get_fightInfo(HistUID, Type) of
			   [] ->
				   ?unicast(RoleID,#sc_hist_replay{result=2,fightInfo=[]});
			   FightInfo ->
				   ?unicast(RoleID, #sc_hist_replay{result=1,fightInfo=[FightInfo]})
		   end;
	   true ->
		   ?unicast(RoleID,#sc_hist_replay{result=2,fightInfo=[]})		   
	end;
do_handle_info({log_pvp_fight,SrcRoleID, SrcRoleName, TarRoleID, TarRoleName, FightInfo,SrcNewRank,TarNewRank,AddRep}) ->
	Result = FightInfo#sc_fight_request.result,
	if Result ->
		   WinType = ?HIST_WIN;
	   true ->
		   WinType = ?HIST_FAIL
	end,
	add_hist(SrcRoleID, TarRoleID, TarRoleName, FightInfo, SrcNewRank, WinType bor ?HIST_PVP bor ?HIST_YOU_FIGHT, ?TYPE_PVP,AddRep),
	%% 过滤机器人
	case tk_id:is_robot(TarRoleID) of
		true ->
			ignore;
		false ->
			add_hist(TarRoleID, SrcRoleID, SrcRoleName, FightInfo, TarNewRank, WinType bor ?HIST_PVP bor ?HIST_YOU_BE_FIGHT, ?TYPE_PVP,0)
	end;
do_handle_info({log_rule, TarRoleID, TarName, RoleID, FightInfo, AddScore, NewScore}) ->
    Result = FightInfo#sc_fight_request.result,
    if Result ->
           WinType = ?HIST_WIN;
       true ->
           WinType = ?HIST_FAIL
    end,
    add_hist(RoleID, TarRoleID, TarName, FightInfo,AddScore, 
             WinType bor ?HIST_RULE bor ?HIST_YOU_FIGHT , ?TYPE_RULE, NewScore);
do_handle_info({clear_hist,RoleID})->
	add_offlineList(RoleID);
do_handle_info({role_login,RoleID})->
	del_offlineList(RoleID);
do_handle_info(dump_tick) ->
	?CATCH(do_write_db()),
	dump_tick();
do_handle_info(Info) ->
	throw({cannot_handle,Info}).

del_offlineList(RoleID)->
	set_offlineList(lists:delete(RoleID, get_offlineList())).
add_offlineList(RoleID)->
	set_offlineList([RoleID|get_offlineList()]).
set_offlineList(List)->
	erlang:put(?offlineList,List).
get_offlineList()->
	case erlang:get(?offlineList) of
		?undefined ->
			[];
		X ->
			X
	end.

hist_get_list(RoleID, Type) ->
    #d_hist{histList=List} = HistInfo = get_hist(RoleID,Type),
    {List2,UnReadNum, NewReadList, DelIDList} = init_client_hist(List, Type),
    add_delList(DelIDList, Type),
    add_readList(NewReadList),
    set_hist(RoleID, Type, HistInfo#d_hist{histList=List2,unreadNum=0}),
    SendList = transform_list(List2),
    {ok, SendList, UnReadNum}.

role_rule_hist(RoleID) ->
    {ok, SendList, _} = hist_get_list(RoleID, ?TYPE_RULE),
    {ok, SendList}.
	
add_hist(RoleID, HisRoleID, HisRoleName, FightInfo, Arg, HistType, Type, AddRep) ->
	HistUID = tk_id:gen_mailUID(),
	NowSec = util:now(),
	Hist = #hist{arg=Arg,histType=HistType,histUID=HistUID,name=HisRoleName,enemyID=HisRoleID,time=NowSec,isRead=false,addRepu=AddRep},
    #d_hist{histList=List,unreadNum=UN} = HistInfo = get_hist(RoleID, Type),
    if length(List) >= ?MAX_HIST_NUM ->
           {List2,DelList} = lists:split(?MAX_HIST_NUM-1, List),
           DelIDList = [E#hist.histUID||E<-DelList],
           add_delList(DelIDList, Type),
           DelUN = lists:foldl(fun(E,Acc) -> 
                                       if E#hist.isRead =:= false ->
                                              Acc+1;
                                          true ->
                                              Acc
                                       end
                               end, 0, DelList),
           UN2 = UN +1 -DelUN,
           List3 = [Hist|List2];
       true ->
           List3 = [Hist|List],
           UN2 = UN +1
    end,
    HistInfo2 = HistInfo#d_hist{histList=List3,unreadNum=UN2},
    set_hist(RoleID, Type, HistInfo2),
    case role_lib:is_online(RoleID) of
        true ->
            case Type =:= ?TYPE_RULE of
                true ->
                    ?unicast(RoleID, #sc_hist_new{list=transform_list([Hist])});
                false ->
                    ?unicast(RoleID, #sc_hist_unreadNum{type=Type,unreadNum=UN2})
            end;
        false ->
            next
    end,
    add_addList(RoleID, Hist, FightInfo, Type).

get_hist(RoleID,Type) ->
	case erlang:get({RoleID,Type}) of
		#d_hist{}=HistInfo ->
			HistInfo;
		_ ->
			HistInfo = get_db_hist(RoleID, Type),
			set_hist(RoleID, Type, HistInfo),
			HistInfo
	end.

get_db_hist(RoleID,Type) ->
	db_sql:get_histList(RoleID, Type).

set_hist(RoleID, Type, HistInfo) ->
	Key = {RoleID, Type},
	erlang:put(Key, HistInfo).

init_client_hist(CerMailList, Type) ->
    TimeLimit =
        case Type of
            ?TYPE_PVP ->
                0;
            ?TYPE_RULE ->
                Date = erlang:date(),
                NowDayWeek = calendar:day_of_the_week(Date),
                util:datetime_to_seconds({Date, {0, 0, 0}}) - ?ONE_DAY_SECONDS * (NowDayWeek - 1)
        end,
	init_client_hist(CerMailList, [], 0, [], [], TimeLimit).
init_client_hist([], Send, DecNum, NewReadList, DelList, _TimeLimit) ->
	{lists:reverse(Send), DecNum, NewReadList, DelList};
init_client_hist([M|List], Send, DecNum, NewReadList, DelList, TimeLimit) ->
    #hist{isRead=IsRead,histUID=HistUID,time=Time} = M,
    case Time < TimeLimit of
        true ->
            init_client_hist(List, Send, DecNum, NewReadList, [HistUID|DelList], TimeLimit);
        false ->
            if IsRead=:=true->
                   init_client_hist(List, [M|Send], DecNum, NewReadList, DelList, TimeLimit);
               true ->
                   init_client_hist(List, [M#hist{isRead=true}|Send], DecNum+1, [HistUID|NewReadList], DelList, TimeLimit)
            end
    end.


clear_buff() ->
	lists:foreach(fun({{RoleID,Type}=Key,#d_hist{}}) when is_integer(RoleID) andalso is_integer(Type) ->
						  erlang:erase(Key);
					 (_) ->
						  ignore
				  end, erlang:get()).

%% 检查本进程buff
tick() ->
	erlang:send_after(?TICK_INTERVAL*1000, self(), tick).

%% 写数据库时间间隔
dump_tick() ->
	erlang:send_after(?DUMP_INTERVAL*1000, self(), dump_tick).


get_readList() ->
	case erlang:get(?READ_LIST) of
		[_|_] = List ->
			List;
		_ ->
			[]
	end.

set_readList(List) ->
	erlang:put(?READ_LIST, List).

add_readList([]) -> ignore;
add_readList(List) ->
	set_readList(List++get_readList()).

get_delList(Type) ->
	case erlang:get({?DEL_LIST, Type}) of
		[_|_] = List ->
			List;
		_ ->
			[]
	end.
		
set_delList(List, Type) ->
	erlang:put({?DEL_LIST, Type}, List).

add_delList([], _Type) -> ignore;
add_delList(List, Type) ->
	set_delList(List++get_delList(Type), Type).
	
get_addList(Type) ->
	case erlang:get({?ADD_LIST,Type}) of
		[_|_] = List ->
			List;
		_ ->
			[]
	end.
		
set_addList(List, Type) ->
	erlang:put({?ADD_LIST, Type}, List).

add_addList(RoleID, Mail, FightInfo, Type) ->
	set_addList([{RoleID, Mail, FightInfo}|get_addList(Type)], Type).

clear_offlineList()->
	List=get_offlineList(),
	set_offlineList([]),
	lists:foreach(fun(E)->erlang:erase({E,?TYPE_PVP}),erlang:erase({E,?TYPE_RULE}) end, List).
	
do_write_db() ->
	clear_offlineList(),
	db_sql:read_hist(get_readList()),
	set_readList([]),
	lists:foreach(fun(Type) ->
						  db_sql:add_histList(get_addList(Type), Type),
						  set_addList([], Type),
						  db_sql:del_hist(get_delList(Type), Type),
						  set_delList([], Type)
				  end, ?TYPE_HIST_LIST).

get_fightInfo(MailUID, Type) ->
	Cache = get_addList(Type),
	case util:fun_find(fun({_,Mail,_}) ->
							   Mail#hist.histUID== MailUID
					   end, Cache) of
		false ->
			db_sql:get_hist_fightInfo(MailUID, Type);
		{_,_,FightInfo} ->
			FightInfo
	end.
	