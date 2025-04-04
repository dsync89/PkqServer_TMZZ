%% @author caijunjun
%% @doc 大乱斗.


-module(melee_server).
-behaviour(gen_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_melee.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start_link/0]).

-compile(export_all).

-define(CHECK_NODE_CONNECTION_INTERVAL, 20000).
-define(update_timestamp, update_timestamp).

-record(state, {
                master_server       :: pid()        %% 
                ,sync_time = 0      :: integer()    %% 更新时间
               }).


start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

i() ->
    gen_server:call(?MODULE, i).    

%% ====================================================================
%% Behavioural functions 
%% ====================================================================


%% init/1
%% ====================================================================
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
    
    case db_sql:get_etc(?DB_ETC_KEY_MELEE) of 
        [{state, #state{} = State}] ->
            ok;
        _ ->
            State = #state{}
    end,
    
    SlaveServerName = melee_master_server:get_cross_server_name(data_setting:get(server_id),false, data_setting:get(platform)),
    global:register_name(SlaveServerName, erlang:self()),
    
    {ok, State}.


%% handle_call/3
%% ====================================================================
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
handle_call(i, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
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
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
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
do_handle_info({sync_melee_msg, {From, Msg}}, #state{master_server = MasterServer} = State) ->
    case catch global:send(MasterServer, {sync, From, Msg}) of
        Pid when erlang:is_pid(Pid) ->
            ok;
        {'EXIT', {badarg, _}} ->
            erlang:send(From, {melee_msg_return, {false, 2}})
    end,
    {noreply, State};
do_handle_info({get_melee_role_fighter, SrcRolePid, TarRoleID}, State) ->
    erlang:spawn(fun() ->
                         case catch role_data:get_otherRoleFighter(TarRoleID) of
                             {TarFighterList, TarLieuAdd} -> 
                                 case catch role_lib:get_rolePublic(TarRoleID) of
                                     #rolePublic{roleName = TarRoleName} ->
                                         erlang:send(SrcRolePid, {get_melee_role_fighter_return, TarFighterList,TarLieuAdd, TarRoleName});
                                     _ ->
                                         erlang:send(SrcRolePid, {get_melee_role_fighter_return, [],{0,0}, <<"">>})
                                 end;
                             _ ->
                                 erlang:send(SrcRolePid, {get_melee_role_fighter_return, [],{0,0}, <<"">>})
                         end
                 end),
    {noreply, State};
do_handle_info({sync_melee_master_server, NewMasterServer}, State) ->
    NewState = sync_master_server(NewMasterServer, State),
    {noreply, NewState};
do_handle_info({disconnect_with_master_server, MasterServer}, State) ->
    ?ERR("disconnect master server ~p", [MasterServer]),
    NewState = disconnect_master_server(MasterServer, State),
    {noreply, NewState};
do_handle_info({send_mail_reward, RoleID, SellReward, MailTemplateID, ArgList}, State) ->
    mail_server:send_sys_mail(RoleID, MailTemplateID, ArgList, "", SellReward),
    {noreply, State};
do_handle_info(_Info, State) ->
    {noreply, State}.

%% ====================================================================
%% internel functions
%% ====================================================================
sync_master_server(NewMasterServer, 
                   #state{master_server = OldMasterServer} = State) ->
    Now = util:now(),
    NewState = 
        if
            NewMasterServer =:= OldMasterServer ->
                State#state{sync_time = Now};
            true ->
                State#state{sync_time = Now, master_server = NewMasterServer}
        end,
    do_persist(NewState),
    NewState.

disconnect_master_server(OldMasterServer, 
                   #state{master_server = MasterServer} = State) ->
    NewState = 
        if MasterServer =:= OldMasterServer ->
               State#state{master_server = ?undefined, sync_time = util:now()};
           true ->
               State
        end,
    do_persist(NewState),
    NewState.
        

do_persist(State) ->
    Info = [{state, State}],
    db_sql:set_etc(?DB_ETC_KEY_MELEE,Info).

%% ====================================================================
%% test
%% ====================================================================
test_sign(RoleID) -> 
    erlang:spawn(
        fun() ->
            %% init role data
            RoleInfo = db_sql:get_roleInfo(RoleID),
            RoleTimes = #roleTimes{meleeSignTimes = 0, 
                                   lastMeleeSignDate = {1,2,3}},
            case role_data:get_otherRoleFighter(RoleID) of
                {FighterList, LieuAdd} ->
                    next;
                _ ->
                    FighterList = [],
                    LieuAdd = {0,0}
            end,

            role_data:robot_set_posList(FighterList),
            role_data:set_lieu_add_attr(LieuAdd),
            role_data:set_roleID(RoleID),
            role_data:set_roleInfo(RoleInfo),
            role_data:set_roleTimes(RoleTimes),

            sign_loop()
        end).

sign_loop() ->
    receive 
        melee_sign ->
            RoleID = role_data:get_roleID(),
            role_melee:call_master_server({melee_sign, RoleID}),
            start_fight_loop();
        _ ->
            ignore,
            sign_loop()
    end.

start_fight_loop() ->
    receive
        melee_fight ->
            fight_loop();
        stop ->
            stop;
        _ ->
            start_fight_loop()
    end.

%% fight per 10s
fight_loop() ->
    receive
        melee_fight ->
            do_fight(),
            fight_loop();
        stop ->
            stop
    after 10000 ->
        do_fight(),
        fight_loop()
    end.

do_fight() ->
    RoleID = role_data:get_roleID(),
    case role_melee:call_master_server({melee_fight, RoleID}) of
        {true, TarRoleID} ->
            case role_melee:get_fighter_info(TarRoleID) of
                ?undefined ->
                    case role_melee:get_fighter_info_1(3) of
                        ?undefined ->
                            io:format("~p can't find tar role~n", [RoleID]);
                        {ok, _, _, _} ->
                            io:format("~p find tar role ~p~n", [RoleID, TarRoleID])
                    end;
                {ok, _, _, _} ->
                    io:format("~p find tar role ~p~n", [RoleID, TarRoleID])
            end;
        {false, ErrorCode} ->
            io:format("select fighter ~p error ~n", [ErrorCode])
    end.

test_robot_sign(Max) when Max >= 1, Max =< 9999 ->
    RobotPids = [begin test_sign(RoleID) end || RoleID <- lists:seq(?ROLE_ID_BASE + 1, ?ROLE_ID_BASE + Max)],
    put(robot_sign, RobotPids),

    timer:sleep(1000),

    notify_robot(melee_sign).
    


notify_robot(Msg) ->
    lists:foreach(fun(RobotPid) ->
                        RobotPid ! Msg
                  end,
                  get(robot_sign)).

test_robot_fight() ->
    notify_robot(melee_fight).


test_robot_stop() ->
    notify_robot(stop).






