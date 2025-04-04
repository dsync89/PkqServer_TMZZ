%% @author admin
%% @doc  好友赠送体力和加好友请求控制

-module(enargy_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

-define(FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,friend_enargy_data_change_roleid_list).

%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit,true),
	dump_data(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
    {noreply, State}.

terminate(_Reason, _State) ->
	persist_change(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%检测是否可以加这个好友(1:可以 0：不可以)
can_add_friend(RoleEnargyFriendInfo,AddFriendID)->
	#friend_enargy{addFriendList=AddFriendList} = RoleEnargyFriendInfo,
	case lists:keyfind(AddFriendID,1,AddFriendList) of
		false->
			1;
		{_,S}->
			case (util:now()-S) >= (3 * ?ONE_DAY_SECONDS) of
				true->
					1;
				false->
					0
			end
	end.
%%检测是否可以给这个好友送体力
can_send_friend_enargy(RoleEnargyFriendInfo,FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
	#friend_enargy{toFriendList=ToFriendList} = RoleEnargyFriendInfo,
	case lists:keyfind(FriendRoleID, 1, ToFriendList) of
		false->
			1;
		{_,S,IsGive}->
			case IsGive of
				1->
					{{_,_,D},_} = util:seconds_to_datetime(S),
					{_,_,D1} = date(),
					case D=/=D1 of
						true->
							1;
						false->
							2
					end;
				_->
					0
			end
	end;
can_send_friend_enargy(undefined, _)->
	0.

%% 是否可以挑战好友
can_friend_fight(RoleEnargyFriendInfo, FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
    #friend_enargy{fightTimes=FightTimes, refreshFightDate=RefreshDate, fightList=FightList} = RoleEnargyFriendInfo,
    NowDate = erlang:date(),
    case RefreshDate of
        NowDate ->
            FightTimes2 = FightTimes,
            NewRefreshDate = RefreshDate;
        _ ->
            FightTimes2 = data_common:get(friend_fight_times),
            NewRefreshDate = NowDate
    end,
    FightList2 = lists:filter(fun({_, Timestamp}) ->
                                      {FightDate, _} = util:seconds_to_datetime(Timestamp),
                                      FightDate =:= NowDate
                              end, FightList),
    case lists:keyfind(FriendRoleID, 1, FightList2) of
        false ->
            NewFightList = [{FriendRoleID,util:now()}|FightList2];
        _ ->
            erlang:throw({false, 2}),
            NewFightList = FightList2
    end,
    case FightTimes2 > 0 of
        true ->
            NewFightTimes = FightTimes2 - 1;
        false ->
            erlang:throw({false, 3}),
            NewFightTimes = FightTimes2
    end,
    {true, RoleEnargyFriendInfo#friend_enargy{fightTimes=NewFightTimes, refreshFightDate=NewRefreshDate, fightList=NewFightList}};
can_friend_fight(undefined, _FriendRoleID) ->
    {false, 3}.

is_friend_fight(RoleEnargyFriendInfo, FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
    #friend_enargy{fightList=FightList} = RoleEnargyFriendInfo,
    case lists:keyfind(FriendRoleID, 1, FightList) of
        false->
            false;
        {_,S}->
            {Date,_} = util:seconds_to_datetime(S),
            NowDate = date(),
            case Date =/= NowDate of
                true->
                    false;
                false->
                    true
            end
    end;
is_friend_fight(undefined, _) ->
    false.

refresh_friend_fight(FightTimes, RefreshDate, FightList) ->
    NowDate = erlang:date(),
    case RefreshDate of
        NowDate ->
            NewFightTimes = FightTimes,
            NewRefreshDate = RefreshDate;
        _ ->
            NewFightTimes = data_common:get(friend_fight_times),
            NewRefreshDate = NowDate
    end,
    NewFightList = lists:filter(fun({_, Timestamp}) ->
                                      {FightDate, _} = util:seconds_to_datetime(Timestamp),
                                      FightDate =:= NowDate
                              end, FightList),
    {NewFightTimes, NewRefreshDate, NewFightList}.

%%判断是否可领取这个好友赠送的体力
can_give_friend_enargy(RoleEnargyFriendInfo,FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
	#friend_enargy{toMeList=ToMyList,giveTimes=GiveTimes,date=Date} = RoleEnargyFriendInfo,
	NewGiveTimes = get_give_times(GiveTimes,Date),
	case NewGiveTimes =< 0 of
		true->
			0;
		false->
			case lists:keyfind(FriendRoleID, 1, ToMyList) of
				false->
					0;
				_->
					1
			end
	end;
can_give_friend_enargy(undefined, _)->
	0.

get_send_second(RoleEnargyFriendInfo,FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
	#friend_enargy{toMeList=ToMyList} = RoleEnargyFriendInfo,
	case lists:keyfind(FriendRoleID, 1, ToMyList) of
		{_,S}->
			S;
		false->
			0
	end;
get_send_second(undefined, _)->
	0.

%%得到今日送的次数
get_give_time(undefined)->
	0;
get_give_time(RoleEnargyFriendInfo)->
	#friend_enargy{giveTimes=GiveTimes,date=Date} = RoleEnargyFriendInfo,
	get_give_times(GiveTimes,Date).

get_give_times(GiveTimes,Date)->
	case date() of
		Date->
			erlang:min(GiveTimes, 20);
		_->
			20
	end.

%%
get_fight_time(undefined)->
    0;
get_fight_time(RoleEnargyFriendInfo)->
    #friend_enargy{fightTimes=FightTimes,refreshFightDate=RefreshFightDate} = RoleEnargyFriendInfo,
    get_fight_times(FightTimes,RefreshFightDate).

get_fight_times(FightTimes,RefreshFightDate)->
    case erlang:date() of
        RefreshFightDate ->
            FightTimes;
        _ ->
            data_common:get(friend_fight_times)
    end.


%%得到信息
get_ets_friend_enargy(RoleID)->
	case ets:lookup(?ETS_ENARGY_FRIEND_DATA, RoleID) of
		[]->
			Info = db_sql:get_enargy_friend(RoleID),
			ets:insert(?ETS_ENARGY_FRIEND_DATA, Info),
			Info;
		[Info]->
			Info
	end.
update_ets_friend_enargy(RoleID,RoleEnargyFriendInfo)->
    OldRoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
	ets:insert(?ETS_ENARGY_FRIEND_DATA, RoleEnargyFriendInfo),
	mark_role_homestead_change(RoleID, calc_flag_list(OldRoleEnargyFriendInfo, RoleEnargyFriendInfo)).

calc_flag_list(#friend_enargy{toFriendList=ToFriendList1,
                              toMeList=ToMeList1,
                              addFriendList=AddFriendList1,
                              beAddFriendList=BeAddFriendList1,
                              date=Date1,giveTimes=GiveTimes1,
                              fightTimes=FightTimes1,fightList=FightList1,
                              refreshFightDate=RefreshFightDate1},
               #friend_enargy{toFriendList=ToFriendList2,
                              toMeList=ToMeList2,
                              addFriendList=AddFriendList2,
                              beAddFriendList=BeAddFriendList2,
                              date=Date2,giveTimes=GiveTimes2,
                              fightTimes=FightTimes2,fightList=FightList2,
                              refreshFightDate=RefreshFightDate2}) ->
    FlagList1 =
        case Date1 =/= Date2 orelse GiveTimes1 =/= GiveTimes2 of
            true ->
                [giveTimes];
            false ->
                []
        end,
    FlagList2 =
        case ToFriendList1 =/= ToFriendList2 of
            true ->
                [toFriendList|FlagList1];
            false ->
                FlagList1
        end,
    FlagList3 =
        case ToMeList1 =/= ToMeList2 of
            true ->
                [toMeList|FlagList2];
            false ->
                FlagList2
        end,
    FlagList4 =
        case AddFriendList1 =/= AddFriendList2 of
            true ->
                [addFriendList|FlagList3];
            false ->
                FlagList3
        end,
    FlagList5 =
        case BeAddFriendList1 =/= BeAddFriendList2 of
            true ->
                [beAddFriendList|FlagList4];
            false ->
                FlagList4
        end,
    FlagList6 =
        case FightList1 =/= FightList2 of
            true ->
                [fightList|FlagList5];
            false ->
                FlagList5
        end,
    FlagList7 =
        case RefreshFightDate1 =/= RefreshFightDate2 orelse FightTimes1 =/= FightTimes2 of
            true ->
                [fightTimes|FlagList6];
            false ->
                FlagList6
        end,
    FlagList7.

mark_role_homestead_change(_RoleID, [])->
    ignore;
mark_role_homestead_change(RoleID, FlagList)->
    case get(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST) of
        ?undefined->
            put(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,[{RoleID, FlagList}]);
        List->
            case lists:keyfind(RoleID, 1, List) of
                false ->
                    put(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,[{RoleID, FlagList}|List]);
                {RoleID, OldFlagList} ->
                    AddFlagList = FlagList -- OldFlagList,
                    case AddFlagList of
                        [] ->
                            ignore;
                        _ ->
                            NewFlagList = OldFlagList ++ AddFlagList,
                            put(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,lists:keyreplace(RoleID, 1, List, {RoleID, NewFlagList}))
                    end
            end
    end.

%% 持久化改变了的
dump_data() ->
	erlang:send_after(30*1000, self(), dump_data).

%% 好友挑战
friend_fight(RoleID, FriendRoleID) ->
    erlang:send(?MODULE, {friend_fight,RoleID,FriendRoleID}).

%%送体力
enargy_send(RoleID,FriendRoleID)->
	erlang:send(?MODULE, {enargy_send,RoleID,FriendRoleID}).
%%领取体力
enargy_give(RoleID,RoleIDList)->
	erlang:send(?MODULE, {enargy_give,RoleID,RoleIDList}).

enargy_add_friend(RoleID,FriendRoleID)->
	erlang:send(?MODULE, {enargy_add_friend,RoleID,FriendRoleID}).

enargy_remove_friend(RoleID,FriendRoleID)->
	erlang:send(?MODULE, {enargy_remove_friend,RoleID,FriendRoleID}).

enargy_role_online(RoleID)->
	erlang:send(?MODULE, {enargy_role_online,RoleID}).

do_handle_info(dump_data)->
	dump_data(),
	persist_change(),
    {Date, Time} = erlang:localtime(),
    case Time of
        {5, _, _} ->
            case erlang:get(clear_date) of
                Date ->
                    ok;
                _ ->
                    ets:delete_all_objects(?ETS_ENARGY_FRIEND_DATA),
                    erlang:put(clear_date, Date)
             end;
        _ ->
            ok
    end;
do_handle_info({friend_fight,RoleID,FriendRoleID}) ->
    RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
    case catch can_friend_fight(RoleEnargyFriendInfo, FriendRoleID) of
        {true, NewRoleEnargyFriendInfo} ->
            role_lib:send_server(RoleID, {route, role_friend, {do_friend_fight, FriendRoleID}}),
            update_ets_friend_enargy(RoleID, NewRoleEnargyFriendInfo);
        {false, Reason} ->
            ?unicast(RoleID, #sc_friend_fight{result=Reason})
    end;
do_handle_info({enargy_send,RoleID,FriendRoleID})->
	RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
	case can_send_friend_enargy(RoleEnargyFriendInfo, FriendRoleID) of
		1->
			FRoleEnargyFriendInfo = get_ets_friend_enargy(FriendRoleID),
			{NewRoleEnargyFriendInfo,NewFRoleEnargyFriendInfo} = send_enargy(RoleEnargyFriendInfo,FRoleEnargyFriendInfo),
			update_ets_friend_enargy(RoleID, NewRoleEnargyFriendInfo),
			update_ets_friend_enargy(FriendRoleID, NewFRoleEnargyFriendInfo),
			?unicast(RoleID, #sc_friend_send_enargy{roleID=FriendRoleID}),
			?unicast(FriendRoleID,#sc_friend_send_enargy_me{roleID=RoleID});
		_->
			?unicast(RoleID, #sc_friend_send_enargy{result=3})
	end;
do_handle_info({enargy_give,RoleID,RoleIDList})->
	case RoleIDList of
		[]->
			?unicast(RoleID, #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=2});
		_->
			RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
			#friend_enargy{toMeList=ToMeList,giveTimes=GiveTimes,date=Date} = RoleEnargyFriendInfo,
			NewRoleIDList1 = lists:filter(fun(Rid)->lists:keyfind(Rid, 1, ToMeList)=/=false end, RoleIDList),
			case NewRoleIDList1 of
				[]->
					?unicast(RoleID, #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=3});
				_->
					NewGiveTimes1 = get_give_times(GiveTimes, Date),
					case NewGiveTimes1 =< 0 of
						true->
							?unicast(RoleID, #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=4});
						_->
							NewRoleIDList = lists:sublist(NewRoleIDList1, NewGiveTimes1),
							GTimes = length(NewRoleIDList),
							NewGiveTimes = NewGiveTimes1 - GTimes,
							give_enargy(RoleEnargyFriendInfo,NewRoleIDList,NewGiveTimes,GTimes)
					end
			end
	end;
do_handle_info({enargy_add_friend,RoleID,FriendRoleID})->
	add_request(RoleID,FriendRoleID),
    add_be_request(RoleID,FriendRoleID),
    notice_new_add_friend(RoleID,FriendRoleID);
do_handle_info({enargy_remove_friend,RoleID,FriendRoleID})->
    remove_add_friend_list(RoleID,FriendRoleID),
    remove_be_add_friend_list(RoleID,FriendRoleID);
do_handle_info({enargy_role_online,RoleID})->
	RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
	#friend_enargy{toFriendList=ToFriendList,giveTimes=GiveTimes,date=Date, fightTimes=FightTimes, refreshFightDate=RefreshDate, fightList=FightList} = RoleEnargyFriendInfo,
	NewToFriendList = refresh_toFriendList(ToFriendList),
	NewGiveTimes = get_give_times(GiveTimes, Date),
    {NewFightTimes, NewRefreshDate, NewFightList} = refresh_friend_fight(FightTimes, RefreshDate, FightList),
	NewRoleEnargyFriendInfo = RoleEnargyFriendInfo#friend_enargy{toFriendList=NewToFriendList,giveTimes=NewGiveTimes,date=date(),
                                                                 fightTimes=NewFightTimes, refreshFightDate=NewRefreshDate, fightList=NewFightList},
	update_ets_friend_enargy(RoleID, NewRoleEnargyFriendInfo);

do_handle_info(_Info)->
	?ERR("无效消息:~p",[_Info]).

remove_add_friend_list(RoleID,FriendRoleID) ->
    FRoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
    #friend_enargy{addFriendList=AddFriendList} = FRoleEnargyFriendInfo,
    NewAddFriendList = lists:keydelete(FriendRoleID, 1, AddFriendList),
    NewFRoleEnargyFriendInfo = FRoleEnargyFriendInfo#friend_enargy{addFriendList=NewAddFriendList},
    update_ets_friend_enargy(RoleID, NewFRoleEnargyFriendInfo).

remove_be_add_friend_list(RoleID,FriendRoleID) ->
    Info = get_ets_friend_enargy(FriendRoleID),
    #friend_enargy{beAddFriendList=BeAddFriendList} = Info,
    NewBeAddFriendList = lists:keydelete(RoleID, 1, BeAddFriendList),
    NewInfo = Info#friend_enargy{beAddFriendList=NewBeAddFriendList},
    update_ets_friend_enargy(FriendRoleID, NewInfo).

add_request(RoleID,FriendRoleID) ->
    FRoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
    #friend_enargy{addFriendList=AddFriendList} = FRoleEnargyFriendInfo,
    NewAddFriendList = [{FriendRoleID,util:now()} | lists:keydelete(FriendRoleID, 1, AddFriendList)],
    NewFRoleEnargyFriendInfo = FRoleEnargyFriendInfo#friend_enargy{addFriendList=NewAddFriendList},
    update_ets_friend_enargy(RoleID, NewFRoleEnargyFriendInfo).

add_be_request(RoleID,FriendRoleID) ->
    EnargyFriendInfo = get_ets_friend_enargy(FriendRoleID),
    #friend_enargy{beAddFriendList=BeAddFriendList} = EnargyFriendInfo,
    NewBeAddFriendList = [{RoleID,util:now()} | lists:keydelete(RoleID, 1, BeAddFriendList)],
    NewEnargyFriendInfo = EnargyFriendInfo#friend_enargy{beAddFriendList=NewBeAddFriendList},
    update_ets_friend_enargy(FriendRoleID, NewEnargyFriendInfo).

notice_new_add_friend(RoleID,FriendRoleID) ->
    List = gen_friend_add_list([RoleID]),
    ?unicast(FriendRoleID, #sc_friend_new_add{list=List}).

gen_friend_add_list(RoleIDList) ->
    Now = util:now(),
    lists:map(fun(RoleID) ->
                      #rolePublic{
                                  roleName=RoleName,
                                  level=Level,
                                  isMale=IsMale,
                                  title=Title,
                                  fightPower=FightPower,
                                  head=Head
                                 } = role_lib:get_rolePublic(RoleID),
                      #p_friend_add{
                                    roleName=RoleName,
                                    roleID=RoleID,
                                    level=Level,
                                    isMale=IsMale,
                                    title=Title,
                                    fightPower=FightPower,
                                    head=Head,
                                    timestamp=Now}
              
              end, RoleIDList).

%%送体力
send_enargy(RoleEnargyFriendInfo,FRoleEnargyFriendInfo)->
	S = util:now(),
	#friend_enargy{toFriendList=ToFriendList,roleID=RoleID} = RoleEnargyFriendInfo,
	#friend_enargy{toMeList=ToMeList,roleID=FriendRoleID} = FRoleEnargyFriendInfo,
	NewToFriendList = refresh_toFriendList([{FriendRoleID,S,0}|lists:keydelete(FriendRoleID, 1, ToFriendList)]),
	NewRoleEnargyFriendInfo = RoleEnargyFriendInfo#friend_enargy{toFriendList=NewToFriendList},
	NewToMeList = [{RoleID,S}|lists:keydelete(RoleID, 1, ToMeList)],
	NewFRoleEnargyFriendInfo = FRoleEnargyFriendInfo#friend_enargy{toMeList=NewToMeList},
	behavior_friend_enargy:log(RoleID,FriendRoleID,2,1),
	{NewRoleEnargyFriendInfo,NewFRoleEnargyFriendInfo}.

%%领取并回赠
give_enargy(RoleEnargyFriendInfo,RoleIDList,NewGiveTimes,GTimes)->
	#friend_enargy{toMeList=ToMeList,roleID=RoleID} = RoleEnargyFriendInfo,
	NewToMeList =
		lists:foldl(fun(ID,Acc)->
							lists:keydelete(ID, 1, Acc)
					end, ToMeList, RoleIDList),
	DDate = date(),
	NewRoleEnargyFriendInfo1 = RoleEnargyFriendInfo#friend_enargy{giveTimes=NewGiveTimes,date=DDate,toMeList=NewToMeList},
 	NewRoleEnargyFriendInfo =
		lists:foldl(fun(Rid,REF)->
							FRoleEnargyFriendInfo = get_ets_friend_enargy(Rid),
							#friend_enargy{toFriendList=ToFriendList,roleID=FRoleID} = FRoleEnargyFriendInfo,
							case lists:keyfind(RoleID, 1, ToFriendList) of
								false->
									REF;
								{_,S,_}->
									{Date,_} = util:seconds_to_datetime(S),
									NewToFriendList =
										case Date of
											DDate->
												lists:keyreplace(RoleID, 1, ToFriendList, {RoleID,S,1});
											_->
												lists:keydelete(RoleID, 1, ToFriendList)
										end,
									NewFRoleEnargyFriendInfo1 = FRoleEnargyFriendInfo#friend_enargy{toFriendList=refresh_toFriendList(NewToFriendList)},
									case can_send_friend_enargy(REF, Rid) of
										1->
											{NewREF,NewFRoleEnargyFriendInfo} = send_enargy(REF, NewFRoleEnargyFriendInfo1),
											?unicast(Rid,#sc_friend_send_enargy_me{roleID=RoleID});
										_->
											NewREF = REF,
											NewFRoleEnargyFriendInfo = NewFRoleEnargyFriendInfo1
									end,
									CanSend = can_send_friend_enargy(NewFRoleEnargyFriendInfo, RoleID),
									SendRecord = #sc_frend_give_enargy_me{roleID=RoleID,canSend=CanSend},
									?unicast(Rid,SendRecord),
									behavior_friend_enargy:log(RoleID,FRoleID,1,1),
									update_ets_friend_enargy(Rid, NewFRoleEnargyFriendInfo),
									NewREF
							end
					end, NewRoleEnargyFriendInfo1,RoleIDList),
	update_ets_friend_enargy(RoleID, NewRoleEnargyFriendInfo),
	role_lib:send_server(RoleID, {route, role_friend,{enargy_give_reward,GTimes}}),
	?unicast(RoleID,#sc_friend_give_enargy{roleIDList=RoleIDList,giveTimes=NewGiveTimes,result=0}).

refresh_toFriendList(ToFriendList)->
	DDate = date(),
	lists:filter(fun({_,S,IsGive})->
						 case IsGive of
							 1->
								 {Date,_} = util:seconds_to_datetime(S),
								 case Date of
									 DDate->
										 true;
									 _->
										 false
								 end;
							 _->
								 true
						 end
				 end, ToFriendList).
	
persist_change()->
    case erase(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST) of
        ?undefined->
            ignore;
        []->
            ignore;
        List->
            Fun = fun()->
                          {GiveTimesData, ToFriendData, ToMeData, AddFriendData, BeAddFriendData, FriendFightData, FriendFightTimesData} =
                              lists:foldr(fun({RoleID, FlagList}, {Acc1, Acc2, Acc3, Acc4, Acc5, Acc6, Acc7}) ->
                                                  #friend_enargy{roleID=RoleID,toFriendList=ToFriendList,toMeList=ToMeList,addFriendList=AddFriendList,
                                                                 beAddFriendList=BeAddFriendList,date=Date,giveTimes=GiveTimes,
                                                                 fightTimes=FightTimes,fightList=FightList,refreshFightDate=RefreshFightDate} = get_ets_friend_enargy(RoleID),
                                                  case lists:member(giveTimes, FlagList) of
                                                      true ->
                                                          NewAcc1 = [[RoleID,db_sql:date(Date),GiveTimes]|Acc1];
                                                      false ->
                                                          NewAcc1 = Acc1
                                                  end,
                                                  case lists:member(toFriendList, FlagList) of
                                                      true ->
                                                          {Acc2List1, Acc2List2} = Acc2,
                                                          NewAcc2 = {[RoleID|Acc2List1], trans_toFriendList(RoleID, ToFriendList)++Acc2List2};
                                                      false ->
                                                          NewAcc2 = Acc2
                                                  end,
                                                  case lists:member(toMeList, FlagList) of
                                                      true ->
                                                          {Acc3List1, Acc3List2} = Acc3,
                                                          NewAcc3 = {[RoleID|Acc3List1], trans_toMeList(RoleID, ToMeList)++Acc3List2};
                                                      false ->
                                                          NewAcc3 = Acc3
                                                  end,
                                                  case lists:member(addFriendList, FlagList) of
                                                      true ->
                                                          {Acc4List1, Acc4List2} = Acc4,
                                                          NewAcc4 = {[RoleID|Acc4List1], trans_addFriendList(RoleID, AddFriendList)++Acc4List2};
                                                      false ->
                                                          NewAcc4 = Acc4
                                                  end,
                                                  case lists:member(beAddFriendList, FlagList) of
                                                      true ->
                                                          {Acc5List1, Acc5List2} = Acc5,
                                                          NewAcc5 = {[RoleID|Acc5List1], trans_beAddFriendList(RoleID, BeAddFriendList)++Acc5List2};
                                                      false ->
                                                          NewAcc5 = Acc5
                                                  end,
                                                  case lists:member(fightList, FlagList) of
                                                      true ->
                                                          {Acc6List1, Acc6List2} = Acc6,
                                                          NewAcc6 = {[RoleID|Acc6List1], trans_fightList(RoleID, FightList)++Acc6List2};
                                                      false ->
                                                          NewAcc6 = Acc6
                                                  end,
                                                  case lists:member(fightTimes, FlagList) of
                                                      true ->
                                                          NewAcc7 = [[RoleID,db_sql:date(RefreshFightDate),FightTimes]|Acc7];
                                                      false ->
                                                          NewAcc7 = Acc7
                                                  end,
                                                  {NewAcc1, NewAcc2, NewAcc3, NewAcc4, NewAcc5, NewAcc6, NewAcc7}
                                          end, {[], {[],[]}, {[],[]}, {[],[]}, {[],[]}, {[],[]}, []}, List),
                          db_sql:batch_set_giveTimes(GiveTimesData),
                          db_sql:batch_set_toFriendList(ToFriendData),
                          db_sql:batch_set_toMeList(ToMeData),
                          db_sql:batch_set_addFriendList(AddFriendData),
                          db_sql:batch_set_beAddFriendList(BeAddFriendData),
                          db_sql:batch_set_friendFightList(FriendFightData),
                          db_sql:batch_set_friendFightTimes(FriendFightTimesData)
                  end,
            spawn(Fun)
    end.

trans_toFriendList(RoleID, ToFriendList) ->
    [[RoleID,FriendID,Timestamp,IsGive]||{FriendID,Timestamp,IsGive}<-ToFriendList].

trans_toMeList(RoleID, ToMeList) ->
    [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-ToMeList].

trans_addFriendList(RoleID, AddFriendList) ->
    [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-AddFriendList].

trans_beAddFriendList(RoleID, BeAddFriendList) ->
    [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-BeAddFriendList].

trans_fightList(RoleID, FightList) ->
    [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-FightList].







