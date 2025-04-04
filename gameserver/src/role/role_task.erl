%% @author crimoon26
%% @doc @todo Add description to role_task.


-module(role_task).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-include("def_role.hrl").
-include("def_task.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================
send_dispach(RoleID,Msg) when is_integer(RoleID)->
	role_lib:send_server(RoleID, {dispach_task,Msg});
send_dispach(RoleServerPid, Msg) when is_pid(RoleServerPid)->
	RoleServerPid ! {dispach_task,Msg}.

cs_task_get_info(_)->
	{MainTaskList,TodayTaskList,AchTaskList} = role_data:get_task_list_group(),
	NewAchTaskList1 = filter_ach_task_list(AchTaskList),
	NewMainTaskList = [r_task2p_task(E)||E<-MainTaskList],
	NewTodayTaskList = [r_task2p_task(E)||E<-TodayTaskList,E#r_task.status=/=?TASK_STATUS_COMMIT],
	NewAchTaskList = [r_task2p_task(E)||E<-NewAchTaskList1],
	?sendself(#sc_task_get_info{main_task_list=NewMainTaskList,today_task_list=NewTodayTaskList,ach_task_list=NewAchTaskList}).

cs_task_operate(#cs_task_operate{operate_code=OperateCode,task_id=TaskID})->
	case check_operate_task(OperateCode,TaskID) of
		{ok,Task,TRoleTask}->
			case OperateCode of
				1->
					do_change_task_status(Task,TRoleTask);
				2->
					do_commit_task(Task,TRoleTask)
			end;
		{error,ErrorCode}->
			?sendself(#sc_task_error{result=ErrorCode})
	end.


do_change_task_status(Task,TRoleTask)->
	case transaction(fun() -> change_task_status(Task,TRoleTask,1) end) of
		{atomic,{NewTask,RewardView,Add}}->
			apply_success_fun(),
			#r_task{task_id=TaskID,status=Status} = NewTask,
			#data_task{task_type=TaskType} = data_task:get(TaskID),
			case RewardView of
				[]->
					ignore;
				_->
					?sendself(#sc_task_operate{task_type=TaskType,task_id=TaskID,status=Status,reward=RewardView})
			end,
			send_notify_change(TaskType,[NewTask|Add],[]);
		{abort,Reason}->
			erase_success_fun(),
			?ERR("任务状态改变时发生错误:~w~n任务数据为:~w",[Reason,Task]),
			?sendself(#sc_task_error{result=5})
	end.

do_commit_task(Task,TRoleTask)->
	Fun = fun()->do_commit_task_1(Task,TRoleTask) end,
	case transaction(Fun) of
		{atomic,{TaskID,RewardView,AddTaskList,DeleteTaskIDList}}->
			apply_success_fun(),
			#data_task{task_type=TaskType} = data_task:get(TaskID),
            role_task:send_notify_change(TaskType,AddTaskList,DeleteTaskIDList),
			case RewardView of
				[]->
					ignore;
				_->
					?sendself(#sc_task_operate{task_type=TaskType,task_id=TaskID,status=?TASK_STATUS_FINISH,reward=RewardView})
			end;
		{abort,Reason}->
			erase_success_fun(),
			?ERR("提交时发生错误:~w~n任务数据为:~w",[Reason,Task]),
			?sendself(#sc_task_error{result=5})
		end.

do_commit_task_1(Task,TRoleTask)->
	{TaskID,DeleteTaskIDList,AddTaskList,Reward} = commit_task(Task,TRoleTask),
	RewardView =
		case Reward of
			[]->
				[];
			_->
				LogType = ?MONEY_ADD_TYPE_TASK_COMMIT,
				Role = role_data:get_roleInfo(),
				RewardFun = fun()->
									role_reward:handle_sys_reward(Role, Reward, LogType, TaskID, "")
							end,
				add_success_fun(RewardFun),
				role_reward:transform2p_reward_view(Reward, [])
		end,
	{TaskID,RewardView,AddTaskList,DeleteTaskIDList}.

commit_task(Task,TRoleTask)->
	#r_task{task_id=TaskID} = Task,
	#data_task{task_type=TaskType,trigger_task_id_list=TriggerTaskIDList,reward=Reward} = data_task:get(TaskID),
	AddTaskList = add_task(TriggerTaskIDList),
	{NewAddTaskList,DeleteTaskIDList,NewTRoleTask} =
		case TaskType of
			?TASK_TYPE_ACH->
				{[Task#r_task{status=?TASK_STATUS_COMMIT}|AddTaskList],[],TRoleTask};
			?TASK_TYPE_TODAY->
				{[Task#r_task{status=?TASK_STATUS_COMMIT}|AddTaskList],[TaskID],TRoleTask};
			_->
				{AddTaskList,[TaskID],TRoleTask}
		end,
	role_data:set_task_list(TaskType,NewAddTaskList++NewTRoleTask),
	{TaskID,DeleteTaskIDList,NewAddTaskList,Reward}.

change_task_status(Task,TRoleTask,Step)->
	#r_task{task_id=TaskID,status=Status,trigger_id=TriggerID} = Task,
	#data_task{trigger_id=TriggerID,trigger_int_list=TriggerIntList,max_step=MaxStep,trigger_num=Num,step_reward=StepReward,task_type=TaskType,type=Type} = data_task:get(TaskID),
	NextStatus = get_status(erlang:min(Status+Step,?TASK_STATUS_FINISH),MaxStep),
	case NextStatus of
		?TASK_STATUS_FINISH->
			RewardView1 = [],
			NewTask = Task#r_task{status=NextStatus,trigger_num=Num,trigger_notes=[]},
			remove_trigger_task_id(TriggerID, TaskID),
			case TaskType of
				?TASK_TYPE_ACH->
					case role_data:get_ach_task_next(TaskID) of
						?undefined->
							Add = [];
						TID->
							{value,NextTask,_} = util:fun_take(fun(#r_task{task_id=TID1})->TID1=:=TID end, TRoleTask),
							role_data:set_ach_curr_taskID(Type, TID),
							case NextTask#r_task.status>=?TASK_STATUS_FINISH of
								true->
									Add = [];
								false->
									Add = [NextTask]
							end
					end;
				_->
					Add=[]
			end;
		?TASK_STATUS_WAS_ACCEPT->
			Add = [],
			case role_task_trigger:get_trigger_num(TriggerID, Num, TriggerIntList) of
				finish->
					RewardView1 =add_step_reward_fun(TaskID,?TASK_STATUS_FINISH,StepReward),
					NewTask = Task#r_task{status=?TASK_STATUS_FINISH,trigger_num=Num,trigger_notes=[]};
				{finish,_TriggerNotes}->
					RewardView1 =add_step_reward_fun(TaskID,?TASK_STATUS_FINISH,StepReward),
					NewTask = Task#r_task{status=?TASK_STATUS_FINISH,trigger_num=Num,trigger_notes=[]};
				CurrNum when is_integer(CurrNum)->
					RewardView1 = [],
					NewTask = Task#r_task{status=NextStatus,trigger_num=CurrNum},
					add_trigger_task_id(TriggerID, TaskID);
				{CurrNum,TriggerNotes}->
					RewardView1 = [],
					NewTask = Task#r_task{status=NextStatus,trigger_num=CurrNum,trigger_notes=TriggerNotes},
					add_trigger_task_id(TriggerID, TaskID)
			end
	end,
	RewardView = add_step_reward_fun(TaskID, NextStatus, StepReward),
	role_data:set_task_list(TaskType,[NewTask|TRoleTask]),
	{NewTask,RewardView++RewardView1,Add}.

send_notify_change(TaskType,UpdateTaskList,DelIDList)->
	List = lists:foldl(fun(#r_task{task_id=TaskID,status=Status}=Task,Acc)->
						case is_notify_change(TaskID,Status) of
							true->
								[r_task2p_task(Task)|Acc];
							false->
								Acc
						end
				 end, [],UpdateTaskList),
	case DelIDList=:=[] andalso List=:=[] of
		true->
			ignore;
		false->
			?sendself(#sc_task_notify_change{task_type=TaskType,updata_task_list=List,del_task_id=DelIDList})
	end.

add_step_reward_fun(TaskID,NextStatus,StepReward)->
	case lists:keyfind(NextStatus, 1, StepReward) of
			false->
				[];
			{_,Reward}->
				case Reward of
					[]->
						[];
					_->
						add_reward_fun(TaskID,Reward)
				end
		end.

add_step_fun_1(TaskID,TaskType,Status,StepReward)->
	case lists:keyfind(Status, 1, StepReward) of
			false->
				[];
			{_,Reward}->
				case Reward of
					[]->
						[];
					_->
						LogType = ?MONEY_ADD_TYPE_TASK_COMMIT,
						Role = role_data:get_roleInfo(),
						RewardView = role_reward:transform2p_reward_view(Reward, []),
						RewardFun = fun()->
											role_reward:handle_sys_reward(Role, Reward, LogType, TaskID, ""),
											?sendself(#sc_task_operate{task_type=TaskType,task_id=TaskID,status=Status,reward=RewardView})
									end,
						add_success_fun(RewardFun)
				end
		end.

add_reward_fun(TaskID,Reward)->
	case Reward of
		[]->
			[];
		_->
			LogType = ?MONEY_ADD_TYPE_TASK_COMMIT,
			Role = role_data:get_roleInfo(),
			RewardFun = fun()->
								role_reward:handle_sys_reward(Role, Reward, LogType, TaskID, "")
						end,
			add_success_fun(RewardFun),
			role_reward:transform2p_reward_view(Reward, [])
	end.

transaction(Fun)->
	case get(is_in_transaction) of
		?undefined->
			task_data_backup(),
			try
				Value = Fun(),
				{atomic,Value}
			catch
				_:Reason->
					task_data_recover(),
					{abort,Reason}
			after
					erase_backup_data()
			end;
		_->
			throw(was_in_transaction)
	end.

add_success_fun(Fun)->
	case get(apply_fun_list) of
		?undefined->
			put(apply_fun_list,[Fun]);
		L->
			put(apply_fun_list,[Fun|L])
	end.
erase_success_fun()->
	erase(apply_fun_list).

apply_success_fun()->
	case erase(apply_fun_list) of
		?undefined->
			ok;
		L->
			lists:foreach(fun(Fun)->
								  Fun()
						  end, lists:reverse(L))
	end.


r_task2p_task(#r_task{task_id=TaskID,status=Status,trigger_num=Num})->
	#p_task{task_id=TaskID,status=Status,trigger_num=Num}.

add_task(TaskID) when is_integer(TaskID)->
	add_task([TaskID]);
add_task([])->
	[];
add_task(TaskIDList) when is_list(TaskIDList)->
	add_task_1(TaskIDList,[]).

add_task_1([],Acc)->
	Acc;
add_task_1([TaskID|TailTaskID],Acc)->
	#data_task{task_type=TaskType,trigger_id=TriggerID,auto_accept=AutoAccept,max_step=MaxStep,step_reward=StepReward} = data_task:get(TaskID),
	NewTask = 
		case is_auto_accept(TaskType,AutoAccept) of
			false->
				Status = get_status(?TASK_STATUS_NOT_ACCEPT,MaxStep),
				#r_task{task_id=TaskID,status=Status,trigger_id=TriggerID,trigger_num=0};
			true->
				Status = get_status(?TASK_STATUS_WAS_ACCEPT,MaxStep),
				#r_task{task_id=TaskID,status=Status,trigger_id=TriggerID,trigger_num=0}
		end,
	case Status  of
		?TASK_STATUS_WAS_ACCEPT->
			add_trigger_task_id(TriggerID, TaskID);
		?TASK_STATUS_FINISH->
			add_step_fun_1(TaskID,TaskType,?TASK_STATUS_WAS_ACCEPT,StepReward);
		_->
			ignore
	end,
	add_step_fun_1(TaskID,TaskType,Status,StepReward),
	add_task_1(TailTaskID,[NewTask|Acc]).
%%配置了autoaccept的或非主线任务都自动接受
is_auto_accept(TaskType,AutoAccept)->
	case AutoAccept of
		true->
			true;
		false->
			TaskType=/=1
	end.
get_status(Status,MaxStep)->
	case Status>MaxStep of
		true->
			?TASK_STATUS_FINISH;
		false->
			Status
	end.
hook_zero_clock()->
	AllTodayTaskList = add_all_today_task(),
	role_data:set_task_list(?TASK_TYPE_TODAY,AllTodayTaskList),
	send_notify_change(?TASK_TYPE_TODAY,AllTodayTaskList,[]).
%% 	?sendself(#sc_task_notify_change{task_type=?TASK_TYPE_TODAY,updata_task_list=remove_not_show_task_add_2p_task(AllTodayTaskList),del_task_id=[]}).
	

init_task_data(RoleID,LastLogoutTime)->
	DBList = db_sql:get_task(RoleID),
	{RoleTaskList,TodayRoleTaskList,AchTaskList} =
		case DBList of
			[]->
				TodayTaskList = add_all_today_task(),
				AList = add_all_ach_task(),
				case  [ID||ID<-data_task:get_list(),(data_task:get(ID))#data_task.task_type=:=?TASK_TYPE_MAIN] of
					[FirstTaskID|_]->
						AddTaskList = add_task(FirstTaskID),
						{AddTaskList,TodayTaskList,AList};
					[]->
						{[],TodayTaskList,AList}
				end;
			_->
				lists:foldl(fun(Task,{MAcc,TAcc,AList})->
								  #data_task{task_type=TaskType} = data_task:get(Task#r_task.task_id),
								  case TaskType of
									  ?TASK_TYPE_TODAY->
										  {MAcc,[Task|TAcc],AList};
									  ?TASK_TYPE_MAIN->
										  {[Task|MAcc],TAcc,AList};
									  ?TASK_TYPE_ACH->
										  {MAcc,TAcc,[Task|AList]}
								  end
						  end, {[],[],[]}, DBList)
		end,
	NewTodayRoleTaskList = refresh_today_task(TodayRoleTaskList,LastLogoutTime),
	NewTaskList = RoleTaskList++NewTodayRoleTaskList++AchTaskList,
	init_trigger_task(NewTaskList),
	role_data:set_task_list_group({RoleTaskList,NewTodayRoleTaskList,AchTaskList}),
	ok.

init_trigger_task(TaskList)->
	List = lists:foldl(fun(#r_task{trigger_id=TriggerID,task_id=TaskID,status=Status},Acc)->
						case Status of
							?TASK_STATUS_WAS_ACCEPT->
								case util:fun_take(fun({TID,_})->TriggerID=:=TID end,Acc) of
									false->
										[{TriggerID,[TaskID]}|Acc];
									{value,{_,L},TailAcc}->
										[{TriggerID,[TaskID|L]}|TailAcc]
								end;
							_->
								Acc
						end
				end, [], TaskList),
	init_trigger_task_1(List).

init_trigger_task_1([])->
	ignore;
init_trigger_task_1([{TriggerID,TaskIDList}|TailList])->
	case TriggerID of
		0->
			ignore;
		_->
			put({?TRIGGER_TASK_ID_LIST,TriggerID},lists:sort(fun(X1,X2)->X1>X2 end, TaskIDList))
	end,
	init_trigger_task_1(TailList).

%%备份
task_data_backup()->
	put(role_task_data_backup,role_data:get_task_list_group()).
erase_backup_data()->
	erase(role_task_data_backup).
%%从备份恢复
task_data_recover()->
	case erase(role_task_data_backup) of
		?undefined->
			ingore;
		Tup->
			role_data:set_task_list_group(Tup),
			TaskList = role_data:get_task_list(),
			init_trigger_task(TaskList)
	end.


refresh_today_task(TodayRoleTaskList,LastLogoutTime)->
	{Date,_} = util:seconds_to_datetime(LastLogoutTime),
	case Date =/= erlang:date() of
			true->
				add_all_today_task();
			false->
				lists:foreach(fun(#r_task{task_id=TaskID,trigger_id=TriggerID,status=Status})->
									  case Status of
											  ?TASK_STATUS_WAS_ACCEPT->
												   add_trigger_task_id(TriggerID, TaskID);
											  _->
												  ignore
										  end
							  end, TodayRoleTaskList),
				TodayRoleTaskList
	end.

add_all_today_task()->
	TaskIDList = data_task:get_list(),
	TodayTaskIDList =lists:filter(fun(TaskID)->
						 (data_task:get(TaskID))#data_task.task_type=:=?TASK_TYPE_TODAY
				 end, TaskIDList),
	add_task(TodayTaskIDList).

add_all_ach_task()->
	TaskIDList = data_task:get_list(),
	TodayTaskIDList =lists:filter(fun(TaskID)->
						 (data_task:get(TaskID))#data_task.task_type=:=?TASK_TYPE_ACH
				 end, TaskIDList),
	add_task(TodayTaskIDList).

persist_task_data(RoleID)->
	RoleTaskList = role_data:get_task_list(),
	db_sql:set_task(RoleID, RoleTaskList).

check_operate_task(OperateCode,TaskID)->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case check_operate_code(OperateCode) of
				true->
					case check_has_task(TaskID) of
						{error,ErrorCode}->
							{error,ErrorCode};
						{ok,Task,TRoleTask}->
							case check_task_operate_status(OperateCode, Task) of
								true->
									case check_level(TaskID) of
										true->
											{ok,Task,TRoleTask};
										false->
											{error,2}%%等级不足
									end;
								false->
									{error,4}%%不合法操作
							end
					end;
				{error,ErrorCode}->
					{error,ErrorCode}
			end;
		false ->
			{error, 255}
	end.

check_operate_code(OperateCode)->
	case OperateCode=:=1 orelse OperateCode=:=2 of
		true->
			true;
		false->
			{error,3}%%参数错误
	end.

check_level(TaskID)->
	#role{level=Level} = role_data:get_roleInfo(),
	#data_task{level_limit=MinLevel} = data_task:get(TaskID),
	Level>=MinLevel.
	
check_has_task(TaskID)->
	case data_task:get(TaskID) of
		?undefined->
			{error,1};%%任务不存在
		#data_task{task_type=TaskType,type=Type}->
			RoleTaskList = role_data:get_task_list(TaskType),
			CheckFun = fun(#r_task{task_id=TID}) ->
							   TID =:= TaskID
					   end,
			case util:fun_take(CheckFun, RoleTaskList) of
				false->
					{error,1};
				{value,Task,TRoleTask}->
					case TaskType of
						?TASK_TYPE_ACH->
							case role_data:get_ach_curr_taskID(Type)=:=TaskID orelse Task#r_task.status=:=?TASK_STATUS_FINISH of
								true->
									{ok,Task,TRoleTask};
								false->
									{error,1}
							end;
						_->
							{ok,Task,TRoleTask}
					end
			end
	end.

check_task_operate_status(OperateCode,#r_task{status=Status})->
	check_task_operate_status(OperateCode,Status);
check_task_operate_status(OperateCode,Status)->
	case OperateCode of
		1->
			Status =:= ?TASK_STATUS_NOT_ACCEPT;
		2->
			Status =:= ?TASK_STATUS_FINISH
	end.

get_trigger_task_id(TriggerID)->
	case get({?TRIGGER_TASK_ID_LIST,TriggerID}) of
		?undefined->
			[];
		L->
			L
	end.

add_trigger_task_id(0,_)->
	ignore;
add_trigger_task_id(TriggerID,TaskID)->
	List = get_trigger_task_id(TriggerID),
	put({?TRIGGER_TASK_ID_LIST,TriggerID},lists:sort(fun(X1,X2)->X1>X2 end, [TaskID|lists:delete(TaskID,List)])).

remove_trigger_task_id(TriggerID,TaskID)->
	List = get_trigger_task_id(TriggerID),
	case lists:delete(TaskID, List) of
		[]->
			erlang:erase({?TRIGGER_TASK_ID_LIST,TriggerID});
		L->
			put({?TRIGGER_TASK_ID_LIST,TriggerID},lists:sort(fun(X1,X2)->X1>X2 end, L))
	end.


is_notify_change(TaskID,Status)->
	#data_task{task_type=TaskType,type=Type} = data_task:get(TaskID),
	case TaskType of
		?TASK_TYPE_ACH->
			Status>=?TASK_STATUS_FINISH orelse role_data:get_ach_curr_taskID(Type) =:= TaskID;
		_->
			true
	end.

%%成就显示过滤
filter_ach_task_list(TaskList)->
	List = lists:foldl(fun(Task,Acc)->
						#r_task{task_id=TaskID} = Task,
						#data_task{type=Type} = data_task:get(TaskID),
						case util:fun_take(fun({T,_})->T=:=Type end, Acc) of
							false->
								[{Type,[Task]}|Acc];
							{value,{_,L},TailAcc1}->
								[{Type,[Task|L]}|TailAcc1]
						end
				end, [], TaskList),
	ShowTaskIDList = lists:map(fun({Type,L})->
						  NewL = lists:keysort(#r_task.task_id, L),
						  CurrTaskID = curr_task_id(NewL,0),
						  role_data:set_ach_curr_taskID(Type, CurrTaskID),
						  set_ach_task_relation(NewL),
						  CurrTaskID
				  end, List),
	lists:filter(fun(#r_task{task_id=TaskID,status=Status})->
						 Status>=?TASK_STATUS_FINISH orelse lists:member(TaskID, ShowTaskIDList)
				 end, TaskList).

curr_task_id([#r_task{task_id=TaskID,status=Status}|Tail],_)->
	case Status>= ?TASK_STATUS_FINISH of
		true->
			curr_task_id(Tail,TaskID);
		_->
			TaskID
	end;
curr_task_id([], TaskID)->
	TaskID.

set_ach_task_relation(List)->
	set_ach_task_relation(List,undefined).

set_ach_task_relation([],_)->
	ignore;
set_ach_task_relation([T|List],undefined)->
	set_ach_task_relation(List,T);
set_ach_task_relation([T|List],#r_task{task_id=TaskID})->
	#r_task{task_id=NextTaskID} = T,
	role_data:set_ach_task_next(TaskID, NextTaskID),
	set_ach_task_relation(List,T).