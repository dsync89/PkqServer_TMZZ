%% @author admin
%% @doc 战斗接口
%% Created 2013-2-26

%% 		-4			-5			-6 
%%		-1			-2			-3
%%		
%%
%%		 1			 2			 3
%%		 4			 5			 6


-module(role_fight).
-include("def_role.hrl").
-compile(export_all).
%% API functions

%% Internal functions
-export([new/6,new/9,get_result/1]).
-export([start/9]).

-define(reborn, reborn).

-type ger_dict() :: ?dict.
%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 战斗计算接口
%% 特殊处理交换出手顺序
new2(AttackerList, DefenderList, LieuAddA, LieuAddD) ->
    new(0,AttackerList, DefenderList, LieuAddA, LieuAddD, true).

new(AttackerList, DefenderList, LieuAddA, LieuAddD) ->
	new(0,AttackerList, DefenderList, LieuAddA, LieuAddD, false).

-spec new(RoleID::integer(),AttackerList :: [#ger{}], DefenderList :: [#ger{}],LieuAddA::{},LieuAddD::{}, IsChange::boolean()) -> 
		  {Result :: boolean(), 
		   FightRecord :: #sc_fight_request{}, 
		   StateList :: {AttackerStateList :: [ger_final_state()], 
						 DefenderStateList :: [ger_final_state()]
						}
		  }.

new(RoleID,AttackerList, DefenderList, LieuAddA, LieuAddD, IsChange) ->
	Ref = erlang:make_ref(), %%防止收到其他来自其他战斗的战斗结果,加上REF后,确保后面取到的消息是该次战斗请求的结果
	new(RoleID,IsChange,false,AttackerList, DefenderList,LieuAddA, LieuAddD, self(), Ref),
	get_result(Ref).

%% 特殊处理招财战斗，只打一回合，敌方不出手
new3(AttackerList, DefenderList, LieuAddA, LieuAddD) ->
    Ref = erlang:make_ref(), %%防止收到其他来自其他战斗的战斗结果,加上REF后,确保后面取到的消息是该次战斗请求的结果
    new(0,false,true,AttackerList, DefenderList,LieuAddA, LieuAddD, self(), Ref),
    get_result(Ref).

%% @doc 异步的战斗
new(RoleID,IsChange,IsGetCoin,AttackerList, DefenderList,LieuAddA, LieuAddD, From, Ref) ->
	Pid = spawn(fun server_loop/0),
	Pid ! {fight,RoleID,IsChange,IsGetCoin,From, Ref, AttackerList, DefenderList,LieuAddA,LieuAddD}.

%% @doc 异步获取战斗结果
get_result(Ref) ->
	receive
		{fight_result, Ref, FightResult} ->
			FightResult
	after 2*1000 ->
			exit("unknown fight error")
%%			获取不到战斗结果不抛出异常,而是根据战斗力获得结果,但是没有打斗过程
%% 			?ERR("unknown fight error"),
%% 			{false,#sc_fight_request{result=false,fighterList=[],actionList=[]},{[],[],[]}}
	end.

%%
server_loop() ->
	receive
		{fight,RoleID,IsChange,IsGetCoin,From, Ref, AttackerList, DefenderList,LieuAddA, LieuAddD} ->
			start(RoleID,IsChange,IsGetCoin,From, Ref, AttackerList, DefenderList,LieuAddA,LieuAddD)
	end.
		
%% @doc 战斗计算接口
-spec start(RoleID::integer(),IsChange::boolean(),IsGetCoin::boolean(),From :: pid(), Ref :: reference(), AttackerList :: [#ger{}], DefenderList :: [#ger{}], LieuAddA::{}, LieuAddD::{}) -> no_return().
start(RoleID,IsChange,IsGetCoin,From, Ref, AttackerList, DefenderList,{AtkAddA, HpAddA}, {AtkAddD, HpAddD}) ->
	%% 获取随机种子
	RS = util:gen_random_seed(),
	DefenderList2 = add_pos_sign(DefenderList),
	GerListT = AttackerList++DefenderList2,
	GerList = lists:foldr(fun(#ger{gerBase=Base}=Ger,Acc) -> 
								  if Base#gerBase.gerPos >= ?guardGerPos ->
										 ?ERR("ERR GerPos when Fight:~w",[Ger]),
										 Acc;
									 true ->
										 [Ger|Acc]
								  end
						  end, [], GerListT),
    case IsChange of
        false ->
            Queue = get_queue(GerList, IsGetCoin);
        true ->
	        Queue = get_queue2(GerList, IsGetCoin)
    end,
	GerDict = init_ger_dict(GerList),
	Queue2 = lists:reverse(
			   lists:foldl(fun(SrcGerPos,QueueAcc)->
								 SrcGer = get_ger(SrcGerPos, GerDict),
								 #gerBase{gerQuality=SrcGerQuality,gerTypeID=SrcGerTypeID} = SrcGer#ger.gerBase,
								 Skills = ger_lib:get_ger_skills(SrcGerTypeID, SrcGerQuality),
								 [{SrcGerPos,Skills}|QueueAcc]
						 end,[],Queue)),
	%?ERR("Queue:~w",[Queue2]),
	AddAttr = {add_attr,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	LieuAddA = AddAttr#add_attr{gerAttackAddtion=AtkAddA, gerHpMaxAddtion=HpAddA},
	LieuAddD = AddAttr#add_attr{gerAttackAddtion=AtkAddD, gerHpMaxAddtion=HpAddD},
	enter_skill(RoleID,IsChange,IsGetCoin,Queue2, GerDict, Queue2,  [], {From, Ref}, RS,LieuAddA, LieuAddD).

%% 登场技能释放
enter_skill(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail], GerDict, Queue,  ActionList, Arg, RS,LieuAddA, LieuAddD) ->
	SrcGer0 = get_ger(SrcGerPos, GerDict),
	SrcGer = 
		if SrcGerPos > 0 ->
			   append_enter_skill_attr(SrcGer0, LieuAddA);
		   true ->
			   append_enter_skill_attr(SrcGer0, LieuAddD)
		end,
	GerDictT = set_ger(SrcGer, GerDict),
	%?ERR("Tar:~w,\n~w,\nLieuAddA\n~wLieuAddD~w",[SrcGer0, SrcGer, LieuAddA, LieuAddD]),
	
	%#gerBase{gerQuality=SrcGerQuality,gerTypeID=SrcGerTypeID} = SrcGer#ger.gerBase,
	%% 判断是否释放登场技能
	%#gerSkill{enter=GerEnterSkills} = ger_lib:get_ger_skills(SrcGerTypeID, SrcGerQuality),
	#gerSkill{enter=GerEnterSkills}=Skills,
	{ActionsList ,RS2,GerDict2}= 
		lists:foldl(fun(SkillIDEnter,{ActionsListAcc,RSAcc,GerDict2Acc})-> 
							case data_skill:get(SkillIDEnter) of
								#data_skill{targetSelect=TarSelect,attackActionID=AttackActionID,defendActionID=DefendActionID,gerEnterOrGodAdd=AddAttr}->
									{TarGerList, RSAcc2} = select_target(TarSelect, SrcGer, GerDict, RSAcc),
									{GerDict2Acc2, DefendActionList, TarPosList} = 
										lists:foldl(fun(Tar, {GerDictAcc,ActionListAcc,TarPosListAcc}) ->
															TarPos = ?b(Tar,gerPos),
															Tar2 = append_enter_skill_attr(Tar, AddAttr),

															if DefendActionID =:= 0 ->
																   ActionListAcc2 = ActionListAcc;
															   true ->
																   ActionListAcc2=[?action(DefendActionID,TarPos,[],0,0,?STATE_DEFAULT)|ActionListAcc]
															end,

															GerDictAcc2 = set_ger(Tar2, GerDictAcc),
															{GerDictAcc2, ActionListAcc2, [TarPos|TarPosListAcc]}
													end, {GerDict2Acc, [], []}, TarGerList), 
									if AttackActionID =:= 0 ->
										   {DefendActionList ++ ActionsListAcc,RSAcc2,GerDict2Acc2};
									   true ->
										   SrcAction = [?action(AttackActionID,SrcGerPos, TarPosList, 0, 0, ?STATE_DEFAULT)],
										   {DefendActionList ++ SrcAction ++ ActionsListAcc,RSAcc2,GerDict2Acc2}
									end;
								_ ->
									{ActionsListAcc,RSAcc,GerDict2Acc}
							end
					end, {ActionList,RS,GerDictT}, GerEnterSkills),
	enter_skill(RoleID,IsChange,IsGetCoin,Tail, GerDict2, Queue, ActionsList, Arg, RS2,LieuAddA, LieuAddD);
enter_skill(RoleID,IsChange,IsGetCoin,[], GerDict, Queue, ActionList, {From, Ref}, RS,_,_) ->
	GerDict2 = map_ger_dict(fun recacl_enter_skill_attr/1, GerDict, Queue),
	FighterList = [ger2p_fighter(Ger)||{_,Ger}<-erlang:get(), is_record(Ger, ger)],
	?DEBUG("fighterlist=~w",[erlang:get()]),
	fight_loop(RoleID,IsChange,IsGetCoin,Queue, GerDict2, Queue, 0, ActionList, [], {FighterList, From, Ref}, RS).

recacl_enter_skill_attr(Tar) ->
	ger_attr:add_enter_skill_attr(Tar, Tar#ger.gerExtra).

append_enter_skill_attr(Tar, AddAttr) ->
	#ger{gerExtra=GerExtra} = Tar,
	case is_record(GerExtra, add_attr) of
		true ->
			AddAttr2 = ger_attr:append_add_attr(GerExtra, AddAttr);
		false ->
			AddAttr2 = AddAttr
	end,
	Tar#ger{gerExtra=AddAttr2}.

%%初始fight_loop
fight_loop(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail], GerDict, Queue, Count, ActionList, ReelAlready, Arg, RS) ->
	SrcGer = get_ger(SrcGerPos, GerDict),
	%% 判断是否处于击晕状态
	case lists:member(SrcGerPos, ReelAlready) of
		true ->
			%% 击晕者跳过一回合
			UnReelAction = ?action(?ACTION_UNREEL, ?b(SrcGer, gerPos), [], 0, 0, ?STATE_DEFAULT),
			fight_loop(RoleID,IsChange,IsGetCoin,Tail, GerDict, Queue, Count, [UnReelAction|ActionList], lists:delete(SrcGerPos, ReelAlready), Arg, RS);
		false ->
			%% 判断是否已死亡
			if SrcGer#ger.gerHp =< 0 ->
				   %% 出手队列也可以清掉这个武将
				   fight_loop(RoleID,IsChange,IsGetCoin,Tail, GerDict, lists:keydelete(SrcGerPos,1, Queue), Count, ActionList, ReelAlready, Arg, RS);
			   true ->
				   %% 选择技能
				   DataSkill = select_skill(SrcGer,Skills),

				   %% 计算目标选择
				   #data_skill{targetSelect=TarSelect,attackTimes=AttackTimes}=DataSkill,
				   {TarGerList, RS2} = select_target(TarSelect, SrcGer, GerDict, RS),
				   ?ERR("TarGerList====================~w",[TarGerList]),
				   	if AttackTimes>1 andalso TarSelect=/=single->
						   	?ERR("AttackTimes>1 andalso TarSelect=/=single->~w",[]),
							fight_loop(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail], GerDict, Queue, Count, ActionList, ReelAlready, Arg, RS2,DataSkill#data_skill{targetSelect=single},TarGerList,[]);
						true->
							%% 计算单次攻击流程
							{GerDict2, ReelAlready2, SrcDeadAction, DamageBackActionList, AbsorbActionList, AttackActionList, SrcAttackActionList, RS3} = 
								apply_attack(DataSkill, SrcGer, TarGerList, GerDict, ReelAlready, RS2, {SrcGerPos,Skills}),
							
							{Tail2, Queue2,ActionListReborn} = lists:foldl(fun({GerPos,_},Acc)->
																					do_reborn(GerPos, Acc, GerDict)
															end, {Tail, Queue,[]}, Queue),
							
							ActionList2 = ActionListReborn ++ SrcDeadAction ++ AbsorbActionList ++ DamageBackActionList ++ AttackActionList ++ SrcAttackActionList ++ ActionList,
							%?ERR("ActionList=~w",[DamageBackActionList ++ AbsorbActionList ++ AttackActionList ++ SrcAttackActionList]),
							%% 判断是否已经分出胜负
							case check_win(GerDict2, IsChange) of
								{ok, Result}->
									fight_over(RoleID,IsChange,GerDict2, ActionList2, Result, Arg);
								no ->			
									#gerSkill{god=GodSkills}=Skills,
									{GodTimes, _} = get_god_skill(SrcGerPos),
									if GodSkills =/= [] andalso GodTimes < 5 ->
											add_god_attr(SrcGerPos, GodSkills,GerDict);
										true -> 
											ignore
									end,
									if AttackTimes>1 andalso TarSelect=:=single->
											?ERR("1111111111~w",[]),
											[TarGerList0|TarGerList1]=TarGerList,
											?ERR("GerHpTarGerList0=======~w",TarGerList0),
											if TarGerList0#ger.gerHp > 0 ->
													fight_loop(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail2], GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3,DataSkill#data_skill{attackTimes=AttackTimes-1},[],TarGerList);
												true ->
													fight_loop(RoleID,IsChange,IsGetCoin,Tail2, GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3)
											end;
										true ->
											fight_loop(RoleID,IsChange,IsGetCoin,Tail2, GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3)
									end
							end
					end
			end
	end;
%% 重头开始依次发动攻击
fight_loop(RoleID,IsChange,IsGetCoin,[], GerDict, Queue, Count, ActionList, ReelAlready, Arg, RS) ->
	%% 计算是否超回合
	NewCount = Count+1,
	case check_exceed(NewCount, IsGetCoin) of
		true ->
			?INFO("exceed....NewCount=~w",[NewCount]),
			fight_over(RoleID,IsChange,GerDict, ActionList, false, Arg);
		false ->
			fight_loop(RoleID,IsChange,IsGetCoin,Queue, GerDict, Queue, Count+1, ActionList, ReelAlready, Arg, RS)
	end.
%%单一目标
fight_loop(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail], GerDict, Queue, Count, ActionList, ReelAlready, Arg, RS, DataSkill,[],TarGerList) ->
	SrcGer = get_ger(SrcGerPos, GerDict),
	%% 判断是否处于击晕状态
	case lists:member(SrcGerPos, ReelAlready) of
		true ->
			%% 击晕者跳过一回合
			UnReelAction = ?action(?ACTION_UNREEL, ?b(SrcGer, gerPos), [], 0, 0, ?STATE_DEFAULT),
			fight_loop(RoleID,IsChange,IsGetCoin,Tail, GerDict, Queue, Count, [UnReelAction|ActionList], lists:delete(SrcGerPos, ReelAlready), Arg, RS);
		false ->
			%% 判断是否已死亡
			if SrcGer#ger.gerHp =< 0 ->
				   %% 出手队列也可以清掉这个武将
				   fight_loop(RoleID,IsChange,IsGetCoin,Tail, GerDict, lists:keydelete(SrcGerPos,1, Queue), Count, ActionList, ReelAlready, Arg, RS);
			   true ->
				   %% 计算目标选择
				   #data_skill{attackTimes=AttackTimes}=DataSkill,
				   
				   
				   %% 计算单次攻击流程
				   {GerDict2, ReelAlready2, SrcDeadAction, DamageBackActionList, AbsorbActionList, AttackActionList, SrcAttackActionList, RS3} = 
					   apply_attack(DataSkill, SrcGer, TarGerList, GerDict, ReelAlready, RS, {SrcGerPos,Skills}),
				   
				   {Tail2, Queue2,ActionListReborn} = lists:foldl(fun({GerPos,_},Acc)->
																		  do_reborn(GerPos, Acc, GerDict)
												 end, {Tail, Queue,[]}, Queue),

				   ActionList2 = ActionListReborn ++ SrcDeadAction ++ AbsorbActionList ++ DamageBackActionList ++ AttackActionList ++ SrcAttackActionList ++ ActionList,
				   %?ERR("ActionList=~w",[DamageBackActionList ++ AbsorbActionList ++ AttackActionList ++ SrcAttackActionList]),
				   %% 判断是否已经分出胜负
				   case check_win(GerDict2, IsChange) of
					   {ok, Result}->
						   fight_over(RoleID,IsChange,GerDict2, ActionList2, Result, Arg);
					   no ->			
						   #gerSkill{god=GodSkills}=Skills,
						   {GodTimes, _} = get_god_skill(SrcGerPos),
						   if GodSkills =/= [] andalso GodTimes < 5 ->
								  add_god_attr(SrcGerPos, GodSkills,GerDict);
							  true -> 
								  ignore
						   end,
						   if AttackTimes>1 ->
								[TarGerList0|TarGerList1]=TarGerList,
								?ERR("GerHpTarGerList0=======~w",TarGerList0),
								if TarGerList0#ger.gerHp > 0 ->
										fight_loop(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail2], GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3,DataSkill#data_skill{attackTimes=AttackTimes-1},[],TarGerList);
									true ->
										fight_loop(RoleID,IsChange,IsGetCoin,Tail2, GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3)
								end;
							  true ->
								fight_loop(RoleID,IsChange,IsGetCoin,Tail2, GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3)
						   end
				   end
			end
	end;
%%多目标
fight_loop(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail], GerDict, Queue, Count, ActionList, ReelAlready, Arg, RS, DataSkill,[TarGerList_tuple|TarGerListAll],[]) ->
	TarGerList = [TarGerList_tuple],
	?ERR("5555555555555555~w",[]),
	SrcGer = get_ger(SrcGerPos, GerDict),
		?ERR("666666666666666~w",[]),
	%% 判断是否处于击晕状态
	case lists:member(SrcGerPos, ReelAlready) of
		true ->
				?ERR("77777777777777~w",[]),
			%% 击晕者跳过一回合
			UnReelAction = ?action(?ACTION_UNREEL, ?b(SrcGer, gerPos), [], 0, 0, ?STATE_DEFAULT),
			fight_loop(RoleID,IsChange,IsGetCoin,Tail, GerDict, Queue, Count, [UnReelAction|ActionList], lists:delete(SrcGerPos, ReelAlready), Arg, RS);
		false ->
				?ERR("888888888888~w",[]),
			%% 判断是否已死亡
			if SrcGer#ger.gerHp =< 0 ->
				   %% 出手队列也可以清掉这个武将
				   fight_loop(RoleID,IsChange,IsGetCoin,Tail, GerDict, lists:keydelete(SrcGerPos,1, Queue), Count, ActionList, ReelAlready, Arg, RS);
			   true ->
				   	?ERR("999999999999~w",[]),
				   %% 计算目标选择
				   #data_skill{attackTimes=AttackTimes}=DataSkill,
				   
				   	?ERR("10=================~w",[]),
				   %% 计算单次攻击流程
				   {GerDict2, ReelAlready2, SrcDeadAction, DamageBackActionList, AbsorbActionList, AttackActionList, SrcAttackActionList, RS3} = 
					   apply_attack(DataSkill, SrcGer, TarGerList, GerDict, ReelAlready, RS, {SrcGerPos,Skills}),
				   	?ERR("11=========~w",[]),
				   {Tail2, Queue2,ActionListReborn} = lists:foldl(fun({GerPos,_},Acc)->
																		  do_reborn(GerPos, Acc, GerDict)
												 end, {Tail, Queue,[]}, Queue),
					?ERR("12=================~w",[]),
				   ActionList2 = ActionListReborn ++ SrcDeadAction ++ AbsorbActionList ++ DamageBackActionList ++ AttackActionList ++ SrcAttackActionList ++ ActionList,
				   %?ERR("ActionList=~w",[DamageBackActionList ++ AbsorbActionList ++ AttackActionList ++ SrcAttackActionList]),
				   %% 判断是否已经分出胜负
				   case check_win(GerDict2, IsChange) of
					   {ok, Result}->
							?ERR("14============~w",[]),
						   fight_over(RoleID,IsChange,GerDict2, ActionList2, Result, Arg);
					   no ->			
							?ERR("15===========~w",[]),
						   #gerSkill{god=GodSkills}=Skills,
														?ERR("16===========~w",[]),
						   {GodTimes, _} = get_god_skill(SrcGerPos),
														?ERR("17===========~w",[]),
						   if GodSkills =/= [] andalso GodTimes < 5 ->
								  							?ERR("18===========~w",[]),
								  add_god_attr(SrcGerPos, GodSkills,GerDict);
							  true -> 
								  							?ERR("19===========~w",[]),
								  ignore
						   end,
							?ERR("20===========~w",[]),
						   if AttackTimes>1 ->
								if erlang:is_list(TarGerListAll) ->
									   ?ERR("21===========erlang:is_list(TarGerListAll)~w",[]);
									erlang:is_tuple(TarGerListAll)->
							   			?ERR("21===========erlang:is_tuple(TarGerListAll)~w",[]);
									true->
							   			?ERR("21===========truetruetruetruetruetrue~w",[])
								end,
								?ERR("erlang:length(TarGerListAll)============~w",erlang:length(TarGerListAll)),
								if erlang:length(TarGerListAll)>0 ->
										fight_loop(RoleID,IsChange,IsGetCoin,[{SrcGerPos,Skills}|Tail2], GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3,DataSkill#data_skill{attackTimes=AttackTimes-1},TarGerListAll,[]);
									true ->
										fight_loop(RoleID,IsChange,IsGetCoin,Tail2, GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3)
								end;
							  true ->
								fight_loop(RoleID,IsChange,IsGetCoin,Tail2, GerDict2, Queue2, Count, ActionList2, ReelAlready2, Arg, RS3)
						   end
				   end
			end
	end.
%% 释放god技能动作
add_god_attr(GerPos,GodSkills,GerDict)->
	Ger = get_ger(GerPos, GerDict),
	%% GerAddAttr 为初始状态 + 已统计过的addattr(addattr为gerExtra字段),计算时，将当前需增加的值和addattr合并，重新运算属性
	%% 属性运算完成后，将属性与初始状态一同使用登场计算获取武将各项属性值，
	%% 更新新武将的hp和sp为重算属性前上阵武将的hp和sp
	{_, GerAddAttr} = get_god_skill(GerPos),
	%GerAddAttr2 = GerAddAttr#ger{gerSp=Ger#ger.gerSp},
	{NewGer, GerAddAttr3} = lists:foldl(fun(SkillID,{GerAcc, GerAddAttrAcc})->
												case data_skill:get(SkillID) of
													#data_skill{gerEnterOrGodAdd=SkillAdd}->
														AddAttr2 = case ?CATCH(ger_attr:append_add_attr_no_Anti(GerAddAttrAcc#ger.gerExtra, SkillAdd)) of
																	   {'EXIT', _}=L->
																		   ?ERR("calc god wrong :GerAcc=~w, GerAddAttrAcc=~w \n SkillID=~w, SkillID=~w,Info=~w",
																				[GerAcc, GerAddAttrAcc, SkillID, SkillAdd,L]),
																		   L;
																	   L ->
																		   L
																   end,
										   {GerAcc#ger{gerExtra=AddAttr2}, GerAddAttrAcc#ger{gerExtra=AddAttr2}};
									   _ ->
										   {GerAcc, GerAddAttrAcc}
								   end
						   end, {Ger, GerAddAttr}, [role_fight:random_one_skill(GodSkills)]),
	%% 基础属性重算
	NewGer2 = ger_attr:add_enter_skill_attr(GerAddAttr, NewGer#ger.gerExtra),
	%% sp和hp值置为当前值
	NewGer3 = NewGer2#ger{gerSp=Ger#ger.gerSp,gerHp=Ger#ger.gerHp},
	%?ERR("what in ger:~w\n~w,\n~w,\n,~w",[GerAddAttr,Ger,NewGer, NewGer3]),
	record_ger_god_skill(GerPos, GerAddAttr3),
	set_ger(NewGer3, GerDict).


record_ger_god_skill(GerPos, Ger)->
	{N, _} = get_god_skill(GerPos),
	erlang:put({god,GerPos},  {N + 1, Ger}).

get_god_skill(GerPos)->
	case erlang:get({god,GerPos}) of
		?undefined ->
			{0, 0};
		X->
			X
	end.

%% 复活动作
do_reborn(GerPos, {TailAcc, QueueAcc, ActionAcc}=Acc, GerDict)->
	Ger = get_ger(GerPos, GerDict),
	if Ger#ger.gerHp =< 0 ->
		   {value, {_,#gerSkill{god2=God2Skill}},QueueAcc2} = lists:keytake(GerPos, 1, QueueAcc),
		   IsReborn = is_reborn(GerPos),
		   if God2Skill =:= [] orelse IsReborn ->
				  {lists:keydelete(GerPos,1, TailAcc),QueueAcc2,ActionAcc};
			  true ->
				  #data_skill{gerEnterOrGodAdd=#add_attr{gerHpMaxAddtion  =X}} = data_skill:get(random_one_skill(God2Skill)),
				  Add = erlang:trunc(Ger#ger.gerAttr#gerAttr.gerHpMax * X / 10000 ), 
				  ActionReborn = ?action(?ACTION_REBORN, ?b(Ger, gerPos), [], Add, 0, ?STATE_DEFAULT), % ActionID
				  Ger2 = Ger#ger{gerHp=Add},
				  set_reborn(GerPos),
				  set_ger(Ger2, GerDict),
				  %%?ERR("Add:~w, ### ,~w, ### ,~w",[Add,Ger2,get_ger(GerPos,GerDict)]),
				  {TailAcc, QueueAcc,[ActionReborn|ActionAcc]}
		   end;
	   true -> 
		   Acc 
	end.

-type ger_final_state() :: {GerID :: ?int32, GerHp :: ?int32, GerSp :: ?int8}.

hp_list() ->
	lists:sort([{Pos, Ger#ger.gerHp} || {Pos, Ger} <- erlang:get()]).

%% 结束结算
-spec fight_over(RoleID::integer(),IsChange::boolean(),GerDict :: ger_dict(), 
				 ActionList :: [#p_action{}], 
				 Result :: boolean(), 
				 {
				  FighterList :: [#p_fighter{}], 
				  From :: pid(),
				  Ref :: reference()
				 }
				) ->  no_return().
fight_over(RoleID,IsChange,GerDict, ActionList, Result, {FighterList, From, Ref}) ->
	FightRecord = #sc_fight_request{fighterList=FighterList,actionList=ActionList,result=Result},
	
	%% 获取剩余血量和怒气
    F = fun(Ger, {Acc1, Acc2, Acc3,Acc4,Acc5,Acc6}) ->
                case IsChange of
                    false ->
                        case ?b(Ger,gerPos) > 0 of 
                            true ->
                                case Ger#ger.gerHp>0 of
                                    true->
                                        {[state(Ger)|Acc1], Acc2,[Ger|Acc3],Acc4,Acc5,Acc6};
                                    false->
                                        {[state(Ger)|Acc1], Acc2,[Ger|Acc3],Acc4,add_born_list(?b(Ger,gerTypeID),Acc5),Acc6}
                                end;
                            false ->
                                case Ger#ger.gerHp>0 of
                                    true->
                                        {Acc1, [state(Ger)|Acc2], Acc3, [Ger|Acc4],Acc5,Acc6};
                                    false->
                                        {Acc1, [state(Ger)|Acc2], Acc3, [Ger|Acc4],Acc5,add_born_list(?b(Ger,gerTypeID),Acc6)}
                                end
                        end;
                    true ->
                        case ?b(Ger,gerPos) > 0 of 
                            false ->
                                case Ger#ger.gerHp>0 of
                                    true->
                                        {[state(Ger)|Acc1], Acc2,[Ger|Acc3],Acc4,Acc5,Acc6};
                                    false->
                                        {[state(Ger)|Acc1], Acc2,[Ger|Acc3],Acc4,add_born_list(?b(Ger,gerTypeID),Acc5),Acc6}
                                end;
                            true ->
                                case Ger#ger.gerHp>0 of
                                    true->
                                        {Acc1, [state(Ger)|Acc2], Acc3, [Ger|Acc4],Acc5,Acc6};
                                    false->
                                        {Acc1, [state(Ger)|Acc2], Acc3, [Ger|Acc4],Acc5,add_born_list(?b(Ger,gerTypeID),Acc6)}
                                end
                        end
                end
        end,	
	{SrcGerStateList,DGerStateList,SrcGerList,DGerList,_SrcDeadList,DDeadList} = fold_ger_dict(F, {[],[],[],[],[],[]}, GerDict),
	From ! {fight_result, Ref, {Result, FightRecord, {SrcGerStateList,DGerStateList,SrcGerList,DGerList}}},
	case DDeadList =:= [] orelse RoleID=:=0 of
		true->
			ignore;
		false->
			catch role_task:send_dispach(RoleID, {dispach_task,kill_monster,DDeadList})
	end,
	erlang:erase(),
	erlang:garbage_collect(),
	ok.
	%server_loop().

add_born_list(GerTypeID,List)->
	case util:fun_take(fun({GTID,_})->GerTypeID =:= GTID end, List) of
		false->
			[{GerTypeID,1}|List];
		{value,{_,N},TList}->
			[{GerTypeID,N+1}|TList]
	end.

%% 获取战斗结束时的状态
state(Ger) ->
	{Ger#ger.gerID,Ger#ger.gerHp,Ger#ger.gerSp}.

%% 战斗单位结构
ger2p_fighter(Ger) ->
	#ger{gerBase=GerBase,gerAttr=GerAttr}=Ger,
	GerPos = GerBase#gerBase.gerPos,
	#p_fighter{
			   gerID=Ger#ger.gerID,
			   gerTypeID=GerBase#gerBase.gerTypeID,
			   %gerSkill1=GerBase#gerBase.gerSkill1,
			   %gerSkill2=GerBase#gerBase.gerSkill2,
			   gerPos=GerPos,
			   %gerCamp=?sign(GerPos),
			   gerHp=Ger#ger.gerHp,
			   gerSp=Ger#ger.gerSp,
			   gerHpMax=GerAttr#gerAttr.gerHpMax,
			   gerQuality=trunc(GerBase#gerBase.gerQuality),
			   gerLevel=GerBase#gerBase.gerLevel}.			   

%% 判断是否已经分出胜负
check_win(GerDict, IsChange) ->
	%?ERR("hpList=~w",[hp_list()]),
	F = fun(_Ger, {no, no}=E) ->
				{return, E};
		   (Ger, {Acc1, Acc2}=E) ->
				if Ger#ger.gerHp > 0 ->
					if ?b(Ger, gerPos) > 0 ->
						{no, Acc2};
						true ->
							{Acc1, no}
					end;
				   true ->
					   E
				end
		end,					
	%% 判断是否死光了，判断各阵营单位是否全部死亡
    case IsChange of
        false ->
            case fold_ger_dict(F, {false, false}, GerDict) of
                {no,no} ->
                    no;
                {false, false} ->
                    no;
                {false, no} ->
                    {ok, false};
                {no, false} ->
                    {ok, true}
            end;
        true ->
            case fold_ger_dict(F, {false, false}, GerDict) of
                {no,no} ->
                    no;
                {false, false} ->
                    no;
                {false, no} ->
                    {ok, true};
                {no, false} ->
                    {ok, false}
            end
    end.
			
	
%% 判断回合数是否已超
check_exceed(Count, IsGetCoin) ->
    case IsGetCoin of
        false ->
	       Count >= 100;
        true ->
           Count >= data_coin_buy:get(count)
    end.

%% 对多个目标进行攻击
apply_attack(DataSkill, SrcGer, TarSelect, GerDict, ReelAlready, RS, QAcc) ->
	?ERR("+++++++++++++++++++=0~w",[]),
	SrcGerPos = ?b(SrcGer, gerPos),
		?ERR("+++++++++++++++++++=1~w",[]),
	?ERR("GerDict^^^^^^^^^^^^^^^^^^^=----1~w",GerDict),
		?ERR("ReelAlready^^^^^^^^^^^^^^^^^^^=----1~w",ReelAlready),
		?ERR("RS^^^^^^^^^^^^^^^^^^^=----1~w",RS),
		?ERR("TarSelect^^^^^^^^^^^^^^^^^^^=----1~w",TarSelect),
	%% 先运算物理伤害、魔法伤害
	{AttackActionList, GerDict2, MissUnmissList, ReelList2, Absorb, DamageBack, RS2} = 
		lists:foldl(fun(TarGer, {AttackActionAcc, GerDictAcc, MissUnmissAcc, ReelAcc, AbsorbAcc, DamageBackAcc, RandomSeed}) ->		
									?ERR("TarGer^^^^^^^^^^^^^^^^^^^=----1~w",TarGer),
								?ERR("^^^^^^^^^^^^^^^^^^^=----1~w",[]),
								?ERR("{AttackActionAcc, GerDictAcc, MissUnmissAcc, ReelAcc, AbsorbAcc, DamageBackAcc, RandomSeed}^^^^^^^^^^^^^^^^^^^=----1~w",{AttackActionAcc, GerDictAcc, MissUnmissAcc, ReelAcc, AbsorbAcc, DamageBackAcc, RandomSeed}),
							{Result, ReelState, TarGer2, AttackAction, AbsorbHp, DamageBackHp, RandomSeed2} = attack(DataSkill, SrcGer, TarGer, RandomSeed),
								?ERR("^^^^^^^^^^^^^^^^^^^=0~w",[]),
							TarGerPos = ?b(TarGer2, gerPos),
							?ERR("^^^^^^^^^^^^^^^^^^^=1~w",[]),
							GerDictAcc2 = set_ger(TarGer2, GerDictAcc),
							?ERR("^^^^^^^^^^^^^^^^^^^=2~w",[]),
							if ReelState =:= ?STATE_REEL ->
								   ?ERR("^^^^^^^^^^^^^^^^^^^=3~w",[]),
								   ReelAcc2 = [TarGerPos|lists:delete(TarGerPos, ReelAcc)];
							   true ->
								   ?ERR("^^^^^^^^^^^^^^^^^^^=4~w",[]),
								   ReelAcc2 = ReelAcc
							end,
							?ERR("^^^^^^^^^^^^^^^^^^^=5~w",[]),
							{[AttackAction|AttackActionAcc], GerDictAcc2, [Result|MissUnmissAcc], ReelAcc2, AbsorbHp+AbsorbAcc, DamageBackHp+DamageBackAcc, RandomSeed2}
					end, {[], GerDict, [], ReelAlready, 0, 0, RS}, TarSelect),
		?ERR("+++++++++++++++++++=2~w",[]),
	%% 再计算主动吸血action
	if Absorb =< 0 ->
		   AbsorbActionList = [];
	   true ->
		   AbsorbActionList = [?action(?ACTION_ABSORB, SrcGerPos, [], Absorb, 0, ?STATE_DEFAULT)]
	end,
		?ERR("+++++++++++++++++++=3~w",[]),
	%% 再运算反弹
	if DamageBack >= 0 ->
		   DamageBackActionList = [];
	   true ->
		   DamageBackActionList = [?action(?ACTION_DAMAGE_BACK, SrcGerPos, [], DamageBack, 0, ?STATE_DEFAULT)]
		   %?ERR("damagebackaction=~1000p",[hd(DamageBackActionList)])
	end,
	
		?ERR("+++++++++++++++++++=4~w",[]),
	%% 计算血量修改
	SrcAddHp = Absorb+DamageBack,
	%% 计算SrcGer增加的怒气
	SrcAddSp = cacl_add_sp(DataSkill, SrcGer, MissUnmissList),
		?ERR("+++++++++++++++++++=5~w",[]),
	%% 计算新的SrcGer
	SrcHp2 = add_hp(SrcGer#ger.gerHp,?a(SrcGer, gerHpMax),SrcAddHp),
    SrcAddSp2 = real_add_sp(SrcGer#ger.gerSp,SrcAddSp, (SrcGer#ger.gerAttr)#gerAttr.gerSpMax),
    SrcSp2 = SrcGer#ger.gerSp + SrcAddSp2,
	?DEBUG("攻击怒气：~w",[{DataSkill#data_skill.skillType,SrcGer#ger.gerSp,SrcSp2,(SrcGer#ger.gerBase)#gerBase.gerPos}]),
	SrcGer2 = SrcGer#ger{gerHp=SrcHp2, gerSp=SrcSp2},
	GerDict3 = set_ger(SrcGer2, GerDict2),
		?ERR("+++++++++++++++++++=6~w",[]),
	%% 攻击者死亡action
	if SrcHp2 =< 0 ->
		   {_,_,RebornAction} = do_reborn(SrcGerPos, {[QAcc],[QAcc],[]},GerDict),
		   if RebornAction =:= [] ->
				  SrcDeadAction = [?action(?ACTION_DEAD, SrcGerPos, [], 0, 0, ?STATE_DEFAULT)];
			  true ->
				  SrcDeadAction = RebornAction
		   end;
	   %%攻击者复活action
	   true ->
		   SrcDeadAction = []
	end,
		?ERR("+++++++++++++++++++=7~w",[]),
	%% 计算攻击者的Action
	TarPosList = [?b(TarGer,gerPos)||TarGer<-TarSelect],
		?ERR("+++++++++++++++++++=8~w",[]),
	SrcAttackAction = ?action(DataSkill#data_skill.attackActionID,SrcGerPos,TarPosList,0,SrcAddSp2,?STATE_DEFAULT),
		?ERR("+++++++++++++++++++=9~w",[]),
	{GerDict3, ReelList2, SrcDeadAction, DamageBackActionList, AbsorbActionList, AttackActionList, [SrcAttackAction], RS2}.

%% 计算攻击者的怒气改变
cacl_add_sp(DataSkill, _SrcGer, _MISS_UNMISS_LIST) ->
	if DataSkill#data_skill.skillType =:= unique orelse DataSkill#data_skill.skillType =:= unique2 ->
		   -100;
	   true ->
		   ?BEAT_ADD_SP
	end.
	
%% 已经复活过，则返回true
set_reborn(N)->
	erlang:put({?reborn,N},1).
is_reborn(N)->
	case erlang:get({?reborn,N}) of
		?undefined ->
			false;
		_ ->
			true
	end.
%% @doc 一次攻击
-spec attack(#data_skill{}, SrcGer :: #ger{}, TarGer :: #ger{}, RandomSeed :: tuple()) ->
		  {
		   Result :: miss | integer(), 
		   ReelState :: ?STATE_DEFAULT | ?STATE_REEL ,
		   TarGer2 :: #ger{}, 
		   AttackAction :: #p_action{},
		   AbsortHp :: ?int32,
		   DamageBackHp :: ?int32
		  }.
attack(DataSkill, SrcGer, TarGer, RandomSeed) ->
	?ERR("---------------------=0~w",[]),
	Doom = ?a(TarGer,gerMiss)-?a(SrcGer,gerDoom)-DataSkill#data_skill.gerDoom,
		?ERR("---------------------=1~w",[]),
	if Doom =< 0 ->
		   	?ERR("---------------------=2~w",[]),
		   attack2(DataSkill, SrcGer, TarGer, RandomSeed);
	   true ->
		   	?ERR("---------------------=3~w",[]),
		   {RandomValue, RandomSeed2} = random:uniform_s(RandomSeed),
				?ERR("---------------------=4~w",[]),
		   case RandomValue =< 1/(1+Doom/100) of
			   true ->
				   	?ERR("---------------------=5~w",[]),
				   attack2(DataSkill, SrcGer, TarGer, RandomSeed2);
			   false ->
				   	?ERR("---------------------=6~w",[]),
                   TarAddSp = real_add_sp(TarGer#ger.gerSp, ?HURT_ADD_SP, (TarGer#ger.gerAttr)#gerAttr.gerSpMax),
				   TarSp2 = TarGer#ger.gerSp + TarAddSp,
				   TarGer2 = TarGer#ger{gerSp=TarSp2},
				   {miss, ?STATE_DEFAULT, TarGer2, ?miss_action(TarGer2, TarAddSp), 0, 0, RandomSeed2}
		   end
	end.
		
-define(physic_defence, (SrcAttr#gerAttr.gerPhyDefBite + DataSkill#data_skill.gerPhyDefBite - TarAttr#gerAttr.gerPhyDef)).
-define(magic_defence,  (SrcAttr#gerAttr.gerMagDefBite + DataSkill#data_skill.gerMagDefBite - TarAttr#gerAttr.gerMagDef)).

%% 攻击伤害的具体运算
attack2(DataSkill, SrcGer, TarGer, RandomSeed) ->
		?ERR("---------------------=7~w",[]),
	#data_skill{defendActionID=DefendActionID,damageRatio=DamageRatio,gerReel=AddReel,
				damageType=DamageType, gerCritic=AddCritic,gerAbsorb=AddAbsorb}=DataSkill,
	#ger{gerBase=_SrcBase,gerAttr=SrcAttr}=SrcGer,
	#ger{gerBase=#gerBase{gerTypeID=TarGerTypeID}=TarBase,gerAttr=TarAttr,gerHp=TarHp,gerSp=TarSp}=TarGer,
				?ERR("---------------------=8~w",[]),
	%% 计算击晕
	Reel = SrcAttr#gerAttr.gerReel + AddReel - TarAttr#gerAttr.gerReelReduce,
				?ERR("---------------------=9~w",[]),
	{ReelState, RandomSeed2} = 
	if Reel =< 0 ->
		   		?ERR("---------------------=10~w",[]),
		   {?STATE_DEFAULT, RandomSeed};
	   true ->
		   		?ERR("---------------------=11~w",[]),
		   {RandomValue, RandomSeedT1} =  random:uniform_s(RandomSeed),
		   case RandomValue =< 1-1/(1+Reel*0.01) of
			   true ->
				   {?STATE_REEL, RandomSeedT1};
			   false ->
				   {?STATE_DEFAULT, RandomSeedT1}
		   end
	end,
			   		?ERR("---------------------=11~w",[]),
	%% 计算暴击
	CritProb = SrcAttr#gerAttr.gerCritic+ AddCritic - TarAttr#gerAttr.gerCriticReduce,
	{CritValue, CritState, RandomSeed3} = 
	if CritProb =< 0 ->
		   {1,?STATE_DEFAULT, RandomSeed2};
	   CritProb > 100 ->
		   {2,?STATE_CRIT, RandomSeed2};
	   true ->		   
		   {RandomValue2, RandomSeedT2} = random:uniform_s(RandomSeed2),
		   case RandomValue2 =< CritProb * 0.01 of
			   true ->
				   {2,?STATE_CRIT, RandomSeedT2};
			   false ->
				   {1,?STATE_DEFAULT, RandomSeedT2}
		   end
	end,
		?ERR("---------------------=12~w",[]),
	%% 伤害数值随机区间
	{A,B} = data_common:get(DamageType),
	?DEBUG("技能伤害类型：~w",[{DamageType,A,B}]),
	{RandomValue3, RandomSeed4} = random_float(A,B, RandomSeed3),
		?ERR("---------------------=13~w",[]),
	%% 抗性系数
	case DamageType of
		%% 刀
		physic_sword ->
			Defence = ?physic_defence;
		%% 斧
		physic_axe ->
			Defence = ?physic_defence;
		%% 锤
		physic_hammer ->
			Defence = ?physic_defence;
		%% 弓
		physic_bow ->
			Defence = ?physic_defence;
		%% 毒
		magic_poison ->
			Defence = ?magic_defence;
		%% 雷
		magic_thunder ->
			Defence = ?magic_defence;
		%% 火
		magic_fire ->
			Defence = ?magic_defence;
		%% 冰
		magic_ice ->
			Defence = ?magic_defence;
		physic_stone ->
			Defence = ?physic_defence;
		physic_wind ->
			Defence = ?physic_defence;
		physic_wave ->
			Defence = ?physic_defence;
		physic_claw ->
			Defence = ?physic_defence
	end,
	?ERR("---------------------=14~w",[]),
	Defence2 =
		if Defence >= 0 ->
			   1 + Defence * 0.01;
		   true ->
			   1 / (1 - Defence * 0.01)
		end,
	%% 克性系数
    ?ERR("---------------------=15~w",[]),
	Anti = get_anti((data_ger:get(TarGerTypeID))#data_ger.gerKingdom,SrcAttr),
	%?DEBUG("CritValue=~w\nAttack=~w\nAnti=~w\nDefence2=~w\nDamageRatio=~w\n",[CritValue, SrcAttr#gerAttr.gerAttack,Anti,Defence2,DamageRatio]),
?ERR("---------------------=16~w",[]),
	%% 伤害计算公式
	HarmValue = erlang:max(1,trunc(RandomValue3 * CritValue * SrcAttr#gerAttr.gerAttack * (1 + 0.0001 * Anti) * Defence2 * DamageRatio * 0.0001)),
		?ERR("---------------------=17~w",[]),
	%% 加怒气
    TarAddSp = real_add_sp(TarSp, ?HURT_ADD_SP, (TarGer#ger.gerAttr)#gerAttr.gerSpMax),
		?ERR("---------------------=18~w",[]),
    TarSp2 = TarSp + TarAddSp,
	%% 扣血量(重算血量。。。设置血量不超过最大血量值)
	TarHp2 = add_hp(TarHp, TarAttr#gerAttr.gerHpMax, -HarmValue),
	%% 实际伤害数值
	Value = TarHp-TarHp2,
	%?ERR("TarHp=~w",[TarHp2]),
?ERR("---------------------=19~w",[]),
	if TarHp2 =< 0 ->
		   %?ERR("if clause=~w",[1]),
		   DeadState = ?STATE_DEAD;
	   true ->
		   %?ERR("if clause=~w",[true]),
		   DeadState = ?STATE_DEFAULT
	end,
		?ERR("---------------------=20~w",[]),
	%% 吸血
	Absorb = SrcAttr#gerAttr.gerAbsorb+AddAbsorb,
	if Absorb =< 0 ->
		   AbsortState = ?STATE_DEFAULT,
		   AbsortHp = 0;
	   true ->
		   AbsortState = ?STATE_ABSORB,
	AbsortHp = erlang:max(1,trunc(Value * Absorb /10000))
	end,
			?ERR("---------------------=21~w",[]),
	%% 反弹
	if TarHp =< 0 ->
		   %% 如果死亡，则不反弹
		   DamageBackState = ?STATE_DEFAULT,
		   DamageBackHp = 0;
	   true ->		   
		   DamageBack = TarAttr#gerAttr.gerDamageBack,
		   if DamageBack =< 0 ->
				  DamageBackState = ?STATE_DEFAULT,
				  DamageBackHp = 0;
			  true ->
				  DamageBackState = ?STATE_DAMAGE_BACK,
				  DamageBackHp = -erlang:max(1,trunc(Value * DamageBack /10000))
		   end
	end,
			?ERR("---------------------=22~w",[]),
	%% 攻击附带的状态（暴击、击晕）
	State = ReelState bor AbsortState bor DamageBackState bor DeadState bor CritState,
		  		?ERR("---------------------=23~w",[]),
	%% 受击action
	Action = ?action(DefendActionID,TarBase#gerBase.gerPos,[],-HarmValue,TarAddSp,State),
			?ERR("---------------------=24~w",[]),
	%% 更新血量与怒气
	TarGer2 = TarGer#ger{gerSp=TarSp2,gerHp=TarHp2},
			?ERR("---------------------=25~w",[]),
	{Value, ReelState, TarGer2, Action, AbsortHp, DamageBackHp, RandomSeed4}.
	

%% %% 吸血
%% absorb(SrcGer, AttackResultList) ->
%% 	if Absorb =< 0 ->
%% 		   {SrcGer, []};
%% 	   true ->
%% 		   GerPos = ?b(SrcGer, gerPos),
%% 		   {AddHpTotal, AbsorbActionList} = 
%% 			   lists:foldl(fun({miss, _,_,_,_}, Acc) ->
%% 								   Acc;
%% 							  ({Value,_,_,TarGer,_}, {HpAcc, ActionListAcc}) ->
%% 								   Action = ?action(?ACTION_ABSORB, GerPos, [?b(TarGer, gerPos)], AddHp, 0, ?STATE_DEFAULT),  
%% 								   {HpAcc+AddHp, [Action|ActionListAcc]}
%% 						   end, {0, []}, AttackResultList),
%% 		   {AddHpTotal, AbsorbActionList}
%% 	end.
%% 
%% %% 反弹
%% damage_back(SrcGer, AttackResultList) ->
%% 	GerPos = ?b(SrcGer, gerPos),
%% 	{DeductHpTotal, DamageBackActionList} = 
%% 		lists:foldl(fun({Value,alive,_,TarGer,_}, {HpAcc, ActionListAcc}=Acc) when is_integer(Value) ->
%% 							DamageBack = ?a(TarGer, gerDamageBack),
%% 							if DamageBack > 0 ->
%% 								   DeductHp= trunc(Value * DamageBack /100),
%% 								   Action = ?action(?ACTION_DAMAGE_BACK, GerPos, [?b(TarGer, gerPos)], -DeductHp, 0, ?STATE_DEFAULT),  
%% 								   {HpAcc+DeductHp, [Action|ActionListAcc]};
%% 							   true ->
%% 								   Acc
%% 							end;
%% 					   (_,Acc) ->
%% 							Acc			
%% 				end, {0, []}, AttackResultList),
%% 	{-DeductHpTotal, DamageBackActionList}.
%% ====================================================================
%% Internal functions
%% ====================================================================

%% 增加怒气
add_sp(TarSp, AddSp, MAXSP) ->
	erlang:max(0, erlang:min(MAXSP, TarSp+AddSp)).

real_add_sp(TarSp, AddSp, MAXSP) ->
    case AddSp >= 0 of
        true ->
            case TarSp + AddSp > MAXSP of
                true ->
                    MAXSP - TarSp;
                false ->
                    AddSp
            end;
        false ->
            case TarSp + AddSp < 0 of
                true ->
                    -TarSp;
                false ->
                    AddSp
            end
    end.

%% 加血量
add_hp(Hp, HpMax, AddHp) ->
	erlang:max(0, erlang:min(HpMax, Hp+AddHp)).

%% 获取相克系数
get_anti(?KINGDOM_SHU, GerAttr) ->
	GerAttr#gerAttr.gerAntiShu;
get_anti(?KINGDOM_WEI, GerAttr) ->
	GerAttr#gerAttr.gerAntiWei;
get_anti(?KINGDOM_WU, GerAttr) ->
	GerAttr#gerAttr.gerAntiWu;
get_anti(?KINGDOM_QUN, GerAttr) ->
	GerAttr#gerAttr.gerAntiQun.

%% 获取阵营
camp(Ger) ->
	?sign(?b(Ger,gerPos)).

%% 检查是否有活着的
select_alive(PosList, GerDict) ->
	lists:foldl(fun(Pos, Acc) -> 
					  Ger = get_ger(Pos, GerDict),
					  if is_record(Ger, ger) andalso Ger#ger.gerHp>0 ->
							 [Ger|Acc];
						 true ->
							 Acc
					  end
			  end, [], PosList).

%% @doc 获取敌方活着的玩家
enemy_alive(SrcGer,?dict) ->
	SrcCamp= camp(SrcGer),
	[Ger || {_,Ger} <-erlang:get(), Ger#ger.gerHp>0, camp(Ger) =/= SrcCamp].

%% @doc 获取己方或者的玩家
self_alive(SrcGer, ?dict) ->
	SrcCamp= camp(SrcGer),
	[Ger || {_,Ger} <-erlang:get(), Ger#ger.gerHp>0, camp(Ger) =:= SrcCamp].

%% @doc 技能目标选择
%% 单一敌人
select_target(random_two, SrcGer, GerDict, RS) ->
	random_select_list(2, enemy_alive(SrcGer, GerDict), RS);
select_target(random_three, SrcGer, GerDict, RS) ->
	random_select_list(3, enemy_alive(SrcGer, GerDict), RS);
select_target(own_random_two, SrcGer, GerDict, RS) ->
	random_select_list(2, self_alive(SrcGer, GerDict), RS);
select_target(own_random_three, SrcGer, GerDict, RS) ->
	random_select_list(3, self_alive(SrcGer, GerDict), RS);
select_target(TarSelect, SrcGer, GerDict, RS) ->
	FightOrder= get_fight_order(TarSelect, ?b(SrcGer,gerPos)),
	List = select_target2(FightOrder, GerDict),
	{List, RS}.

select_target2([], _GerDict) ->
	[];
select_target2([TarList|FightOrder], GerDict) ->
	case select_alive(TarList, GerDict) of
		[] ->
			select_target2(FightOrder, GerDict);
		TarAlive->
			TarAlive
	end.

%% @doc 选择技能
select_skill(Ger,GerSkills) ->
	#ger{gerSp=GerSp} = Ger,
	if GerSp >= ?MAXSP ->
		   #gerSkill{unique=UniqueSkills} = GerSkills,
		   data_skill:get(random_one_skill(UniqueSkills));
	   true ->
		   #gerSkill{normal=NormalSkills} = GerSkills,
		   data_skill:get(random_one_skill(NormalSkills))
	end.
random_one_skill([])->
	0;
random_one_skill(T)->
	erlang:hd(T).

%% 预留Dict接口，并传递，用来在以后测试切换成lists模块、dict模块、orddict模块时的效率
%% @doc 初始化ger列表
init_ger_dict(GerList) ->
	lists:foreach(fun(Ger) ->erlang:put(?b(Ger,gerPos), Ger) end, GerList),
	?dict.

%% @doc 获取对应pos的Ger
get_ger(Pos, ?dict) ->
	erlang:get(Pos).

%% @doc 设置对应pos的Ger
set_ger(Ger, ?dict) ->
	erlang:put(?b(Ger, gerPos), Ger),
	?dict.

%% @doc 遍历gerDict
fold_ger_dict(Fun, Init, ?dict) ->
	Fun2 = fun({Key, Value}, Acc) when is_integer(Key)->
				   Fun(Value, Acc);
			  (_,Acc) ->
				   Acc
		   end,
	util:foldl(Fun2, Init, erlang:get()).

%% @doc map遍历gerDict
map_ger_dict(Fun, ?dict, Queue) ->
	lists:foreach(fun({Key, Ger}) ->
						  case lists:keyfind(Key, 1, Queue) of
							  {_,#gerSkill{god=GodSkill}} ->
								  if GodSkill =/= [] ->
										 record_ger_god_skill(Key,Ger);
									 true ->
										 ignore
								  end;
							  _ ->
								  ignore
						  end,
						  Ger3 = Fun(Ger), 
						  put(Key, Ger3)
				  end, erlang:get()),
	?dict.

%% @doc mapfold
mapfold_ger_dict(Fun, Init, ?dict) ->
	Fun2 = fun({Key, Value}, Acc) when is_integer(Key)->
				  {Value2, Acc2} = Fun(Value, Acc),
				  if Value2 =:= Value ->
						 Acc2;
					 true ->
						 put(Key, Value2),
						 Acc2
				  end;
			(_, Acc) ->
				Acc
		   end,
	foldl_dict(Fun2, Init, erlang:get()).

foldl_dict(F, Accu, [Hd|Tail]) ->
    foldl_dict(F, F(Hd, Accu), Tail);
foldl_dict(F, Accu, []) when is_function(F, 2) -> {?dict, Accu}.

%% @doc 增加位置符号，在上面的单位位置符号为-1
add_pos_sign(DefenderList) ->
	[add_pos_sign2(Defender)||Defender<-DefenderList].
add_pos_sign2(#ger{gerBase=#gerBase{gerPos=GerPos}=GerBase}=Defender) ->
	Defender#ger{gerBase=GerBase#gerBase{gerPos=-GerPos}}.

%% @doc 获取出手顺序列表,如果是打招财boss，从出手列表中删除boss
get_queue(GerList, IsGetCoin) ->
	PosList = [?b(Ger, gerPos)||Ger<-GerList],
	F = fun(A,B) ->
				A1 = erlang:abs(A),
				B1 = erlang:abs(B),
				if A1 < B1 ->
					   true;
				   A1 =:= B1 ->
					   if A > 0 ->
							  true ;
						  true ->
							  false
					   end;
				   true ->
					   false
				end
		end,
	PosList2 = lists:sort(F, PosList),
    case IsGetCoin of
        false ->
            PosList2;
        true ->
            [Pos||Pos<-PosList2,Pos>0]
    end.

get_queue2(GerList, IsGetCoin) ->
    PosList = [?b(Ger, gerPos)||Ger<-GerList],
    F = fun(A,B) ->
                A1 = erlang:abs(A),
                B1 = erlang:abs(B),
                if A1 < B1 ->
                       true;
                   A1 =:= B1 ->
                       if A < 0 ->
                              true ;
                          true ->
                              false
                       end;
                   true ->
                       false
                end
        end,
    PosList2 = lists:sort(F, PosList),
    case IsGetCoin of
        false ->
            PosList2;
        true ->
            [Pos||Pos<-PosList2,Pos<0]
    end.

%% 技能的目标个数
skill_target_num(single) ->	1;
skill_target_num(single_back) ->1;
skill_target_num(front_row) ->	3;
skill_target_num(back_row) ->	3;
skill_target_num(array) ->	2;
skill_target_num(random_two) ->	2;
skill_target_num(random_three) ->	3;
skill_target_num(all) ->6;
skill_target_num(own_self) ->1;
skill_target_num(own_single       ) ->1;
skill_target_num(own_front_row    ) ->3;
skill_target_num(own_back_row     ) ->3;
skill_target_num(own_all          ) ->6;
skill_target_num(own_array        ) ->2;
skill_target_num(own_single_back  ) ->1;
skill_target_num(own_random_two   ) ->2;
skill_target_num(own_random_three ) ->3.


%% @计算战斗力使用
select_posList(TarSelect, Pos, PosList) ->
	SelectList = get_fight_order(TarSelect, Pos),
	util:fun_find(fun(Select) ->
						  lists:any(fun(D) -> lists:member(D, PosList) end, Select)
				  end, SelectList).

%% 静态化目标选择逻辑
%% 找最近的一个
get_fight_order(single, 1) ->
	[[-1],[-2],[-3],[-4],[-5],[-6]];
get_fight_order(single, -1) ->
	[[1],[2],[3],[4],[5],[6]];

get_fight_order(single, 2) ->
	[[-2],[-1],[-3],[-5],[-4],[-6]];
get_fight_order(single, -2) ->
	[[2],[1],[3],[5],[4],[6]];

get_fight_order(single,3) ->
	[[-3],[-2],[-1],[-6],[-5],[-4]];
get_fight_order(single,-3) ->
	[[3],[2],[1],[6],[5],[4]];

get_fight_order(single, 4) ->
	[[-1],[-2],[-3],[-4],[-5],[-6]];
get_fight_order(single, -4) ->
	[[1],[2],[3],[4],[5],[6]];

get_fight_order(single, 5) ->
	[[-2],[-1],[-3],[-5],[-4],[-6]];
get_fight_order(single, -5) ->
	[[2],[1],[3],[5],[4],[6]];

get_fight_order(single,6) ->
	[[-3],[-2],[-1],[-6],[-5],[-4]];
get_fight_order(single,-6) ->
	[[3],[2],[1],[6],[5],[4]];

%% 后排的最近一个
get_fight_order(single_back, 1) ->
	[[-4],[-5],[-6],[-1],[-2],[-3]];
get_fight_order(single_back, -1) ->
	[[4],[5],[6],[1],[2],[3]];

get_fight_order(single_back, 2) ->
	[[-5],[-4],[-6],[-2],[-1],[-3]];
get_fight_order(single_back, -2) ->
	[[5],[4],[6],[2],[1],[3]];

get_fight_order(single_back,3) ->
	[[-6],[-5],[-4],[-3],[-2],[-1]];
get_fight_order(single_back,-3) ->
	[[6],[5],[4],[3],[2],[1]];

get_fight_order(single_back, 4) ->
	[[-4],[-5],[-6],[-1],[-2],[-3]];
get_fight_order(single_back, -4) ->
	[[4],[5],[6],[1],[2],[3]];

get_fight_order(single_back, 5) ->
	[[-5],[-4],[-6],[-2],[-1],[-3]];
get_fight_order(single_back, -5) ->
	[[5],[4],[6],[2],[1],[3]];

get_fight_order(single_back,6) ->
	[[-6],[-5],[-4],[-3],[-2],[-1]];
get_fight_order(single_back,-6) ->
	[[6],[5],[4],[3],[2],[1]];

%% 前排三个
get_fight_order(front_row,1) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(front_row,-1) ->
	[[1,2,3],[4,5,6]];

get_fight_order(front_row,2) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(front_row,-2) ->
	[[1,2,3],[4,5,6]];

get_fight_order(front_row,3) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(front_row,-3) ->
	[[1,2,3],[4,5,6]];

get_fight_order(front_row,4) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(front_row,-4) ->
	[[1,2,3],[4,5,6]];

get_fight_order(front_row,5) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(front_row,-5) ->
	[[1,2,3],[4,5,6]];

get_fight_order(front_row,6) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(front_row,-6) ->
	[[1,2,3],[4,5,6]];

%% 后排三个
get_fight_order(back_row,1) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(back_row,-1) ->
	[[4,5,6],[1,2,3]];

get_fight_order(back_row,2) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(back_row,-2) ->
	[[4,5,6],[1,2,3]];

get_fight_order(back_row,3) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(back_row,-3) ->
	[[4,5,6],[1,2,3]];

get_fight_order(back_row,4) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(back_row,-4) ->
	[[4,5,6],[1,2,3]];

get_fight_order(back_row,5) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(back_row,-5) ->
	[[4,5,6],[1,2,3]];

get_fight_order(back_row,6) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(back_row,-6) ->
	[[4,5,6],[1,2,3]];

%% 一列
get_fight_order(array, 1) ->
	[[-1,-4],[-2,-5],[-3,-6]];
get_fight_order(array, -1) ->
	[[1,4],[2,5],[3,6]];

get_fight_order(array, 2) ->
	[[-2,-5],[-1,-4],[-3,-6]];
get_fight_order(array, -2) ->
	[[2,5],[1,4],[3,6]];

get_fight_order(array, 3) ->
	[[-3,-6],[-2,-5],[-1,-4]];
get_fight_order(array, -3) ->
	[[3,6],[2,5],[1,4]];

get_fight_order(array, 4) ->
	[[-1,-4],[-2,-5],[-3,-6]];
get_fight_order(array, -4) ->
	[[1,4],[2,5],[3,6]];

get_fight_order(array, 5) ->
	[[-2,-5],[-1,-4],[-3,-6]];
get_fight_order(array, -5) ->
	[[2,5],[1,4],[3,6]];

get_fight_order(array, 6) ->
	[[-3,-6],[-2,-5],[-1,-4]];
get_fight_order(array, -6) ->
	[[3,6],[2,5],[1,4]];

%% 全体
get_fight_order(all, -1) ->
	[[1,2,3,4,5,6]];
get_fight_order(all, -2) ->
	[[1,2,3,4,5,6]];
get_fight_order(all, -3) ->
	[[1,2,3,4,5,6]];
get_fight_order(all, -4) ->
	[[1,2,3,4,5,6]];
get_fight_order(all, -5) ->
	[[1,2,3,4,5,6]];
get_fight_order(all, -6) ->
	[[1,2,3,4,5,6]];


get_fight_order(all, 1) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(all, 2) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(all, 3) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(all, 4) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(all, 5) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(all, 6) ->
	[[-1,-2,-3,-4,-5,-6]];

%% 自己
get_fight_order(own_self, 1) ->
	[[1]];
get_fight_order(own_self, -1) ->
	[[-1]];

get_fight_order(own_self, 2) ->
	[[2]];
get_fight_order(own_self, -2) ->
	[[-2]];

get_fight_order(own_self,-3) ->
	[[-3]];
get_fight_order(own_self,3) ->
	[[3]];

get_fight_order(own_self, -4) ->
	[[-4]];
get_fight_order(own_self, 4) ->
	[[4]];

get_fight_order(own_self, -5) ->
	[[-5]];
get_fight_order(own_self, 5) ->
	[[5]];

get_fight_order(own_self,-6) ->
	[[-6]];
get_fight_order(own_self,6) ->
	[[6]];


%% 己方一个
get_fight_order(own_single, 1) ->
	[[1],[2],[3],[4],[5],[6]];
get_fight_order(own_single, -1) ->
	[[-1],[-2],[-3],[-4],[-5],[-6]];

get_fight_order(own_single, 2) ->
	[[2],[1],[3],[5],[4],[6]];
get_fight_order(own_single, -2) ->
	[[-2],[-1],[-3],[-5],[-4],[-6]];

get_fight_order(own_single,-3) ->
	[[-3],[-2],[-1],[-6],[-5],[-4]];
get_fight_order(own_single,3) ->
	[[3],[2],[1],[6],[5],[4]];

get_fight_order(own_single, -4) ->
	[[-1],[-2],[-3],[-4],[-5],[-6]];
get_fight_order(own_single, 4) ->
	[[1],[2],[3],[4],[5],[6]];

get_fight_order(own_single, -5) ->
	[[-2],[-1],[-3],[-5],[-4],[-6]];
get_fight_order(own_single, 5) ->
	[[2],[1],[3],[5],[4],[6]];

get_fight_order(own_single,-6) ->
	[[-3],[-2],[-1],[-6],[-5],[-4]];
get_fight_order(own_single,6) ->
	[[3],[2],[1],[6],[5],[4]];

%% 后排的己方一个

get_fight_order(own_single_back, -1) ->
	[[-4],[-5],[-6],[-1],[-2],[-3]];
get_fight_order(own_single_back, 1) ->
	[[4],[5],[6],[1],[2],[3]];

get_fight_order(own_single_back, -2) ->
	[[-5],[-4],[-6],[-2],[-1],[-3]];
get_fight_order(own_single_back, 2) ->
	[[5],[4],[6],[2],[1],[3]];

get_fight_order(own_single_back,-3) ->
	[[-6],[-5],[-4],[-3],[-2],[-1]];
get_fight_order(own_single_back,3) ->
	[[6],[5],[4],[3],[2],[1]];

get_fight_order(own_single_back, -4) ->
	[[-4],[-5],[-6],[-1],[-2],[-3]];
get_fight_order(own_single_back, 4) ->
	[[4],[5],[6],[1],[2],[3]];

get_fight_order(own_single_back, -5) ->
	[[-5],[-4],[-6],[-2],[-1],[-3]];
get_fight_order(own_single_back, 5) ->
	[[5],[4],[6],[2],[1],[3]];

get_fight_order(own_single_back,-6) ->
	[[-6],[-5],[-4],[-3],[-2],[-1]];
get_fight_order(own_single_back,6) ->
	[[6],[5],[4],[3],[2],[1]];


%% 己方前排三个

get_fight_order(own_front_row,-1) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(own_front_row,1) ->
	[[1,2,3],[4,5,6]];

get_fight_order(own_front_row,-2) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(own_front_row,2) ->
	[[1,2,3],[4,5,6]];

get_fight_order(own_front_row,-3) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(own_front_row,3) ->
	[[1,2,3],[4,5,6]];

get_fight_order(own_front_row,-4) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(own_front_row,4) ->
	[[1,2,3],[4,5,6]];

get_fight_order(own_front_row,-5) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(own_front_row,5) ->
	[[1,2,3],[4,5,6]];

get_fight_order(own_front_row,-6) ->
	[[-1,-2,-3],[-4,-5,-6]];
get_fight_order(own_front_row,6) ->
	[[1,2,3],[4,5,6]];

%% 己方后排三个

get_fight_order(own_back_row,-1) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(own_back_row,1) ->
	[[4,5,6],[1,2,3]];

get_fight_order(own_back_row,-2) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(own_back_row,2) ->
	[[4,5,6],[1,2,3]];

get_fight_order(own_back_row,-3) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(own_back_row,3) ->
	[[4,5,6],[1,2,3]];

get_fight_order(own_back_row,-4) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(own_back_row,4) ->
	[[4,5,6],[1,2,3]];

get_fight_order(own_back_row,-5) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(own_back_row,5) ->
	[[4,5,6],[1,2,3]];

get_fight_order(own_back_row,-6) ->
	[[-4,-5,-6],[-1,-2,-3]];
get_fight_order(own_back_row,6) ->
	[[4,5,6],[1,2,3]];


%% 己方一列
get_fight_order(own_array, -1) ->
	[[-1,-4],[-2,-5],[-3,-6]];
get_fight_order(own_array, 1) ->
	[[1,4],[2,5],[3,6]];

get_fight_order(own_array, -2) ->
	[[-2,-5],[-1,-4],[-3,-6]];
get_fight_order(own_array, 2) ->
	[[2,5],[1,4],[3,6]];

get_fight_order(own_array, -3) ->
	[[-3,-6],[-2,-5],[-1,-4]];
get_fight_order(own_array, 3) ->
	[[3,6],[2,5],[1,4]];

get_fight_order(own_array, -4) ->
	[[-1,-4],[-2,-5],[-3,-6]];
get_fight_order(own_array, 4) ->
	[[1,4],[2,5],[3,6]];

get_fight_order(own_array, -5) ->
	[[-2,-5],[-1,-4],[-3,-6]];
get_fight_order(own_array, 5) ->
	[[2,5],[1,4],[3,6]];

get_fight_order(own_array, -6) ->
	[[-3,-6],[-2,-5],[-1,-4]];
get_fight_order(own_array, 6) ->
	[[3,6],[2,5],[1,4]];

%% 己方全体
get_fight_order(own_all, 1) ->
	[[1,2,3,4,5,6]];
get_fight_order(own_all, 2) ->
	[[1,2,3,4,5,6]];
get_fight_order(own_all, 3) ->
	[[1,2,3,4,5,6]];
get_fight_order(own_all, 4) ->
	[[1,2,3,4,5,6]];
get_fight_order(own_all, 5) ->
	[[1,2,3,4,5,6]];
get_fight_order(own_all, 6) ->
	[[1,2,3,4,5,6]];


get_fight_order(own_all, -1) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(own_all, -2) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(own_all, -3) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(own_all, -4) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(own_all, -5) ->
	[[-1,-2,-3,-4,-5,-6]];
get_fight_order(own_all, -6) ->
	[[-1,-2,-3,-4,-5,-6]].


%% @doc 从一个列表中随机抽取N个，顺序随机 ，N可以超过界限
random_select_list(N, List, RS) ->
	random_list2(List, N, length(List),[], RS).

random_list2(_List, 0, _Length, Result, RS) ->
	{Result, RS};
random_list2(List, N, Length, Result, RS) ->
	if Length =:= 1 ->
		   Select = hd(List),
		   Rest = [],
		   random_list2(Rest, N-1, Length-1, [Select|Result], RS);
	   Length =:= 0 ->
		   {Result, RS};
	   true ->
		   {Rand,RS2} = random:uniform_s(Length, RS),
		   {value, Select, Rest} = util:nth_take(Rand, List),		   
		   random_list2(Rest, N-1, Length-1, [Select|Result], RS2)
	end.



%% @doc 从(lower...Higher)中随机出一个浮点数出来
random_float(Lower, Lower, RS) ->
	{Lower, RS};
random_float(Lower, Higher, RS) ->
	{V, RS2} = random:uniform_s(RS),
	{V * (Higher -Lower) + Lower, RS2}.

%% 己方随机两个
-define(TEST_NUM, 10000).

test() ->
	AttackerList = ?test_attacker_list,
	DefenderList = ?test_defender_list,
	{_,_Record,_} = Result = role_fight:new(AttackerList, DefenderList,{0,0},{0,0}),
	Result.


	

