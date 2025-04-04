%% @author admin
%% @doc 武将API
%% Created 2013-3-6


-module(ger_lib).

-include("def_role.hrl").
-compile(export_all).
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

is_ger_material_match(Ger, GerTypeID) ->
	#gerSimple{gerLevel=GerLevel,gerQuality=GerRank,gerTypeID=T} = Ger,
	T == GerTypeID andalso GerLevel < 10 andalso GerRank < 1.

check_ger_material(GerBag, GerTypeID, Num) ->
	check_ger_material(GerBag, GerTypeID, Num, [],[]).


check_ger_material(GerBag, _GerTypeID, 0, Result,DelAcc)->
	{true, GerBag++Result, DelAcc};
check_ger_material([], _GerTypeID, _Num, _Result,_DelAcc)->
	false;
check_ger_material([Ger|GerBag], GerTypeID, Num, Result, DelAcc) ->
	case is_ger_material_match(Ger, GerTypeID) of
		true ->
			check_ger_material(GerBag, GerTypeID, Num-1, Result, [Ger|DelAcc]);
		false ->
			check_ger_material(GerBag, GerTypeID, Num, [Ger|Result], DelAcc)
	end.
			
	
	
	
gerList2p_ger_pos(PosList) ->
	[#p_ger_pos{gerID=GerID,gerPos=GerPos}||#ger{gerID=GerID,gerBase=#gerBase{gerPos=GerPos}} <- PosList].

ger2gerSimple(Ger) ->
	#ger{gerBase=GerBase}=Ger,
	#gerSimple{
			   gerQuality=GerBase#gerBase.gerQuality,
			   gerTypeID=GerBase#gerBase.gerTypeID,
			   gerLevel=GerBase#gerBase.gerLevel,
			   gerID=Ger#ger.gerID,
			   gerExp=GerBase#gerBase.gerExp}.


%% @doc 武将加经验
-spec add_exp_and_notify(GerID :: ?int64, 
						 AddExp :: ?int32, 
						 RoleLevel :: ?int16,
		   GerList ::[#ger{}]) ->
		  {IsLevelUpgraded::boolean(), 
		   NewGer:: #ger{}, 
		   RealAddExp :: ?int32}.
add_exp_and_notify(Ger, AddExp, RoleLevel, GerList) ->
	#ger{gerBase=GerBase,gerID=GerID}= Ger ,
	#gerBase{gerExp=GerExp,gerLevel=GerLevel} = GerBase,
	case add_exp(GerLevel, GerExp, AddExp, RoleLevel) of
		{level_up, Level2, Exp2} ->
			#ger{gerBase=GerBase,gerAttr=_GerAttr} = Ger,
			Ger2  = Ger#ger{gerBase=GerBase#gerBase{gerLevel=Level2,gerExp=Exp2}},
			Ger3 = ger_attr:recacl(Ger2, GerList),
			notify_update(Ger3),
			role_homestead:hook_ger_levelup(GerID, Level2),
			{true, Ger3, Exp2-GerBase#gerBase.gerExp};
		{level_not_up, _Level2, Exp2} ->
			#ger{gerBase=GerBase} = Ger,
			Ger2  = Ger#ger{gerBase=GerBase#gerBase{gerExp=Exp2}},
			?sendself(#sc_ger_update_exp{gerExp=Exp2,gerID=GerID}),
			{false, Ger2, Exp2-GerBase#gerBase.gerExp};
		{level_max,_Level2,_Exp2} ->
			{false, Ger, 0}
	end.

add_exp_and_notify2(GerSimple, AddExp, RoleLevel) ->
	#gerSimple{gerID=GerID,gerExp=GerExp,gerLevel=GerLevel} = GerSimple,
	case add_exp(GerLevel, GerExp, AddExp, RoleLevel) of
		{level_up, Level2, Exp2} ->
			GerSimple2  = GerSimple#gerSimple{gerLevel=Level2,gerExp=Exp2},
			notify_update(GerSimple2),
			role_homestead:hook_ger_levelup(GerID, Level2),
			GerSimple2;
		{level_not_up, _Level2, Exp2} ->
			GerSimple2  = GerSimple#gerSimple{gerExp=Exp2},
			?sendself(#sc_ger_update_exp{gerExp=Exp2,gerID=GerID}),
			GerSimple2;
		{level_max,_Level2,_Exp2} ->
			GerSimple
	end.
	
	
	
notify_update(#ger{}=Ger) ->
	#ger{gerBase=GerBase,gerAttr=GerAttr}=Ger,
	Notify = #sc_ger_update{
							gerID=Ger#ger.gerID,
							gerQuality=GerBase#gerBase.gerQuality,
							gerLevel=GerBase#gerBase.gerLevel,
							gerAttack=GerAttr#gerAttr.gerAttack,
							gerHpMax=GerAttr#gerAttr.gerHpMax,
							gerFightPower=GerAttr#gerAttr.gerFightPower,
							gerExp=GerBase#gerBase.gerExp
						   },
	?sendself(Notify);
notify_update(Ger) ->
	Notify = #sc_ger_update{
							gerID=Ger#gerSimple.gerID,
		   gerQuality	=Ger#gerSimple.gerQuality,	
		   gerLevel		=Ger#gerSimple.gerLevel,	
		   gerAttack	=0,
		   gerHpMax		=0,
		   gerFightPower=0,
		   gerExp 		=Ger#gerSimple.gerExp
						   },
	?sendself(Notify).

%% @doc #ger{}结构转化为#p_ger{}
ger2p_ger(#ger{}=Ger) ->
	#ger{gerAttr=GerAttr,gerBase=GerBase,gerID=GerID} = Ger,
	#p_ger{gerID		=GerID,
		   gerTypeID	=GerBase#gerBase.gerTypeID,
		   gerQuality	=GerBase#gerBase.gerQuality,	
		   gerLevel		=GerBase#gerBase.gerLevel,	
		   gerAttack	=GerAttr#gerAttr.gerAttack,	
		   gerHpMax		=GerAttr#gerAttr.gerHpMax,
		   gerFightPower=GerAttr#gerAttr.gerFightPower,
		   gerExp 		=GerBase#gerBase.gerExp};
ger2p_ger(#gerSimple{}=Ger) ->
	#p_ger{gerID		=Ger#gerSimple.gerID,
		   gerTypeID	=Ger#gerSimple.gerTypeID,
		   gerQuality	=Ger#gerSimple.gerQuality,	
		   gerLevel		=Ger#gerSimple.gerLevel,	
		   gerAttack	=0,
		   gerHpMax		=0,
		   gerFightPower=0,
		   gerExp 		=Ger#gerSimple.gerExp}.

%% @doc 初始化data_mon配置
data_mon_list2ger_list(DataMonList) ->
	[data_mon2ger(DataMon) || DataMon <- DataMonList].
data_mon2ger(DataMon) when is_record(DataMon, data_mon) ->
	#data_mon{gerTypeID=GerID} = DataMon,
	GerBase = #gerBase{
					   gerTypeID            =GerID       
					  ,gerQuality           =DataMon#data_mon.gerQuality      
					  ,gerLevel             =DataMon#data_mon.gerLevel        
					  ,gerPos               =0
					  ,gerExp               =0
					  },
	GerAttr = #gerAttr{
					   gerAntiWei           =DataMon#data_mon.gerAntiWei     
					  ,gerAntiShu           =DataMon#data_mon.gerAntiShu     
					  ,gerAntiWu            =DataMon#data_mon.gerAntiWu      
					  ,gerAntiQun           =DataMon#data_mon.gerAntiQun
					  ,gerAttack            =DataMon#data_mon.gerAttack      
					  ,gerHpMax             =DataMon#data_mon.gerHpMax       
					  ,gerSpInit            =DataMon#data_mon.gerSpInit      
					  ,gerSpMax            =DataMon#data_mon.gerSpMax      
					  ,gerCritic            =DataMon#data_mon.gerCritic      
					  ,gerCriticReduce      =DataMon#data_mon.gerCriticReduce
					  ,gerDoom              =DataMon#data_mon.gerDoom        
					  ,gerMiss              =DataMon#data_mon.gerMiss        
					  ,gerAbsorb            =DataMon#data_mon.gerAbsorb      
					  ,gerDamageBack        =DataMon#data_mon.gerDamageBack  
					  ,gerReel              =DataMon#data_mon.gerReel        
					  ,gerReelReduce        =DataMon#data_mon.gerReelReduce  
					  ,gerPhyDefBite        =DataMon#data_mon.gerPhyDefBite  
					  ,gerPhyDef            =DataMon#data_mon.gerPhyDef      
					  ,gerMagDefBite        =DataMon#data_mon.gerMagDefBite  
					  ,gerMagDef            =DataMon#data_mon.gerMagDef
					  },
	#ger{
		 gerID=GerID
		,gerBase=GerBase
		,gerAttr=GerAttr
		,gerHp=DataMon#data_mon.gerHpMax
		,gerSp=DataMon#data_mon.gerSpInit
		}.
				
		   
%% @doc 
data_ger2ger(DataGer, GerLevel) when is_record(DataGer, data_ger) ->
	#data_ger{gerTypeID=GerID} = DataGer,
	GerBase = #gerBase{
					   gerTypeID            =GerID    
					  ,gerPos               =0
					  ,gerExp               =data_ger_level:get(GerLevel)
					  },
	GerAttr = #gerAttr{
					   gerAntiWei           =DataGer#data_ger.gerAntiWei     
					  ,gerAntiShu           =DataGer#data_ger.gerAntiShu     
					  ,gerAntiWu            =DataGer#data_ger.gerAntiWu      
					  ,gerAntiQun           =DataGer#data_ger.gerAntiQun
					  ,gerAttack            =DataGer#data_ger.gerAttack      
					  ,gerHpMax             =DataGer#data_ger.gerHpMax       
					  ,gerSpInit            =DataGer#data_ger.gerSpInit      
					  ,gerSpMax            =DataGer#data_ger.gerSpMax      
					  ,gerCritic            =DataGer#data_ger.gerCritic      
					  ,gerCriticReduce      =DataGer#data_ger.gerCriticReduce
					  ,gerDoom              =DataGer#data_ger.gerDoom        
					  ,gerMiss              =DataGer#data_ger.gerMiss        
					  ,gerAbsorb            =DataGer#data_ger.gerAbsorb      
					  ,gerDamageBack        =DataGer#data_ger.gerDamageBack  
					  ,gerReel              =DataGer#data_ger.gerReel        
					  ,gerReelReduce        =DataGer#data_ger.gerReelReduce  
					  ,gerPhyDefBite        =DataGer#data_ger.gerPhyDefBite  
					  ,gerPhyDef            =DataGer#data_ger.gerPhyDef      
					  ,gerMagDefBite        =DataGer#data_ger.gerMagDefBite  
					  ,gerMagDef            =DataGer#data_ger.gerMagDef
					  },
	#ger{
		 gerID=GerID
		,gerBase=GerBase
		,gerAttr=GerAttr
		,gerHp=DataGer#data_ger.gerHpMax
		,gerSp=DataGer#data_ger.gerSpInit
		}.
%% @doc 加经验，计算等级成长，
-spec add_exp(?int16, ?int64, ?int32, ?int16) -> 
		  {level_up, NewLevel::?int16, NewExp::?int64} | 
			  {level_not_up, OldLevel::?int16, NewExp::?int64} | 
			  {level_max, OldLevel::?int16, OldExp::?int64}.
add_exp(Level, Exp, AddExp, RoleLevel) ->
	MaxLevel = erlang:min(RoleLevel, data_common:get(max_ger_level)),
	MaxExp = data_ger_level:get(MaxLevel+1)-1,
	if MaxExp > Exp ->			
		   NewExp=erlang:min(MaxExp, Exp+AddExp),
		   NewLevel = data_ger_exp:get(NewExp),
		   if NewLevel > Level ->
				  {level_up, NewLevel, NewExp};
			  true ->
				  {level_not_up, Level, NewExp}
		   end;
	   true ->
		   {level_max, Level, Exp}
	end.

ger2p_ger_view(#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}}=_Ger) ->
	#p_ger_view{gerQuality=GerQuality,gerLevel=GerLevel,gerTypeID=GerTypeID};
ger2p_ger_view(#gerSimple{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}=_Ger) ->
	#p_ger_view{gerQuality=GerQuality,gerLevel=GerLevel,gerTypeID=GerTypeID}.

ger2p_ger_view_dtl(#ger{gerID=GerID
				  , gerBase=#gerBase{gerTypeID=GerTypeID, gerLevel=GerLevel, gerQuality=GerQuality, gerExp=GerExp}
				  , gerAttr=#gerAttr{gerAttack=GerAttack, gerHpMax=GerHpMax,gerFightPower=GerFightPower}}=_Ger)->
	#p_ger{gerID = GerID, gerTypeID=GerTypeID, gerLevel=GerLevel, gerQuality=GerQuality
		  		  , gerExp=GerExp, gerAttack=GerAttack, gerHpMax=GerHpMax,gerFightPower=GerFightPower}.
ger2p_ger_pos(#ger{gerID=GerID,gerBase=#gerBase{gerPos=GerPos}})->
	#p_ger_pos{gerID=GerID,gerPos=GerPos}.

new_ger2p_ger_view(#new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}) ->
	#p_ger_view{gerQuality=GerQuality,gerLevel=GerLevel,gerTypeID=GerTypeID}.

new_gerList2p_ger_view(#new_ger{}=NewGer) ->
	[new_ger2p_ger_view(NewGer)];
new_gerList2p_ger_view(List) when is_list(List) ->
	[new_ger2p_ger_view(E)||E<-List];
new_gerList2p_ger_view(_) ->
	[].

add_ger_list(NewGerList, Type, ArgID, Desc) when is_list(NewGerList) ->
	GerSimpleList = gen_ger_list(NewGerList),
	add_ger_list2(GerSimpleList),
	GerGatherList = [{E1,E2}||#new_ger{gerTypeID=E1,gerQuality=E2} <- NewGerList],
	role_gather:hook_add_ger_list(GerGatherList),
	LogList = [ [E#gerSimple.gerID,E#gerSimple.gerTypeID,E#gerSimple.gerLevel,E#gerSimple.gerQuality ]|| E<-GerSimpleList],
	RoleID = role_data:get_roleID(),
	{Date, _} = Time = erlang:localtime(),
	behavior_ger_add:log(RoleID, LogList, Date, Time, Type, ArgID, Desc),
	role_task_trigger:add_ger_trigger(GerSimpleList),
	GerSimpleList;
add_ger_list(NewGer, Type, ArgID, Desc) ->
	add_ger_list([NewGer], Type, ArgID, Desc).
	  
add_ger(NewGer, Type, ArgID, Desc) ->
	[GerSimple] = add_ger_list([NewGer], Type, ArgID, Desc),
	GerSimple.
	
gen_ger_list(NewGerList) ->
	[gen_ger_list2(E) || E<- NewGerList].
gen_ger_list2(NewGer) ->
	#new_ger{gerLevel=GerLevel,gerQuality=GerQuality,gerTypeID=GerTypeID} = NewGer,
	#gerSimple{gerExp=data_ger_level:get(GerLevel),gerLevel=GerLevel,gerID=tk_id:gen_gerID(),gerPos=0,gerQuality=GerQuality,gerTypeID=GerTypeID}.

add_ger_list2(GerSimpleList) ->
	GerBag = role_data:get_gerBag(),
	GerBag2 = GerSimpleList++GerBag,
	role_data:set_gerBag(GerBag2),
	List = lists:map(fun(E)->
					  ger2p_ger(E)
			  end, GerSimpleList),
	Notify = #sc_ger_new_list{newGerList=List},
	?sendself(Notify).
%% 	lists:foreach(fun(E) ->
%% 						  Notify = #sc_ger_new{newGer=ger2p_ger(E)},
%% 						  ?sendself(Notify)
%% 				  end, GerSimpleList).

get_ger_skills(GerTypeID, GerQuality) ->
	#data_ger{gerSkill1=GerSkill1,gerSkill2=GerSkill2,gerSkill3=GerSkill3,gerSkill4=GerSkill4,
			  gerSkill5=GerSkill5,gerSkill6=GerSkill6,gerSkill7=GerSkill7} = data_ger:get(GerTypeID),
	OpenList = ?SKILL_NEED_QUALITY_LIST,
	Num = lists:foldl(fun(E,N)->
						 case GerQuality>=E of
							 true->
								 N+1;
							 _->
								 N
						 end
				 end, 0,OpenList),
	SkillList = lists:sublist([GerSkill1,GerSkill2,GerSkill3,GerSkill4,GerSkill5,GerSkill6,GerSkill7],Num),
%% 	?ERR("=======>>>>>~p",[{GerQuality,[GerSkill1,GerSkill2,GerSkill3,GerSkill4,GerSkill5,GerSkill6,GerSkill7],SkillList}]),
	{NormalSkill, UniqueSkill, WakeSkill, GodSkill, EnterSkill,Unique2Skill,God2Skill} = 
		lists:foldl(fun(E, {Normal, Unique, Wake, God, Enter,Unique2,God2}=Acc)->
							if E =/= 0 ->
								   #data_skill{skillType=SkillType}=data_skill:get(E),
								   case SkillType of
									   enter ->
												  {Normal, Unique, Wake, God, [E|Enter], Unique2, God2};
									   god ->
												  {Normal, Unique, Wake, [E|God], Enter, Unique2, God2};
									   normal ->
										   {[E|Normal], Unique, Wake, God, Enter, Unique2, God2};
									   wake ->
												  {Normal, Unique, [E|Wake], God, Enter, Unique2, God2};
									   god2 ->
												  {Normal, Unique, Wake, God, Enter, Unique2, [E|God2]};
									   unique ->
										   {Normal, [E|Unique], Wake, God, Enter, Unique2, God2};
									   unique2 ->
												  {Normal, Unique, Wake, God, Enter, [E|Unique2], God2};
									   X->
										   ?ERR("other type :~w",[X])
								   end;
							   true->
								   Acc 
							end
					end, {[],[],[],[],[],[],[]}, SkillList),
	if GerQuality >= ?UNIQUE2_REPLACE_UNIQUE_NEED_QUALITY andalso Unique2Skill =/= [] ->
		   UniqueSkill2 = Unique2Skill;
	   true ->
		   UniqueSkill2 = UniqueSkill
	end,
	#gerSkill{normal=NormalSkill, unique=UniqueSkill2,wake=WakeSkill, god=GodSkill, god2=God2Skill, enter=EnterSkill}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-include("def_item.hrl").
%% 计算最强阵容


get_destiny_equip(GerTypeID) ->
	#data_ger{destinyIDList=DL} = data_ger:get(GerTypeID),
	lists:foldl(fun(E,Acc) ->
						#data_destiny{destinyNeedList=ItemTypeIDList,destinyType=DT}=data_destiny:get(E),
						if DT =:= 2->
							   ItemTypeIDList ++ Acc;
						   true ->
							   Acc
						end
				end, [], DL).

best_equip(?weapon)->
	11010;
best_equip(?armor) ->
	12004;
best_equip(?horse) ->
	13004.

best_equip_for_ger(GerTypeID) ->
	DestEquipIDList = get_destiny_equip(GerTypeID),
	DestEquipTypeList = [T||E<-DestEquipIDList, #data_item{itemType=T}<-[data_item:get(E)]],
	RestEquipTypeList = [?weapon,?armor, ?horse]--DestEquipTypeList,
	EquipIDList = DestEquipIDList ++ [best_equip(E)||E<-RestEquipTypeList],
	Treasure = [14010,14002,14012],
	List =
	[hd(item_lib:create_equip_with_attr(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_MAIN_EQUIP,itemTypeID=E}, data_item:get(E)))||E<-EquipIDList] ++
	[hd(item_lib:create_equip_with_attr(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_TREASURE,itemTypeID=E}, data_item:get(E)))||E<-Treasure],
	{_,L}=lists:foldl(fun(E,{Pos, Acc}) ->
						{Pos+1, [E#item{itemPos=Pos}|Acc]}
				end, {1, []}, List),
	L.

good_equip_list(Type) ->
	MaxItemLevel = data_common:get(max_role_level)+4,
	[hd(item_lib:create_equip(#new_item{itemLevel=MaxItemLevel,itemNum=1,itemRank=19,itemTypeID=ItemTypeID},DataItem))||
	   ItemTypeID <- data_item:get_list(),
	   #data_item{itemType=ItemType,itemStar=ItemStar} = DataItem<-[data_item:get(ItemTypeID)],
	   ItemType =:= Type, 
	   ItemStar=:= 4].


%% 比较两把武器，哪个厉害点
is_equip_better_than(ItemTypeID1, ItemTypeID2) ->
	EquipList1 = item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=19,itemTypeID=ItemTypeID1}),
	
	EquipList2 = item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=19,itemTypeID=ItemTypeID2}),
	
	is_equip_better_than2(EquipList1, EquipList2).
is_equip_better_than2(EquipList1, EquipList2) ->
	L=[ger_lib:is_equip_better_than2(EquipList1,EquipList2,E)||E<-data_ger:get_list(),E<20000],
	R = length([E||E<-L,E])/length(L),
	io:format("~w\n",[R]),
	R > 0.5.
is_equip_better_than2(EquipList1, EquipList2, TestGerTypeID) ->
	%% 生成测试用队伍
	GenTeam = fun(EquipList) ->
					  Team = [?change_pos(Ger, Pos)||Pos<-lists:seq(1,6), Ger <-[ger_attr:new_ger(TestGerTypeID, 100, 19, EquipList, [])] ],
					  Team
			  end,
	is_better_than(GenTeam(EquipList1), GenTeam(EquipList2)).

get_best_equip(ItemType) ->
	EquipList = good_equip_list(ItemType),
	L = lists:sort(fun(A,B) -> is_equip_better_than2([A],[B]) end,EquipList),
	[E||#item{itemTypeID=E}<-L].

%% 比较两个队伍，哪个厉害点
is_better_than(Team1, Team2) ->
	{Result1, St1, {_,_,_,_}} = role_fight:new(Team1, Team2,{0,0},{0,0}),
	{Result2, St2, {_,_,_,_}} = role_fight:new(Team2, Team1,{0,0},{0,0}),
	%% Team1连输两次
	if Result1 =:= false andalso Result2 =:= true ->		
			false;
	   %% Team1 连赢两次
	   Result1 =:= true andalso Result2 =:= false ->
		   true;
	   %% 各有胜负时，比较谁杀得快
	   true ->
		   F = fun(M) ->length(M#sc_fight_request.actionList) end,
		   %% 先手必胜
		   if Result1 =:= true ->
				  io:format("tag...\n"),
				  F(St1) >= F(St2);
			  true ->
				  F(St2) >= F(St1)
		   end
	end.

to_p_ger_guard(GuardGer, RoleGuardInfo) ->
    #gerSimple{gerID=GerID
               ,gerPos=GerPos
               ,gerQuality=GerQuality
               ,gerTypeID=GerTypeID} = GuardGer,
    case lists:keyfind(GerPos, #guard_info.pos, RoleGuardInfo) of
        false ->
            HighList = [];
        #guard_info{list=HighList} ->
            next
    end,
    #p_ger_guard{
                 gerPos=GerPos
                     ,gerID=GerID
                     ,gerTypeID=GerTypeID
                     ,gerQuality=GerQuality
                     ,baseList=calc_base_list(GerTypeID, GerQuality)
                     ,highList=HighList}.

calc_base_list(GerTypeID, GerQuality) ->
    #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
    HPStarAdd = lists:keyfind(GerStar, 1, data_guard:get(hp_star)),
    AtkStarAdd = lists:keyfind(GerStar, 1, data_guard:get(atk_star)),
    HPQualityAdd = lists:keyfind(GerQuality, 1, data_guard:get(hp_quality)),
    AtkQualityAdd = lists:keyfind(GerQuality, 1, data_guard:get(atk_quality)),
    [#p_ger_guard_attr{attrType=AttrType,addValue=AddVal}||{_, AttrType,AddVal}<-[HPStarAdd,AtkStarAdd,HPQualityAdd,AtkQualityAdd]].

calc_guard_attr(GerPos, GuardPosList, RoleGuardInfo) ->
    case lists:keyfind(GerPos, #gerSimple.gerPos, GuardPosList) of
        false ->
            #add_attr{};
        #gerSimple{gerTypeID=GerTypeID, gerQuality=GerQuality} ->
            BaseList = calc_base_list(GerTypeID, GerQuality),
            case lists:keyfind(GerPos, #guard_info.pos, RoleGuardInfo) of
                false ->
                    HighList = [];
                #guard_info{list=HighList} ->
                    next
            end,
            calc_guard_attr2(BaseList ++ HighList, #add_attr{})
    end.

calc_guard_attr2([], AddAttr) ->
    AddAttr;
calc_guard_attr2([#p_ger_guard_attr{attrType=AttrType,addValue=AddValue}|List], AddAttr) ->
    calc_guard_attr2(List, add_guard_attr(AttrType, AddValue, AddAttr)).

-define(gerAttack,1). % 攻击力
-define(gerHpMax,2).% 血量上限
-define(gerSpInit,3).% 怒气初始值
-define(gerSpMax,4).% 怒气最大值
-define(gerCritic,5).% 暴击
-define(gerCriticReduce,6).% 暴击抵抗
-define(gerDoom,7).% 命中
-define(gerMiss,8).% 闪避
-define(gerAbsorb,9).% 吸血
-define(gerDamageBack,10).% 反弹
-define(gerReel,11).% 眩晕
-define(gerReelReduce,12).% 眩晕抵抗
-define(gerPhyDefBite,13).% 破甲
-define(gerPhyDef,14).% 护甲
-define(gerMagDefBite,15).% 法穿 
-define(gerMagDef,16).% 法抗
-define(gerAttackAddtion,17).% 攻击增加万分比
-define(gerHpMaxAddtion,18).% 血量增加万份比/复活时的血量万分比

add_guard_attr(?gerAttack, AddValue, #add_attr{gerAttack=GerAttack}=AddAttr) ->
    AddAttr#add_attr{gerAttack=GerAttack+AddValue};
add_guard_attr(?gerHpMax, AddValue,  #add_attr{gerHpMax=GerHpMax}=AddAttr) ->
    AddAttr#add_attr{gerHpMax=GerHpMax+AddValue};
add_guard_attr(?gerSpInit, AddValue,  #add_attr{gerSpInit=GerSpInit}=AddAttr) ->
    AddAttr#add_attr{gerSpInit=GerSpInit+AddValue};
add_guard_attr(?gerSpMax, AddValue,  #add_attr{gerSpMax=GerSpMax}=AddAttr) ->
    AddAttr#add_attr{gerSpMax=GerSpMax+AddValue};
add_guard_attr(?gerCritic, AddValue,  #add_attr{gerCritic=GerCritic}=AddAttr) ->
    AddAttr#add_attr{gerCritic=GerCritic+AddValue};
add_guard_attr(?gerCriticReduce, AddValue,  #add_attr{gerCriticReduce=GerCriticReduce}=AddAttr) ->
    AddAttr#add_attr{gerCriticReduce=GerCriticReduce+AddValue};
add_guard_attr(?gerDoom, AddValue,  #add_attr{gerDoom=GerDoom}=AddAttr) ->
    AddAttr#add_attr{gerDoom=GerDoom+AddValue};
add_guard_attr(?gerMiss, AddValue,  #add_attr{gerMiss=GerMiss}=AddAttr) ->
    AddAttr#add_attr{gerMiss=GerMiss+AddValue};
add_guard_attr(?gerAbsorb, AddValue,  #add_attr{gerAbsorb=GerAbsorb}=AddAttr) ->
    AddAttr#add_attr{gerAbsorb=GerAbsorb+AddValue};
add_guard_attr(?gerDamageBack, AddValue,  #add_attr{gerDamageBack=GerDamageBack}=AddAttr) ->
    AddAttr#add_attr{gerDamageBack=GerDamageBack+AddValue};
add_guard_attr(?gerReel, AddValue,  #add_attr{gerReel=GerReel}=AddAttr) ->
    AddAttr#add_attr{gerReel=GerReel+AddValue};
add_guard_attr(?gerReelReduce, AddValue,  #add_attr{gerReelReduce=GerReelReduce}=AddAttr) ->
    AddAttr#add_attr{gerReelReduce=GerReelReduce+AddValue};
add_guard_attr(?gerPhyDefBite, AddValue,  #add_attr{gerPhyDefBite=GerPhyDefBite}=AddAttr) ->
    AddAttr#add_attr{gerPhyDefBite=GerPhyDefBite+AddValue};
add_guard_attr(?gerPhyDef, AddValue,  #add_attr{gerPhyDef=GerPhyDef}=AddAttr) ->
    AddAttr#add_attr{gerPhyDef=GerPhyDef+AddValue};
add_guard_attr(?gerMagDefBite, AddValue,  #add_attr{gerMagDefBite=GerMagDefBite}=AddAttr) ->
    AddAttr#add_attr{gerMagDefBite=GerMagDefBite+AddValue};
add_guard_attr(?gerMagDef, AddValue,  #add_attr{gerMagDef=GerMagDef}=AddAttr) ->
    AddAttr#add_attr{gerMagDef=GerMagDef+AddValue};
add_guard_attr(?gerAttackAddtion, AddValue,  #add_attr{gerAttackAddtion=GerAttackAddtion}=AddAttr) ->
    AddAttr#add_attr{gerAttackAddtion=GerAttackAddtion+AddValue};
add_guard_attr(?gerHpMaxAddtion, AddValue,  #add_attr{gerHpMaxAddtion=GerHpMaxAddtion}=AddAttr) ->
    AddAttr#add_attr{gerHpMaxAddtion=GerHpMaxAddtion+AddValue};
add_guard_attr(_, _, AddAttr) ->
    AddAttr.
