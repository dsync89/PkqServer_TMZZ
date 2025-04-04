%% @author admin
%% @doc 武将属性处理
%% Created 2013-3-6


-module(ger_attr).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% API functions

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

recacl_gers() ->
	PosListT=role_data:get_posList(),
	PosList=[recacl(E, lists:delete(E, PosListT))||E<-PosListT],
	role_data:set_posList2(PosList) ,
	recacl_gers(PosList).

%% 计入副将加成，通知客户端更新武将主要属性，刷新战斗力，更新总战斗力
recacl_gers(PosList) ->	
	refresh_lieu_add_attr(),
	{AtkAdd, HpAdd}=role_data:get_lieu_add_attr(),
	PosList2=
	lists:foldl(fun(Tar,GerListAcc)->
						AddAttr=#add_attr{gerAttackAddtion=AtkAdd,gerHpMaxAddtion=HpAdd},
						Tar2=add_enter_skill_attr(Tar, AddAttr),
						ger_lib:notify_update(Tar2),
						[Tar2|GerListAcc]
				end,[],PosList), 
	role_data:set_posListT(refresh_fightPower(PosList2, AtkAdd, HpAdd)).

%% 计入副将加成，单个武将的
recacl_gers(Ger, PosList) ->
    refresh_lieu_add_attr(),
    {AtkAdd, HpAdd}=role_data:get_lieu_add_attr(),
	AddAttr = #add_attr{gerAttackAddtion=AtkAdd,gerHpMaxAddtion=HpAdd},
    Ger2 = add_enter_skill_attr(Ger, AddAttr),
    ger_lib:notify_update(Ger2),
    Ger3 = refresh_fightPower_ger(Ger2, AtkAdd, HpAdd, PosList),
    role_data:set_posListT([Ger3|PosList]).

%% 计入套装、守护武将加成，单个武将的
recacl_ger_all_equip(#ger{gerID=GerID}=Ger, AddAttr1) ->
    AddAttr2 = recal_all_equipment(GerID),
    AddAttr = append_add_attr(AddAttr1, AddAttr2),
    add_enter_skill_attr(Ger, AddAttr);
recacl_ger_all_equip(#gerSimple{}=Ger, _) ->
    Ger.

refresh_lieu_add_attr()->
	role_data:set_lieu_add_attr(calc_lieu_add_attr()).

calc_lieu_add_attr()->
	LPosListT = role_data:get_lieuposList(),
	LPosList = [recacl_lieu(E,LPosListT)||E<-LPosListT],
	role_data:set_lieuposList2(LPosList),
	LieuInfo=role_data:get_lieutenantInfo(),
	lists:foldl(fun(#ger{gerBase=GerBase,gerAttr=GerAttr},{AttAddAcc,HpAddAcc})->
						#gerAttr{gerFightPower=FightPower}=GerAttr,
						#gerBase{gerPos=Pos}=GerBase,
						case lists:keyfind(Pos, #t_lieu.pos, LieuInfo) of
							#t_lieu{infoID2=ID2, infoID3=ID3}->
								Add=data_lieu_add:get(FightPower),
								Value2 = data_lieu_clo_setting:get(ID2),
								Value3 = data_lieu_clo_setting:get(ID3),
								{AttAddAcc+Add+Value2,HpAddAcc+Add+Value3};
							_X ->
								{AttAddAcc, HpAddAcc}
						end
				end,{0,0},LPosList).
	
get_original_posList()->
	PosList=role_data:get_posList(), 
	lists:foldl(fun(Ger, Acc)->
						[recacl(Ger, PosList)|Acc]end, [], PosList).

get_original_lieu_posList()->
	PosList=role_data:get_lieuposList(),
	lists:foldl(fun(Ger, Acc)->
						[recacl_lieu(Ger, PosList)|Acc]end, [], PosList).

%% 上阵武将重算属性
recacl_f(GerID) ->
	PosList = role_data:get_posList(),
	case lists:keytake(GerID, #ger.gerID, PosList) of
		{value, #ger{}=Ger, PosList2} ->
			Ger2 = recacl(Ger, PosList2),
            NewPosList = [Ger2|PosList2],
            erlang:put(?posList, NewPosList),
            ger_attr:recacl_gers(Ger2, PosList2),
            role_data:clear_mainGerTypeID();
		_ ->
			LPosList=role_data:get_lieuposList(),
			{value, #ger{}=Ger, LPosList2} = lists:keytake(GerID, #ger.gerID, LPosList),
			Ger2 = recacl(Ger, LPosList2),
			LPosList3 = [Ger2|LPosList2],
			role_data:set_lieuposList(LPosList3)
	end.

%% 值推送主将战斗力变化
refresh_fightPower(GerList,AtkAdd, HpAdd) ->
	{GerList2, UpdatePowerList} = 
	lists:mapfoldl(fun(Ger, Acc) ->
					  #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
					  #gerBase{gerPos=GerPos} = GerBase,
						{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList),
					  FightPower = cacl_fight_power(GerBase, Ger, EnterSkillHpAdd+HpAdd, EnterSkillAtkAdd+AtkAdd),
					  GerAttr2 = GerAttr#gerAttr{gerFightPower=FightPower},
					  Ger2 = Ger#ger{gerAttr=GerAttr2},
					  if GerAttr#gerAttr.gerFightPower == FightPower ->
							 {Ger2, Acc};
						 true ->
					  		{Ger2, [#p_ger_power{fightPower=FightPower,pos=GerPos}|Acc]}
					  end
				   end, [], GerList),
	if UpdatePowerList =/= [] ->
		   ?sendself(#sc_ger_refresh_power{gerPowerList=UpdatePowerList});
	   true ->
		   ignore
	end,
	GerList2.

%% 推送主将战斗力变化,单个武将
refresh_fightPower_ger(Ger,AtkAdd, HpAdd, GerList) ->
    #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
    #gerBase{gerPos=GerPos} = GerBase,
    {EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, [Ger|GerList]),
    FightPower = cacl_fight_power(GerBase, Ger, EnterSkillHpAdd + HpAdd, EnterSkillAtkAdd + AtkAdd),
    GerAttr2 = GerAttr#gerAttr{gerFightPower=FightPower},
    Ger2 = Ger#ger{gerAttr=GerAttr2},
    if GerAttr#gerAttr.gerFightPower =:= FightPower ->
           UpdatePowerList = [];
       true ->
           UpdatePowerList = [#p_ger_power{fightPower=FightPower,pos=GerPos}]
    end,
    if UpdatePowerList =/= [] ->
           ?sendself(#sc_ger_refresh_power{gerPowerList=UpdatePowerList});
       true ->
           ignore
    end,
    Ger2.

refresh_other_fightPower(GerList,AtkAdd, HpAdd) ->
	{GerList2, _UpdatePowerList} = 
	lists:mapfoldl(fun(Ger, Acc) ->
					  #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
					  #gerAttr{gerAttack=GerAttack,gerHpMax=GerHpMax}=GerAttr,
					  GerAttrT = GerAttr#gerAttr{gerAttack=erlang:trunc(GerAttack*(1+AtkAdd/10000)),gerHpMax=erlang:trunc(GerHpMax*(1+HpAdd/10000))},
					  GerT = Ger#ger{gerAttr=GerAttrT},
					  #gerBase{gerPos=GerPos} = GerBase,
					  {EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList),
					  FightPower = cacl_fight_power(GerBase, GerT, EnterSkillHpAdd+HpAdd, EnterSkillAtkAdd+AtkAdd),
					  GerAttr2 = GerAttrT#gerAttr{gerFightPower=FightPower},
					  Ger2 = GerT#ger{gerAttr=GerAttr2},
					  if GerAttr#gerAttr.gerFightPower == FightPower ->
							 {Ger2, Acc};
						 true ->
					  		{Ger2, Acc}
					  end
				   end, [], GerList),
	GerList2.

refresh_fightPower(GerList) ->
	{GerList2, _UpdatePowerList} = 
	lists:mapfoldl(fun(Ger, Acc) ->
					  #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
					  #gerBase{gerPos=GerPos} = GerBase,
						{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList),
					  FightPower = cacl_fight_power(GerBase, Ger, EnterSkillHpAdd, EnterSkillAtkAdd),
					  GerAttr2 = GerAttr#gerAttr{gerFightPower=FightPower},
					  Ger2 = Ger#ger{gerAttr=GerAttr2},
					  if GerAttr#gerAttr.gerFightPower == FightPower ->
							 {Ger2, Acc};
						 true ->
					  		{Ger2, [#p_ger_power{fightPower=FightPower,pos=GerPos}|Acc]}
					  end
				   end, [], GerList),
%% 	if UpdatePowerList =/= [] ->
%% 		   ?sendself(#sc_ger_refresh_power{gerPowerList=UpdatePowerList});
%% 	   true ->
%% 		   ignore
%% 	end,
	GerList2.


					  
%% 计算出战武将的属性,计入属性包含基本、天命、装备、wake技能、god技能，enter技能的属性加成只计入战斗力
recacl(Ger, GerList) ->
	if is_record(Ger, ger) ->
		   #ger{gerBase=#gerBase{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos},gerID=GerID} = Ger;
	   true ->
		   #gerSimple{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerID=GerID} = Ger
	end,
	EquipList = role_data:get_equip(GerID),
    GuardPosList = role_data:get_guardPosList(),
    #role_guard{infoList=RoleGuardInfo} = role_data:get_roleGuardInfo(),
    GuardAttr = ger_lib:calc_guard_attr(GerPos, GuardPosList, RoleGuardInfo),
	GerList2=[Ger|GerList],
	PosTypeIDList =
		lists:map(fun(#ger{gerBase=#gerBase{gerTypeID=E}}) ->
						  E;
					 (#gerSimple{gerTypeID=E}) ->
						  E
				  end, GerList2),
	{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList2),
	SpLieuList=get_sp_lieu_typeIDList(role_data:get_lieutenantInfo()),
	recacl(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, lists:delete(GerTypeID, SpLieuList++PosTypeIDList), EnterSkillHpAdd, EnterSkillAtkAdd, GuardAttr, true).
	
recacl_lieu(Ger, GerList) ->
	LieuInfo = role_data:get_lieutenantInfo(),
	if is_record(Ger, ger) ->
		   #ger{gerBase=#gerBase{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos},gerID=GerID} = Ger;
	   true ->
		   #gerSimple{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerID=GerID} = Ger
	end,
	#t_lieu{infoID1=ID1}=get_lieu_ger_info(GerPos, LieuInfo),
	EquipList = role_data:get_equip(GerID),
	GerList2=[Ger|GerList],
	PosTypeIDList =
		lists:map(fun(#ger{gerBase=#gerBase{gerTypeID=E}}) ->
						  E;
					 (#gerSimple{gerTypeID=E}) ->
						  E
				  end, GerList2),
	%{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList2),
	PosTypeIDList2=
		if GerTypeID =:= ID1 ->
			   PosTypeIDList ++ get_main_ger_typeIDList();
		   true ->
			   PosTypeIDList
		end,
	recacl(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, lists:delete(GerTypeID, PosTypeIDList2), 0, 0, 0, false).

get_main_ger_typeIDList()->
	lists:foldl(fun(#ger{gerBase=#gerBase{gerTypeID=E}},Acc)->
						[E|Acc];
				   (#gerSimple{gerTypeID=E},Acc)->
						[E|Acc]
				end, [], role_data:get_posList()).

get_sp_lieu_typeIDList(LieuInfo) ->
	LieuList=role_data:get_lieuposList(),
	lists:foldl(fun(Ger, Acc)->
						if is_record(Ger, ger) ->
							   #ger{gerBase=#gerBase{gerPos=Pos,gerTypeID=GerTypeID}} = Ger;
						   true ->
							   #gerSimple{gerPos=Pos,gerTypeID=GerTypeID} = Ger
						end,
						#t_lieu{infoID1=ID1}=get_lieu_ger_info(Pos, LieuInfo),
						if GerTypeID =:= ID1 ->
							   [GerTypeID|Acc];
						   true ->
							   Acc
						end
				end, [], LieuList).

get_lieu_ger_info(Pos, LieuInfo)->
	case lists:keyfind(Pos, #t_lieu.pos, LieuInfo) of
		false ->
			{t_lieu,Pos,0,0,0,0,0,0};
		#t_lieu{}=X ->
			X
	end.  

cacl_enter_skill_add_fight_power(TarPos, GerList) ->
	PosList = 
	lists:foldl(fun(#gerSimple{gerPos=GerPos}, Acc) ->
							 [GerPos|Acc];
						(#ger{gerBase=#gerBase{gerPos=GerPos}}, Acc) ->
							 [GerPos|Acc]
					 end, [], GerList),
	lists:foldl(fun(#gerSimple{gerTypeID=GerTypeID,gerPos=GerPos,gerQuality=GerQuality}, Acc) ->
							 cacl_enter_skill_add_fight_power2(TarPos, GerPos, GerQuality, GerTypeID, Acc, PosList);
						(#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerPos=GerPos,gerQuality=GerQuality}}, Acc) ->
							 cacl_enter_skill_add_fight_power2(TarPos, GerPos, GerQuality, GerTypeID, Acc, PosList)
					 end, {0,0}, GerList).

cacl_enter_skill_add_fight_power2(TarPos, GerPos, GerQuality, GerTypeID, Acc, PosList) ->
	#gerSkill{enter=GerEnterSkills} = ger_lib:get_ger_skills(GerTypeID, GerQuality),
	lists:foldl(fun(GerSkillEnterID,Acc0)-> 
						case data_skill:get(GerSkillEnterID) of
							#data_skill{targetSelect=TarSelect,gerEnterOrGodAdd=#add_attr{gerHpMaxAddtion=HpAdd,gerAttackAddtion=AtkAdd}} 
							  when HpAdd =/= 0 orelse AtkAdd =/= 0->
								SelectList = role_fight:select_posList(TarSelect, GerPos, PosList),
								if is_list(SelectList) ->
									   case lists:member(TarPos, SelectList) of
										   true ->
											   {A,B}=Acc0,
											   {HpAdd+A, AtkAdd+B};
										   false ->
											   Acc0
									   end;							   
								   true ->
									   Acc0
								end;
							_ ->
								Acc0
						end
				end,Acc,GerEnterSkills).

typeList(EquipList) ->
	[ItemTypeID || #item{itemTypeID=ItemTypeID} <- EquipList].

check_destiny_add(1, ArmedGerTypeIDList, _EquipTypeIDList, DesNeedList) ->
	lists:all(fun(E) -> lists:member(E, ArmedGerTypeIDList) end, DesNeedList);
check_destiny_add(2, _ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) ->
	lists:all(fun(E) -> lists:member(E, EquipTypeIDList) end, DesNeedList).

cacl_destiny_add([], _ArmedGerTypeIDList, _EquipList) ->
	{0,0};
cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipList) ->
	cacl_destiny_add(DesIDList, ArmedGerTypeIDList, typeList(EquipList), 0,0).


cacl_destiny_add([DesID|DesIDList], ArmedGerTypeIDList, EquipTypeIDList, HpAdd, AttackAdd) ->
	#data_destiny{destinyType=DesType, destinyNeedList=DesNeedList, addAttr=DesAddAttr} = data_destiny:get(DesID),
	case check_destiny_add(DesType, ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) of
		false ->
			cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipTypeIDList, HpAdd, AttackAdd);
		true ->
			#add_attr{gerHpMaxAddtion=GerHpMaxAddtion,gerAttackAddtion=GerAttackAddtion}=DesAddAttr,
			cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipTypeIDList, GerHpMaxAddtion+HpAdd, GerAttackAddtion+AttackAdd)
	end;
cacl_destiny_add([], _ArmedGerTypeIDList, _EquipTypeIDList, HpAdd, AttackAdd) ->
	{HpAdd, AttackAdd}.

%% @doc 重算属性,计入属性包含基本、天命、装备、wake技能、god技能，enter技能的属性加成只计入战斗力
recacl(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, ArmedGerTypeIDList, EnterSkillHpAdd, EnterSkillAtkAdd, GuardAttr, Type) ->
	#gerSkill{wake=WakeSkills,god=GodSkills} = ger_lib:get_ger_skills(GerTypeID, Quality),
	
	#data_ger{gerHpMax=Hp,gerAttack=Atk,destinyIDList=DesIDList} = DataGer = data_ger:get(GerTypeID),
	{DesHpAdd, DesAttackAdd} = cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipList),
	Hp2 = trunc(Hp *(1+(Level-1)*0.1)* math:pow(2,Quality*0.1) * (1+DesHpAdd/10000)),
	Atk2= trunc(Atk*(1+(Level-1)*0.1)* math:pow(2,Quality*0.1) * (1+DesAttackAdd/10000)),
	EquipAddAttr = recacl_equip(EquipList),
	
	%% 无双技能
	if Type ->
		   AllAttr1 = lists:foldl(fun(SkillID,EquipAddAttrAcc)->
										  case data_skill:get(SkillID) of
											  #data_skill{gerEnterOrGodAdd=SkillAdd}->
												  append_add_attr(SkillAdd, EquipAddAttrAcc);
											  _ ->
												  EquipAddAttrAcc
										  end
								  end, EquipAddAttr, [role_fight:random_one_skill(WakeSkills)]),
		   %% 神将技能,这里只计算克制属性，其他属性由于是每次攻击时都会增加，在战斗中计算
		   AllAttr9 = lists:foldl(fun(SkillID,AllAttr1Acc)->
										  case data_skill:get(SkillID) of
											  #data_skill{gerEnterOrGodAdd=SkillAdd}->
												  append_add_attr_only_Anti(AllAttr1Acc, SkillAdd);
											  _ ->
												  AllAttr1Acc
										  end
								  end, AllAttr1, [role_fight:random_one_skill(GodSkills)]);
	   true ->
		   AllAttr9 = EquipAddAttr
	end,
	Hp3 = trunc((EquipAddAttr#add_attr.gerHpMax         +Hp2)),
	Atk3 = trunc((EquipAddAttr#add_attr.gerAttack        +Atk2) ),
	GerAttr2 = #gerAttr{
				    gerAntiWei      	= DataGer#data_ger.gerAntiWei   +  AllAttr9#add_attr.gerAntiWei      			
				   ,gerAntiShu      	= DataGer#data_ger.gerAntiShu   +  AllAttr9#add_attr.gerAntiShu      			
				   ,gerAntiWu       	= DataGer#data_ger.gerAntiWu    +  AllAttr9#add_attr.gerAntiWu       			
				   ,gerAntiQun      	= DataGer#data_ger.gerAntiQun   +  AllAttr9#add_attr.gerAntiQun
				   ,gerAttack       	= Atk3
				   ,gerHpMax        	= Hp3	
				   ,gerSpInit       	= DataGer#data_ger.gerSpInit       		+  AllAttr9#add_attr.gerSpInit       			
				   ,gerSpMax       	= DataGer#data_ger.gerSpMax       		+  AllAttr9#add_attr.gerSpMax       			
				   ,gerCritic       	= DataGer#data_ger.gerCritic       		+  AllAttr9#add_attr.gerCritic       			
				   ,gerCriticReduce 	= DataGer#data_ger.gerCriticReduce 		+  AllAttr9#add_attr.gerCriticReduce 			
				   ,gerDoom         	= DataGer#data_ger.gerDoom         		+  AllAttr9#add_attr.gerDoom         			
				   ,gerMiss         	= DataGer#data_ger.gerMiss         		+  AllAttr9#add_attr.gerMiss         			
				   ,gerAbsorb       	= DataGer#data_ger.gerAbsorb       		+  AllAttr9#add_attr.gerAbsorb       			
				   ,gerDamageBack   	= DataGer#data_ger.gerDamageBack   		+  AllAttr9#add_attr.gerDamageBack   			
				   ,gerReel         	= DataGer#data_ger.gerReel         		+  AllAttr9#add_attr.gerReel         			
				   ,gerReelReduce   	= DataGer#data_ger.gerReelReduce   		+  AllAttr9#add_attr.gerReelReduce   			
				   ,gerPhyDefBite   	= DataGer#data_ger.gerPhyDefBite   		+  AllAttr9#add_attr.gerPhyDefBite   			
				   ,gerPhyDef       	= DataGer#data_ger.gerPhyDef       		+  AllAttr9#add_attr.gerPhyDef       			
				   ,gerMagDefBite   	= DataGer#data_ger.gerMagDefBite   		+  AllAttr9#add_attr.gerMagDefBite   			
				   ,gerMagDef       	= DataGer#data_ger.gerMagDef       		+  AllAttr9#add_attr.gerMagDef       	
								   },
	GerBase2 = #gerBase{		
						gerExp=GerExp
					   ,gerPos=GerPos
					   ,gerTypeID=GerTypeID    			
							   ,gerLevel=Level
							   ,gerQuality=Quality
							  },
	GerT=#ger{gerID=GerID, gerAttr=GerAttr2, gerBase=GerBase2, gerHp=Hp3, gerSp=GerAttr2#gerAttr.gerSpInit},
	FightPower = cacl_fight_power(GerBase2,GerT, EnterSkillHpAdd, EnterSkillAtkAdd),
	GerAttr3 = GerAttr2#gerAttr{gerFightPower=FightPower},
	recacl_ger_all_equip(GerT#ger{gerAttr=GerAttr3}, GuardAttr).

new_ger(GerTypeID, Level, Quality, EquipList, TypeIDList) ->
	recacl(tk_id:gen_gerID(), GerTypeID, 0, Level, data_ger_level:get(Level), Quality, EquipList, lists:delete(GerTypeID, TypeIDList), 0, 0, 0, true).

new_mon(GerTypeID, Level, Quality, EquipList, TypeIDList) ->
	recacl(GerTypeID, GerTypeID, 0, Level, data_ger_level:get(Level), Quality, EquipList, lists:delete(GerTypeID, TypeIDList), 0, 0, 0, true).

%% 重算所有装备带来的属性加成
-spec recacl_equip(EquipList :: [#item{}]) -> #add_attr{}.
recacl_equip([]) ->
	#add_attr{};
recacl_equip([Head|EquipList]) ->
	lists:foldl(fun(#item{addAttr=AddAttr}, Acc) ->
						append_add_attr(AddAttr, Acc)
				end, Head#item.addAttr, EquipList).

recal_all_equipment(GerID)->
	lists:foldl(fun(AddAttr, Acc) ->
						append_add_attr(AddAttr, Acc)
				end, #add_attr{}, role_allequipment:get_ger_all_equipment_add_attr_list(GerID)).


append_add_attr(0,0) ->
	#add_attr{};
append_add_attr(0, AddAttrB) ->
	AddAttrB;
append_add_attr(AddAttrA,0) ->
	AddAttrA;
append_add_attr(AddAttrA, AddAttrB) ->
	#add_attr{
			  gerAttack          = AddAttrA#add_attr.gerAttack       +AddAttrB#add_attr.gerAttack       
			 ,gerHpMax           = AddAttrA#add_attr.gerHpMax        +AddAttrB#add_attr.gerHpMax        
			 ,gerSpInit          = AddAttrA#add_attr.gerSpInit       +AddAttrB#add_attr.gerSpInit       
			 ,gerSpMax          = AddAttrA#add_attr.gerSpMax       +AddAttrB#add_attr.gerSpMax       
			 ,gerCritic          = AddAttrA#add_attr.gerCritic       +AddAttrB#add_attr.gerCritic       
			 ,gerCriticReduce    = AddAttrA#add_attr.gerCriticReduce +AddAttrB#add_attr.gerCriticReduce 
			 ,gerDoom            = AddAttrA#add_attr.gerDoom         +AddAttrB#add_attr.gerDoom         
			 ,gerMiss            = AddAttrA#add_attr.gerMiss         +AddAttrB#add_attr.gerMiss         
			 ,gerAbsorb          = AddAttrA#add_attr.gerAbsorb       +AddAttrB#add_attr.gerAbsorb       
			 ,gerDamageBack      = AddAttrA#add_attr.gerDamageBack   +AddAttrB#add_attr.gerDamageBack   
			 ,gerReel            = AddAttrA#add_attr.gerReel         +AddAttrB#add_attr.gerReel         
			 ,gerReelReduce      = AddAttrA#add_attr.gerReelReduce   +AddAttrB#add_attr.gerReelReduce   
			 ,gerPhyDefBite      = AddAttrA#add_attr.gerPhyDefBite   +AddAttrB#add_attr.gerPhyDefBite   
			 ,gerPhyDef          = AddAttrA#add_attr.gerPhyDef       +AddAttrB#add_attr.gerPhyDef       
			 ,gerMagDefBite      = AddAttrA#add_attr.gerMagDefBite   +AddAttrB#add_attr.gerMagDefBite   
			 ,gerMagDef          = AddAttrA#add_attr.gerMagDef       +AddAttrB#add_attr.gerMagDef       
			 ,gerAttackAddtion   = AddAttrA#add_attr.gerAttackAddtion+AddAttrB#add_attr.gerAttackAddtion
			 ,gerHpMaxAddtion    = AddAttrA#add_attr.gerHpMaxAddtion +AddAttrB#add_attr.gerHpMaxAddtion 
			 ,gerAntiWei         = AddAttrA#add_attr.gerAntiWei      +AddAttrB#add_attr.gerAntiWei      
			 ,gerAntiShu         = AddAttrA#add_attr.gerAntiShu      +AddAttrB#add_attr.gerAntiShu      
			 ,gerAntiWu          = AddAttrA#add_attr.gerAntiWu       +AddAttrB#add_attr.gerAntiWu       
			 ,gerAntiQun         = AddAttrA#add_attr.gerAntiQun      +AddAttrB#add_attr.gerAntiQun
			 }.
			  

append_add_attr_no_Anti(0,0) ->
	#add_attr{};
append_add_attr_no_Anti(0, AddAttrB) ->
	AddAttrB;
append_add_attr_no_Anti(AddAttrA,0) ->
	AddAttrA;
%% 原始增加为A， 补充增加为B，即不计算B的克制属性
append_add_attr_no_Anti(AddAttrA, AddAttrB) ->
	#add_attr{
			  gerAttack          = AddAttrA#add_attr.gerAttack       +AddAttrB#add_attr.gerAttack       
			 ,gerHpMax           = AddAttrA#add_attr.gerHpMax        +AddAttrB#add_attr.gerHpMax        
			 ,gerSpInit          = AddAttrA#add_attr.gerSpInit       +AddAttrB#add_attr.gerSpInit       
			 ,gerSpMax          = AddAttrA#add_attr.gerSpMax       +AddAttrB#add_attr.gerSpMax       
			 ,gerCritic          = AddAttrA#add_attr.gerCritic       +AddAttrB#add_attr.gerCritic       
			 ,gerCriticReduce    = AddAttrA#add_attr.gerCriticReduce +AddAttrB#add_attr.gerCriticReduce 
			 ,gerDoom            = AddAttrA#add_attr.gerDoom         +AddAttrB#add_attr.gerDoom         
			 ,gerMiss            = AddAttrA#add_attr.gerMiss         +AddAttrB#add_attr.gerMiss         
			 ,gerAbsorb          = AddAttrA#add_attr.gerAbsorb       +AddAttrB#add_attr.gerAbsorb       
			 ,gerDamageBack      = AddAttrA#add_attr.gerDamageBack   +AddAttrB#add_attr.gerDamageBack   
			 ,gerReel            = AddAttrA#add_attr.gerReel         +AddAttrB#add_attr.gerReel         
			 ,gerReelReduce      = AddAttrA#add_attr.gerReelReduce   +AddAttrB#add_attr.gerReelReduce   
			 ,gerPhyDefBite      = AddAttrA#add_attr.gerPhyDefBite   +AddAttrB#add_attr.gerPhyDefBite   
			 ,gerPhyDef          = AddAttrA#add_attr.gerPhyDef       +AddAttrB#add_attr.gerPhyDef       
			 ,gerMagDefBite      = AddAttrA#add_attr.gerMagDefBite   +AddAttrB#add_attr.gerMagDefBite   
			 ,gerMagDef          = AddAttrA#add_attr.gerMagDef       +AddAttrB#add_attr.gerMagDef       
			 ,gerAttackAddtion   = AddAttrA#add_attr.gerAttackAddtion+AddAttrB#add_attr.gerAttackAddtion
			 ,gerHpMaxAddtion    = AddAttrA#add_attr.gerHpMaxAddtion +AddAttrB#add_attr.gerHpMaxAddtion 
			 ,gerAntiWei         = AddAttrA#add_attr.gerAntiWei   
			 ,gerAntiShu         = AddAttrA#add_attr.gerAntiShu   
			 ,gerAntiWu          = AddAttrA#add_attr.gerAntiWu     
			 ,gerAntiQun         = AddAttrA#add_attr.gerAntiQun
			 }.


append_add_attr_only_Anti(0,0) ->
	#add_attr{};
append_add_attr_only_Anti(0, AddAttrB) ->
	AddAttrB;
append_add_attr_only_Anti(AddAttrA,0) ->
	AddAttrA;
%% 原始增加为A， 补充增加为B，即只计算B的克制属性
append_add_attr_only_Anti(AddAttrA, AddAttrB) ->
	#add_attr{
			  gerAttack          = AddAttrA#add_attr.gerAttack  
			 ,gerHpMax           = AddAttrA#add_attr.gerHpMax  
			 ,gerSpInit          = AddAttrA#add_attr.gerSpInit
			 ,gerSpMax          = AddAttrA#add_attr.gerSpMax 
			 ,gerCritic          = AddAttrA#add_attr.gerCritic 
			 ,gerCriticReduce    = AddAttrA#add_attr.gerCriticReduce
			 ,gerDoom            = AddAttrA#add_attr.gerDoom   
			 ,gerMiss            = AddAttrA#add_attr.gerMiss  
			 ,gerAbsorb          = AddAttrA#add_attr.gerAbsorb  
			 ,gerDamageBack      = AddAttrA#add_attr.gerDamageBack 
			 ,gerReel            = AddAttrA#add_attr.gerReel      
			 ,gerReelReduce      = AddAttrA#add_attr.gerReelReduce
			 ,gerPhyDefBite      = AddAttrA#add_attr.gerPhyDefBite 
			 ,gerPhyDef          = AddAttrA#add_attr.gerPhyDef    
			 ,gerMagDefBite      = AddAttrA#add_attr.gerMagDefBite  
			 ,gerMagDef          = AddAttrA#add_attr.gerMagDef      
			 ,gerAttackAddtion   = AddAttrA#add_attr.gerAttackAddtion
			 ,gerHpMaxAddtion    = AddAttrA#add_attr.gerHpMaxAddtion
			 ,gerAntiWei         = AddAttrA#add_attr.gerAntiWei      +AddAttrB#add_attr.gerAntiWei      
			 ,gerAntiShu         = AddAttrA#add_attr.gerAntiShu      +AddAttrB#add_attr.gerAntiShu      
			 ,gerAntiWu          = AddAttrA#add_attr.gerAntiWu       +AddAttrB#add_attr.gerAntiWu       
			 ,gerAntiQun         = AddAttrA#add_attr.gerAntiQun      +AddAttrB#add_attr.gerAntiQun
			 }.

cacl_fight_power(GerBase,Ger, EnterSkillHpAdd, EnterSkillAtkAdd) ->
	#gerBase{gerTypeID=GerTypeID,gerQuality=GerQuality}=GerBase,
	#gerSkill{unique=UniqueSkills, god=GodSkills, god2=God2Skills,enter=EnterSkills} = ger_lib:get_ger_skills(GerTypeID, GerQuality),
	GerUniqueSkillID = role_fight:random_one_skill(UniqueSkills),
	#data_skill{damageRatio=DamageRatio,targetSelect=TargetSelect
			   ,gerReel=GerReel2,gerCritic=GerCritic2,gerAbsorb=GerAbsorb2,gerDoom=GerDoom2} = data_skill:get(GerUniqueSkillID),
	%% 叠加计算登场技能属性
	GerEnterSkillID = role_fight:random_one_skill(EnterSkills),
	case data_skill:get(GerEnterSkillID) of
		#data_skill{gerEnterOrGodAdd=AddAttr}->
			#ger{gerAttr=GerAttr} = add_enter_skill_attr(Ger, AddAttr);
		_ ->
			#ger{gerAttr=GerAttr} = Ger
	end,
	
	%?ERR("fightPower:~w,\n~w\n~w\n~w\n~w",[role_data:get_roleID(),GerBase, GerAttr, EnterSkillHpAdd, EnterSkillAtkAdd]),
	#gerAttr{gerReel=GerReel,gerCritic=GerCritic,gerAbsorb=GerAbsorb,gerDoom=GerDoom} = GerAttr,
	TargetNum = role_fight:skill_target_num(TargetSelect),
	%% 技能释放频率
	SkillProb = 0.33+0.0017*GerAttr#gerAttr.gerSpInit+0.005*GerAttr#gerAttr.gerSpMax,
	Base = ((1-SkillProb)*(1+GerReel/100)*(1+GerCritic/100)*(1+GerAbsorb/10000)*(1+GerDoom/100) + 
				SkillProb * (DamageRatio/10000) * TargetNum * 
				(1+(GerReel+GerReel2)/100) * (1+(GerCritic+GerCritic2)/100) * (1+(GerAbsorb+GerAbsorb2)/10000) * (1+(GerDoom+GerDoom2)/100)),
	%?ERR("prob=~w\nbase=~w",[SkillProb,Base]),
	FightPower=
		(GerAttr#gerAttr.gerAttack)/26
			*(GerAttr#gerAttr.gerHpMax)/156
			%*(1+GerAttr#gerAttr.gerCritic/100)
			*(1+GerAttr#gerAttr.gerCriticReduce/100)
			%*(1+GerAttr#gerAttr.gerDoom/100)
			*(1+GerAttr#gerAttr.gerMiss/100)
			*(1+GerAttr#gerAttr.gerDamageBack/10000)
			%*(1+GerAttr#gerAttr.gerAbsorb/10000)
			*(1+GerAttr#gerAttr.gerPhyDefBite/100)
			*(1+GerAttr#gerAttr.gerMagDefBite/100)
			*(1+GerAttr#gerAttr.gerPhyDef/100)
			*(1+GerAttr#gerAttr.gerMagDef/100)
			*(1+GerAttr#gerAttr.gerReelReduce/100)
			*(1+GerAttr#gerAttr.gerAntiQun/10000)
			*(1+GerAttr#gerAttr.gerAntiWei/10000)
			*(1+GerAttr#gerAttr.gerAntiWu/10000)
			*(1+GerAttr#gerAttr.gerAntiShu/10000)
			*Base
			*(1+EnterSkillHpAdd/10000)
			*(1+EnterSkillAtkAdd/10000),
	FightPower2 = 
		if GodSkills =:= [] ->
			   FightPower;
		   true ->
			   if God2Skills =:= []->
					  FightPower * (1.25);
				  true ->
					  #data_skill{gerEnterOrGodAdd=God2AddAttr} = data_skill:get(role_fight:random_one_skill(God2Skills)),
					  ReAddHp = God2AddAttr#add_attr.gerHpMaxAddtion / 10000,
					  FightPower * (1.25) * (1+ReAddHp)
			   end
		end,
	erlang:trunc(math:pow(FightPower2,0.3029) * 100).

%% @doc 计算登场技能的属性加成
add_enter_skill_attr(Ger, AddAttr) when is_record(AddAttr, add_attr)->
	#ger{gerAttr=GerAttr,gerHp=GerHp}=Ger,
	#gerAttr{gerAttack=GerAttack,gerHpMax=GerHpMax}=GerAttr,
	GerAttack2 = trunc((GerAttack+AddAttr#add_attr.gerAttack)*(10000+AddAttr#add_attr.gerAttackAddtion)/10000),
	GerHpMax2= trunc((GerHpMax+AddAttr#add_attr.gerHpMax)*(10000+AddAttr#add_attr.gerHpMaxAddtion)/10000),	
	GerAttr2 = GerAttr#gerAttr{
					gerAttack       	= GerAttack2
				   ,gerHpMax        	= GerHpMax2
				   ,gerSpInit       	= GerAttr#gerAttr.gerSpInit       		+  AddAttr#add_attr.gerSpInit       			
				   ,gerSpMax       	= GerAttr#gerAttr.gerSpMax       		+  AddAttr#add_attr.gerSpMax       			
				   ,gerCritic       	= GerAttr#gerAttr.gerCritic       		+  AddAttr#add_attr.gerCritic       			
				   ,gerCriticReduce 	= GerAttr#gerAttr.gerCriticReduce 		+  AddAttr#add_attr.gerCriticReduce 			
				   ,gerDoom         	= GerAttr#gerAttr.gerDoom         		+  AddAttr#add_attr.gerDoom         			
				   ,gerMiss         	= GerAttr#gerAttr.gerMiss         		+  AddAttr#add_attr.gerMiss         			
				   ,gerAbsorb       	= GerAttr#gerAttr.gerAbsorb       		+  AddAttr#add_attr.gerAbsorb       			
				   ,gerDamageBack   	= GerAttr#gerAttr.gerDamageBack   		+  AddAttr#add_attr.gerDamageBack   			
				   ,gerReel         	= GerAttr#gerAttr.gerReel         		+  AddAttr#add_attr.gerReel         			
				   ,gerReelReduce   	= GerAttr#gerAttr.gerReelReduce   		+  AddAttr#add_attr.gerReelReduce   			
				   ,gerPhyDefBite   	= GerAttr#gerAttr.gerPhyDefBite   		+  AddAttr#add_attr.gerPhyDefBite   			
				   ,gerPhyDef       	= GerAttr#gerAttr.gerPhyDef       		+  AddAttr#add_attr.gerPhyDef       			
				   ,gerMagDefBite   	= GerAttr#gerAttr.gerMagDefBite   		+  AddAttr#add_attr.gerMagDefBite   			
				   ,gerMagDef       	= GerAttr#gerAttr.gerMagDef       		+  AddAttr#add_attr.gerMagDef
				   ,gerAntiWei      	= GerAttr#gerAttr.gerAntiWei   +  AddAttr#add_attr.gerAntiWei      			
				   ,gerAntiShu      	= GerAttr#gerAttr.gerAntiShu   +  AddAttr#add_attr.gerAntiShu      			
				   ,gerAntiWu       	= GerAttr#gerAttr.gerAntiWu    +  AddAttr#add_attr.gerAntiWu       			
				   ,gerAntiQun      	= GerAttr#gerAttr.gerAntiQun   +  AddAttr#add_attr.gerAntiQun       	
								   },
	%% 只有在参战时，血量是满的情况，才将当前血量随之改变
	%% 如果参战时，血量不满，则不涨血量
	GerHp2 = 
		if GerHp =/= GerHpMax ->
			   erlang:min(GerHp, GerHpMax2);
		   true ->
			   GerHpMax2
		end,
	%?DEBUG("sp=~w,,newSp=~w",[Ger#ger.gerSp,GerAttr2#gerAttr.gerSpInit]),
	Ger#ger{gerAttr=GerAttr2,
			gerHp=GerHp2,
			gerSp=GerAttr2#gerAttr.gerSpInit
		   };
add_enter_skill_attr(Ger, _) ->
	Ger.
			
	
%% @doc 计算登场技能的属性加成
add_buff_attr(Ger, AddAttr) when is_record(AddAttr, add_attr)->
	#ger{gerAttr=GerAttr,gerHp=GerHp}=Ger,
	#gerAttr{gerAttack=GerAttack,gerHpMax=GerHpMax}=GerAttr,
	GerAttack2 = trunc((GerAttack+AddAttr#add_attr.gerAttack)*(10000+AddAttr#add_attr.gerAttackAddtion)/10000),
	GerHpMax2= trunc((GerHpMax+AddAttr#add_attr.gerHpMax)*(10000+AddAttr#add_attr.gerHpMaxAddtion)/10000),	
	GerAttr2 = GerAttr#gerAttr{
					gerAttack       	= GerAttack2
				   ,gerHpMax        	= GerHpMax2
				   ,gerSpInit       	= GerAttr#gerAttr.gerSpInit       		+  AddAttr#add_attr.gerSpInit       			
				   ,gerSpMax       	= GerAttr#gerAttr.gerSpMax       		+  AddAttr#add_attr.gerSpMax       			
				   ,gerCritic       	= GerAttr#gerAttr.gerCritic       		+  AddAttr#add_attr.gerCritic       			
				   ,gerCriticReduce 	= GerAttr#gerAttr.gerCriticReduce 		+  AddAttr#add_attr.gerCriticReduce 			
				   ,gerDoom         	= GerAttr#gerAttr.gerDoom         		+  AddAttr#add_attr.gerDoom         			
				   ,gerMiss         	= GerAttr#gerAttr.gerMiss         		+  AddAttr#add_attr.gerMiss         			
				   ,gerAbsorb       	= GerAttr#gerAttr.gerAbsorb       		+  AddAttr#add_attr.gerAbsorb       			
				   ,gerDamageBack   	= GerAttr#gerAttr.gerDamageBack   		+  AddAttr#add_attr.gerDamageBack   			
				   ,gerReel         	= GerAttr#gerAttr.gerReel         		+  AddAttr#add_attr.gerReel         			
				   ,gerReelReduce   	= GerAttr#gerAttr.gerReelReduce   		+  AddAttr#add_attr.gerReelReduce   			
				   ,gerPhyDefBite   	= GerAttr#gerAttr.gerPhyDefBite   		+  AddAttr#add_attr.gerPhyDefBite   			
				   ,gerPhyDef       	= GerAttr#gerAttr.gerPhyDef       		+  AddAttr#add_attr.gerPhyDef       			
				   ,gerMagDefBite   	= GerAttr#gerAttr.gerMagDefBite   		+  AddAttr#add_attr.gerMagDefBite   			
				   ,gerMagDef       	= GerAttr#gerAttr.gerMagDef       		+  AddAttr#add_attr.gerMagDef
				   ,gerAntiWei      	= GerAttr#gerAttr.gerAntiWei   +  AddAttr#add_attr.gerAntiWei      			
				   ,gerAntiShu      	= GerAttr#gerAttr.gerAntiShu   +  AddAttr#add_attr.gerAntiShu      			
				   ,gerAntiWu       	= GerAttr#gerAttr.gerAntiWu    +  AddAttr#add_attr.gerAntiWu       			
				   ,gerAntiQun      	= GerAttr#gerAttr.gerAntiQun   +  AddAttr#add_attr.gerAntiQun       	
								   },
	%% 只有在参战时，血量是满的情况，才将当前血量随之改变
	%% 如果参战时，血量不满，则不涨血量
	GerHp2 = 
		if GerHp =/= GerHpMax ->
			   erlang:min(GerHp, GerHpMax2);
		   true ->
			   GerHpMax2
		end,
	?DEBUG("sp=~w,,newSp=~w",[Ger#ger.gerSp,GerAttr2#gerAttr.gerSpInit]),
	Ger#ger{gerAttr=GerAttr2,
			gerHp=GerHp2,
			gerSp=GerAttr2#gerAttr.gerSpInit,
			gerExtra=0
		   
		   }.
%% ====================================================================
%% Internal functions
%% ====================================================================

test_all_ger() ->
	[new_ger(E,1,0,[], [])||E<-data_ger:get_list(),E < 20000].
	
test_ger_list() ->
	random:seed(erlang:now()),
	RandomPool = [E||E<-data_ger:get_list(),E < 20000],
	IDList = util:random_list2(RandomPool, 6),
	L = lists:zip(lists:seq(1,length(IDList)), IDList),
	[?change_pos(Ger, Pos) || {Pos,ID} <- L, Ger<-[new_ger(ID,1,0,[], IDList)]].
