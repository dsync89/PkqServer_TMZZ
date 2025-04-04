%% @author admin
%% @doc 武将相关操作协议
%% Created 2013-3-8


-module(role_ger).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_item.hrl").
-compile(export_all).
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_ger_info(_) ->
	PGerBag = [ger_lib:ger2p_ger(E)||E<-role_data:get_gerBag()],
	PPosList = [ger_lib:ger2p_ger(E)||E<-role_data:get_posListT()],
	PGuardPosList = [ger_lib:ger2p_ger(E)||E<-role_data:get_guardPosList()],
	case erlang:length(PGerBag) > 1000 of
		false ->
            ?sendself(#sc_ger_info{gerList=PGuardPosList++PPosList++PGerBag});
		true ->
            {PGerBag1, PGerBagRest} = lists:split(1000, PGerBag),
            ?sendself(#sc_ger_info{gerList=PGuardPosList++PPosList++PGerBag1}),
            send_rest_ger(PGerBagRest)
	end.

send_rest_ger(PGerBag) ->
    case erlang:length(PGerBag) > 1000 of
        true ->
            {PGerBag1, PGerBagRest} = lists:split(1000, PGerBag),
            ?sendself(#sc_ger_more{list=PGerBag1}),
            send_rest_ger(PGerBagRest);
        false ->
            ?sendself(#sc_ger_more{list=PGerBag}),
            ?sendself(#sc_ger_more{list=[]})
    end.

cs_ger_pos_list(_) ->
	PosList = [begin
				   EquipList=[Equip#item.itemUID || Equip <-role_data:get_equip(GerID)],				   
				   #p_ger_pos_info{gerID=GerID,gerPos=Pos,itemUIDList=EquipList}
			   end ||#ger{gerBase=#gerBase{gerPos=Pos},gerID=GerID}<-role_data:get_posList()],
	Reply = #sc_ger_pos_list{gerPosInfoList=PosList},
	?sendself(Reply).

guard_ger_move(SrcPos, DestPos) ->
    #role_guard{count=Count,infoList=RoleGuardInfo} = role_data:get_roleGuardInfo(),
    GuardGerList = role_data:get_guardPosList(),
    NewRoleGuardInfo = util:list_key_replace(SrcPos, DestPos, #guard_info.pos, RoleGuardInfo),
    NewGuardGerList = util:list_key_replace(SrcPos, DestPos, #gerSimple.gerPos, GuardGerList),
    role_data:set_roleGuardInfo(#role_guard{count=Count,infoList=NewRoleGuardInfo}),
    role_data:set_guardPosList(NewGuardGerList),
    ?sendself(#sc_ger_guard_info{list=[ger_lib:to_p_ger_guard(GuardGer, NewRoleGuardInfo)||GuardGer<-NewGuardGerList]}).

cs_ger_guard_info(_) ->
    #role_guard{infoList=RoleGuardInfo} = role_data:get_roleGuardInfo(),
    GuardGerList = role_data:get_guardPosList(),
    ?sendself(#sc_ger_guard_info{list=[ger_lib:to_p_ger_guard(GuardGer, RoleGuardInfo)||GuardGer<-GuardGerList]}).

cs_ger_guard_set(#cs_ger_guard_set{gerID=GerID, gerPos=GerPos}) ->
    case check_ger_guard_set(GerID, GerPos) of
        {true, NewBaseList, NewGuardPosList, NewGerBag, PosGerID} ->
            role_data:set_guardPosList(NewGuardPosList),
            role_data:set_gerBag(NewGerBag),
            ger_attr:recacl_f(PosGerID),
            ?sendself(#sc_ger_guard_set{result=0, gerID=GerID, gerPos=GerPos, baseList=NewBaseList});
        {false, Reason} ->
            ?sendself(#sc_ger_guard_set{result=Reason, gerID=GerID, gerPos=GerPos, baseList=[]})
    end.

check_ger_guard_set(GerID, GerPos) ->
    GerBag = role_data:get_gerBag(),
    case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
        false ->
            {false, 1};
        {value, #gerSimple{gerTypeID=GerTypeID, gerQuality=GerQuality}=UpGer, GerBag2} ->
            case util:is_exp_card(GerTypeID) of
                false ->
                    case (is_integer(GerPos) andalso GerPos >0 andalso GerPos =< 6) of
                        true ->
                            PosList = role_data:get_posList(),
                            case lists:any(fun(E) ->
                                                   #ger{gerBase=EBase} = E,
                                                   EBase#gerBase.gerTypeID =:= GerTypeID
                                           end, PosList) of
                                true ->
                                    {false, 4};
                                false ->
                                    GuardPosList = role_data:get_guardPosList(),
                                    GuardPosList2 = lists:keydelete(GerPos, #gerSimple.gerPos, GuardPosList),
                                    case lists:any(fun(E) ->
                                                           E#gerSimple.gerTypeID =:= GerTypeID
                                                   end, GuardPosList2) of
                                        true ->
                                            {false, 5};
                                        false ->
                                            NewBaseList = ger_lib:calc_base_list(GerTypeID, GerQuality),
                                            case GuardPosList =:= GuardPosList2 of
                                                true ->
                                                    NewGuardPosList = [UpGer#gerSimple{gerPos=GerPos}|GuardPosList],
                                                    NewGerBag = GerBag2;
                                                false ->
                                                    NewGuardPosList = [UpGer#gerSimple{gerPos=GerPos}|GuardPosList2],
                                                    {value, DownGer, _} = lists:keytake(GerPos, #gerSimple.gerPos, GuardPosList),
                                                    NewGerBag = [DownGer#gerSimple{gerPos=0}|GerBag2]
                                            end,
                                            case util:fun_find(fun(E) ->
                                                                       #ger{gerBase=EBase} = E,
                                                                       EBase#gerBase.gerPos =:= GerPos
                                                               end, PosList) of
                                                false ->
                                                    {false, 6};
                                                #ger{gerID=PosGerID} ->
                                                    {true, NewBaseList, NewGuardPosList, NewGerBag, PosGerID}
                                            end
                                    end
                            end;
                        false ->
                            {false, 3}
                    end;
                true ->
                    {false, 2}
            end
    end.

cs_ger_guard_refresh(#cs_ger_guard_refresh{gerPos=GerPos,lockList=LockList}) ->
    case catch check_ger_guard_refresh(GerPos, LockList) of
        {true, RoleInfo, Count, NewRoleGuardInfo, NewHighList, NeedGold, PosGerID} ->
            do_ger_guard_refresh(RoleInfo, Count, NewRoleGuardInfo, NewHighList, NeedGold, PosGerID);
        {false, Reason} ->
            ?sendself(#sc_ger_guard_refresh{result=Reason,highList=[]})
    end.

check_ger_guard_refresh(GerPos, LockList) ->
    GuardPosList = role_data:get_guardPosList(),
    #role_guard{count=Count, infoList=RoleGuardInfo} = role_data:get_roleGuardInfo(),
    case lists:keyfind(GerPos, #gerSimple.gerPos, GuardPosList) of
        false ->
            erlang:throw({false, 1});
        _ ->
            next
    end,
    case lists:keyfind(GerPos, #guard_info.pos, RoleGuardInfo) of
        false ->
            IsHaveHighAttr = false,
            HighList = [];
        #guard_info{list=HighList} ->
            IsHaveHighAttr = true,
            next
    end,
    case erlang:length(LockList) =:= 6 of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    LockedNum = erlang:length([Lock||Lock<-LockList, Lock =:= true]),
    case LockedNum > 0 andalso HighList =< 1 of
        true ->
            erlang:throw({false, 3});
        false ->
            next
    end,
    case LockedNum > HighList of
        true ->
            erlang:throw({false, 4});
        false ->
            next
    end,
    case LockedNum =:= 6 of
        true ->
            erlang:throw({false, 5});
        false ->
            next
    end,
    NeedGold = data_guard:get({lock, LockedNum}),
    RoleInfo = role_data:get_roleInfo(),
    case role_lib:check_money(RoleInfo, gold, NeedGold) of
        true ->
            next;
        false ->
            erlang:throw({false, 6})
    end,
    {ok, NewHighList} = calc_high_attr_list(HighList, LockList, Count + 1),
    case IsHaveHighAttr of
        false ->
            NewRoleGuardInfo = [#guard_info{pos=GerPos,list=NewHighList}|RoleGuardInfo];
        true ->
            NewRoleGuardInfo = lists:keyreplace(GerPos, #guard_info.pos, RoleGuardInfo, #guard_info{pos=GerPos,list=NewHighList})
    end,
    case util:fun_find(fun(E) ->
                               #ger{gerBase=EBase} = E,
                               EBase#gerBase.gerPos =:= GerPos
                       end, role_data:get_posList()) of
        false ->
            PosGerID = 0;
        #ger{gerID=PosGerID} ->
            next
    end,
    {true, RoleInfo, Count, NewRoleGuardInfo, NewHighList, NeedGold, PosGerID}.

calc_high_attr_list(HighList, LockList, RefreshCount) ->
    AttrNum = erlang:length(HighList),
    case AttrNum =:= 6 of
        true ->
            NewAttrNum = AttrNum;
        false ->
            case lists:member(RefreshCount, data_guard:get(rule_times_to_next_attr)) of
                true ->
                    NewAttrNum = AttrNum + 1;
                false ->
                    case random:uniform(10000) < lists:nth(AttrNum + 1, data_guard:get(add_attr_prob)) of
                        true ->
                            NewAttrNum = AttrNum + 1;
                        false ->
                            NewAttrNum = AttrNum
                    end
            end
    end,
    {HaveTypeList, _} =
        lists:foldl(fun(#p_ger_guard_attr{attrType=AttrType}, {AccHaveTypeList, Count}) ->
                            case is_lock(LockList, Count) of
                                true ->
                                    {[AttrType|AccHaveTypeList], Count + 1};
                                false ->
                                    {AccHaveTypeList, Count + 1}
                             end
                    end, {[], 1}, HighList),
    calc_high_attr_list2(HighList, LockList, [], NewAttrNum, 1, HaveTypeList).

calc_high_attr_list2(HighList, _LockList, _AccNewHighList, 0, _Count, _HaveTypeList) ->
    {ok, HighList};
calc_high_attr_list2(HighList, LockList, AccNewHighList, NewAttrNum, Count, HaveTypeList) ->
    case is_lock(LockList, Count) of
        true ->
            NewHighAttr = lists:nth(Count, HighList),
            NewHaveTypeList = HaveTypeList;
        false ->
            #p_ger_guard_attr{attrType=AttrType} = NewHighAttr = gen_new_high_attr(HaveTypeList),
            NewHaveTypeList = [AttrType|HaveTypeList]
    end,
    case NewAttrNum =:= Count of
        false ->
            calc_high_attr_list2(HighList, LockList, AccNewHighList ++ [NewHighAttr], NewAttrNum, Count + 1, NewHaveTypeList);
        true ->
            {ok, AccNewHighList ++ [NewHighAttr]}
    end.

gen_new_high_attr(HaveTypeList) ->
    TypeList = data_guard:get(attr_type_list) -- HaveTypeList,
    AttrType = util:random_one_from_list(TypeList),
    {Min, Max} = util:random_one_from_weigh_list(data_guard:get(AttrType)),
    Value = random:uniform(Max - Min + 1) + Min - 1,
    #p_ger_guard_attr{attrType=AttrType, addValue=Value}.

is_lock(LockList, Count) ->
    lists:nth(Count, LockList) =:= true.

do_ger_guard_refresh(RoleInfo, Count, NewRoleGuardInfo, NewHighList, NeedGold, PosGerID) ->
    case NeedGold > 0 of
        true ->
            role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_GUARD_REFRESH, 0, "");
        false ->
            next
    end,
    role_data:set_roleGuardInfo(#role_guard{count=Count+1,infoList=NewRoleGuardInfo}),
    case PosGerID > 0 of
        true ->
            ger_attr:recacl_f(PosGerID);
        false ->
            next
    end,
    ?sendself(#sc_ger_guard_refresh{result=0,highList=NewHighList}).
        
%% 出战一个武将
cs_ger_standup(#cs_ger_standup{gerPos=GerPos, gerID=GerID})->
    case check_standup(GerPos, GerID) of
        {true, UpGer, PosList, GerBag2} ->
            do_standup(GerPos, GerID, UpGer, PosList, GerBag2);
        {false,Reason} ->
            ?sendself(#sc_ger_standup{gerPos=GerPos,gerID=GerID, result=Reason})
    end.


check_down_new(GerID) ->
	?ERR("check_down_new  :GerID=~w",[GerID]),
    PosList = ger_attr:get_original_posList(),
    case erlang:length(PosList) >= 1 of
        true ->
			?ERR("check_down_new  : 11111111111"),
            case lists:keytake(GerID, #ger.gerID, PosList) of
                false ->
					?ERR("check_down_new  : 22222222"),
                    {false, 1};
                {value, DownGer, PosList2} ->
					?ERR("check_down_new  : 3333333333"),
                    case role_data:get_equip(GerID) of
                        [] ->
							?ERR("check_down_new  : 444444444444"),
                            {true, DownGer, PosList2};
                        _ ->
							?ERR("check_down_new  : 555555555555"),
                            %%{false, 2}
							{true, DownGer, PosList2}
                    end
            end;
        false ->
			?ERR("check_down_new  : 6666666666666666"),
            {false, 3}
    end.

do_down_new(#ger{gerID=GerID,gerBase=#gerBase{gerTypeID=GerTypeID}}=DownGer, PosList2) ->
	?ERR("do_down_new  : 111111111111"),
    GerBag = role_data:get_gerBag(),
	?ERR("do_down_new  : 22222222222222"),
    GerBag2 = [ger_lib:ger2gerSimple(DownGer)|GerBag],
	?ERR("do_down_new  : 3333333333333"),
    role_data:set_gerBag(GerBag2),
	?ERR("do_down_new  : 44444444444"),
    PosList3 = [destiny_impact(E,GerTypeID, GerTypeID, PosList2)||E<-PosList2],
	?ERR("do_down_new  : 55555555555555"),
    PosList4 = ger_attr:refresh_fightPower(PosList3),
	?ERR("do_down_new  : 6666666666666"),
    role_data:set_posList(PosList4),
	?ERR("do_down_new  : 7777777777777").

%%%================================保存武将阵型=========================================================
cs_ger_down(#cs_ger_down{gerID1=GerID1,gerID2=GerID2,gerID3=GerID3,gerID4=GerID4,gerID5=GerID5,gerID6=GerID6,gerEquip1=GerEquip1,gerEquip2=GerEquip2,gerEquip3=GerEquip3,gerEquip4=GerEquip4,gerEquip5=GerEquip5,gerEquip6=GerEquip6}) ->
	?ERR("cs_ger_down  :gerID1=~w,gerID1=~w,gerID1=~w,gerID1=~w,gerID1=~w,gerID1=~w, ",[GerID1,GerID2,GerID3,GerID4,GerID5,GerID6]),
	?ERR("cs_ger_down  :gerEquip1=~w,gerEquip2=~w,gerEquip3=~w,gerEquip4=~w,gerEquip5=~w,gerEquip6=~w, ",[GerEquip1,GerEquip2,GerEquip3,GerEquip4,GerEquip5,GerEquip6]),
	
	PosList2 = role_data:get_posList(),
	?ERR("cs_ger_down  :PosList2=~w",[PosList2]),
	
	lists:foreach(fun(E) ->
 			GerID = E#ger.gerID,
			GerPos = E#ger.gerBase#gerBase.gerPos,
			?ERR("cs_ger_down  :id=~w",[GerID]),
			?ERR("cs_ger_down  :pos=~w",[GerPos]),
			%%if GerID > 0 ->
				case check_down_new(GerID) of
				   {true, DownGer, PosListt2} ->
					   do_down_new(DownGer, PosListt2);
				   {false, Reason} ->
					   ?sendself(#sc_ger_down{result=Reason, gerID=0})
				end,
			%%end,
 			?ERR("cs_ger_down  :id=~w",[GerID])
 	end, PosList2),
	
	if GerID1 > 0 ->
			?ERR("cs_ger_down1 :GerID1=~w",[GerID1]),
			case check_standup(1, GerID1) of
				{true, UpGer1, PosList11, GerBag11} ->
					do_standup(1, GerID1, UpGer1, PosList11, GerBag11);
				{false,Reason1} ->
					?ERR("standup  error  :pos=1")
			end;
		true ->
			?ERR("standup  error  :GerID1=0")
	end,
	
	if GerID2 > 0 ->
			?ERR("cs_ger_down2 :GerID1=~w",[GerID2]),
			case check_standup(2, GerID2) of
				{true, UpGer2, PosList22, GerBag22} ->
					do_standup(2, GerID2, UpGer2, PosList22, GerBag22);
				{false,Reason2} ->
					?ERR("standup  error  :pos=2")
			end;
		true ->
			?ERR("standup  error  :GerID2=0")
	end,
	
	if GerID3 > 0 ->
			?ERR("cs_ger_down3 :GerID3=~w",[GerID3]),
			case check_standup(3, GerID3) of
				{true, UpGer3, PosList33, GerBag33} ->
					do_standup(3, GerID3, UpGer3, PosList33, GerBag33);
				{false,Reason3} ->
					?ERR("standup  error  :pos=3")
			end;
		true ->
			?ERR("standup  error  :GerID3=0")
	end,
	
	if GerID4 > 0 ->
			?ERR("cs_ger_down4 :GerID4=~w",[GerID4]),
			case check_standup(4, GerID4) of
				{true, UpGer4, PosList44, GerBag44} ->
					do_standup(4, GerID4, UpGer4, PosList44, GerBag44);
				{false,Reason4} ->
					?ERR("standup  error  :pos=4")
			end;
		true ->
			?ERR("standup  error  :GerID4=0")
	end,
	
	if GerID5 > 0 ->
			?ERR("cs_ger_down5 :GerID5=~w",[GerID5]),
			case check_standup(5, GerID5) of
				{true, UpGer5, PosList55, GerBag55} ->
					do_standup(5, GerID5, UpGer5, PosList55, GerBag55);
				{false,Reason5} ->
					?ERR("standup  error  :pos=5")
			end;
		true ->
			?ERR("standup  error  :GerID5=0")
	end,
	if GerID6 > 0 ->
			?ERR("cs_ger_down6 :GerID6=~w",[GerID6]),
			case check_standup(6, GerID6) of
				{true, UpGer6, PosList66, GerBag66} ->
					do_standup(6, GerID6, UpGer6, PosList66, GerBag66);
				{false,Reason6} ->
					?ERR("standup  error  :pos=6")
			end;
		true ->
			?ERR("standup  error  :GerID6=0")
	end,
	
		%%换装备
	if GerID1 /= GerEquip1 andalso  GerEquip1 /= 18446744073709551615  ->
			?ERR("GerEquip1  :GerEquip1=~w",[GerEquip1]),
			role_data:swap_equip(GerID1, GerEquip1);
		true ->
			?ERR("standup  error  :GerEquip1=0")
	end,

	
	if GerID2/=GerEquip1 andalso GerEquip2/=GerID1 andalso GerID2 /= GerEquip2 andalso  GerEquip2 /= 18446744073709551615  ->
			?ERR("GerEquip2  :GerEquip2=~w",[GerEquip2]),
			role_data:swap_equip(GerID2, GerEquip2);
		true ->
			?ERR("standup  error  :GerEquip2=0")
	end,


	if GerID3/=GerEquip1 andalso GerEquip3/=GerID1 andalso GerID3/=GerEquip2 andalso GerEquip3/=GerID2 andalso GerID3 /= GerEquip3 andalso  GerEquip3 /= 18446744073709551615  ->
			?ERR("GerEquip3  :GerEquip3=~w",[GerEquip3]),
			role_data:swap_equip(GerID3, GerEquip3);
		true ->
			?ERR("standup  error  :GerEquip3=0")
	end,


	if GerID4/=GerEquip1 andalso GerEquip4/=GerID1 andalso GerID4/=GerEquip2 andalso GerEquip4/=GerID2 andalso GerID4/=GerEquip3 andalso GerEquip4/=GerID3 andalso GerID4 /= GerEquip4 andalso  GerEquip4 /= 18446744073709551615  ->
			?ERR("GerEquip4  :GerEquip4=~w",[GerEquip4]),
			role_data:swap_equip(GerID4, GerEquip4);
		true ->
			?ERR("standup  error  :GerEquip4=0")
	end,

	if GerID5/=GerEquip1 andalso GerEquip5/=GerID1 andalso GerID5/=GerEquip2 andalso GerEquip5/=GerID2 andalso GerID5/=GerEquip3 andalso GerEquip5/=GerID3 andalso GerID5/=GerEquip4 andalso GerEquip5/=GerID4 andalso GerID5 /= GerEquip5 andalso  GerEquip5 /= 18446744073709551615  ->
			?ERR("GerEquip5  :GerEquip5=~w",[GerEquip5]),
			role_data:swap_equip(GerID5, GerEquip5);
		true ->
			?ERR("standup  error  :GerEquip5=0")
	end,


	if GerID6/=GerEquip1 andalso GerEquip6/=GerID1 andalso GerID6/=GerEquip2 andalso GerEquip6/=GerID2 andalso GerID6/=GerEquip3 andalso GerEquip6/=GerID3 andalso GerID6/=GerEquip4 andalso GerEquip6/=GerID4 andalso GerID6/=GerEquip5 andalso GerEquip6/=GerID5 andalso GerID6 /= GerEquip6 andalso  GerEquip6 /= 18446744073709551615  ->
			?ERR("GerEquip6  :GerEquip6=~w",[GerEquip6]),
			role_data:swap_equip(GerID6, GerEquip6);
		true ->
			?ERR("standup  error  :GerEquip6=0")
	end,
	
	%%%
	%%%if GerID1 > 0 ->
	%%%	case lists:keytake(GerID, #ger.gerID, PosList) of
    %%%            false ->
    %%%                {false, 1};
    %%%            {value, DownGer, PosList2} ->
    %%%                case role_data:get_equip(GerID) of
    %%%                    [] ->
    %%%                        {true, DownGer, PosList2};
    %%%                    _ ->
    %%%                        {false, 2}
    %%%                end
    %%%    end;
	%%%end,
	
	
    %%%case erlang:length(PosList) >= 2 of
    %%%    true ->
    %%%        case lists:keytake(GerID, #ger.gerID, PosList) of
    %%%            false ->
    %%%                {false, 1};
    %%%            {value, DownGer, PosList2} ->
    %%%                case role_data:get_equip(GerID) of
    %%%                    [] ->
    %%%                        {true, DownGer, PosList2};
    %%%                    _ ->
    %%%                        {false, 2}
    %%%                end
    %%%        end;
    %%%    false ->
    %%%        {false, 3}
    %%%end;
	
	
	PosList1 = [begin
				   EquipList=[Equip#item.itemUID || Equip <-role_data:get_equip(GerID)],				   
				   #p_ger_pos_info{gerID=GerID,gerPos=Pos,itemUIDList=EquipList}
			   end ||#ger{gerBase=#gerBase{gerPos=Pos},gerID=GerID}<-role_data:get_posList()],
	?ERR("cs_ger_down  :PosList=~w",[PosList1]),
	Reply = #sc_ger_pos_list{gerPosInfoList=PosList1},
	?ERR("cs_ger_down  :Reply=~w",[Reply]),
	?sendself(Reply).
	
    %%case check_down(GerID) of
    %%   {true, DownGer, PosList2} ->
    %%       do_down(DownGer, PosList2);
    %%   {false, Reason} ->
    %%       ?sendself(#sc_ger_down{result=Reason, gerID=0})
    %%end.

%% 阵容对比
cs_ger_view_other(#cs_ger_view_other{tarRoleID=TarRoleID}) ->
    RoleID = role_data:get_roleID(),
    ServerID = util:role_id_to_server_id(RoleID),
    TarServerID = util:role_id_to_server_id(TarRoleID),
    case TarServerID =:= ServerID of
        true ->
            case erlang:whereis(role_lib:regName(TarRoleID)) of
                Pid when erlang:is_pid(Pid) ->
                    role_lib:send_server(TarRoleID, {do_ger_view, RoleID, ?undefined});
                ?undefined ->
                    do_offline_ger_view(0, ?undefined, TarRoleID)
            end;
        false ->
            role_rule:ger_view_other(RoleID, TarRoleID, ServerID, TarServerID)
    end.

cs_ger_view_other_dtl(#cs_ger_view_other_dtl{tarRoleID=TarRoleID}) ->
    RoleID = role_data:get_roleID(),
    ServerID = util:role_id_to_server_id(RoleID),
    TarServerID = util:role_id_to_server_id(TarRoleID),
    case TarServerID =:= ServerID of
        true ->
            case erlang:whereis(role_lib:regName(TarRoleID)) of
                Pid when erlang:is_pid(Pid) ->
                    role_lib:send_server(TarRoleID, {do_ger_view_dtl, RoleID, ?undefined});
                ?undefined ->
                    do_offline_ger_view_dtl(0, ?undefined, TarRoleID)
            end;
        false ->
            role_rule:ger_view_other_dtl(RoleID, TarRoleID, ServerID, TarServerID)
    end.

%% 吞噬武将,获得经验
cs_ger_eat(#cs_ger_eat{gerID=GerID,foodIDList=FoodIDList}) ->
	case catch check_eat(GerID, FoodIDList) of
		{true, NewBagItemList, AllAddExp, DelItemList, PosList2, LPosList2, GerBag2, GuardGerList2, SrcGer,Type} ->
			do_eat(GerID, SrcGer, NewBagItemList, AllAddExp, DelItemList, PosList2, LPosList2, GerBag2, GuardGerList2, Type);
		{false, Reason} ->
			?sendself(#sc_ger_eat{gerID=GerID,result=Reason})
    end.

cs_ger_down_rank(#cs_ger_down_rank{srcGerID=SrcGerID}) ->
    case check_down_rank(SrcGerID) of
        {true, SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,GuardGerList2,Type,NeedGold} ->
            do_down_ger(SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,GuardGerList2,Type,NeedGold);
        {false, Reason} ->
            ?sendself(#sc_ger_down_rank{result=Reason,add_ger_list=[]})
    end.

check_down_rank(SrcGerID) ->
    case role_data:get_ger2(SrcGerID) of     
        {value, SrcGer, PosList2, LPosList2, GerBag2, GuardGerList2, Type} ->
            if is_record(SrcGer, gerSimple) ->
                   #gerSimple{gerQuality=GerQuality,gerTypeID=GerTypeID} = SrcGer;
               true ->
                   #ger{gerBase=#gerBase{gerQuality=GerQuality,gerTypeID=GerTypeID}} = SrcGer
            end,
            case util:is_exp_card(GerTypeID) of
                false ->
                    case GerQuality > 0 of 
                        true ->
                            Role = role_data:get_roleInfo(),
                            NeedGold = data_common:get(down_ger_need_gold),
                            case role_lib:check_money(Role, gold, NeedGold) of
                                false ->
                                    {false,4}; 
                                true ->
                                    NewQuality = GerQuality - 1,
                                    {true, SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,GuardGerList2,Type,NeedGold}
                            end;
                        false ->
                            {false, 3}
                    end;
                true ->
                    {false, 2}
            end;
        false ->
            {false,1}
    end.

do_down_ger(SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,GuardGerList2,Type,NeedGold) ->
    #role{roleID=RoleID} = Role,
    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_GER_DOWN_RANK, 0, ""),
    if is_record(SrcGer, gerSimple) ->
           #gerSimple{gerQuality=OldQuality,gerLevel=SrcGerLevel,gerTypeID=GerTypeID} = SrcGer,
           SrcGer2 = SrcGer#gerSimple{gerQuality=NewQuality},
           SrcGer3 = SrcGer2,
           if Type =:= bag ->
                  PosList3 = PosList2,
                  LPosList3 = LPosList2,
                  GerBag3 = [SrcGer3|GerBag2],
                  GuardGerList3 = GuardGerList2;
              true ->
                  PosList3 = PosList2,
                  LPosList3 = LPosList2,
                  GerBag3 = GerBag2,
                  GuardGerList3 = [SrcGer3|GuardGerList2]
           end;
       true->
           #ger{gerBase=#gerBase{gerQuality=OldQuality,gerLevel=SrcGerLevel,gerTypeID=GerTypeID}=GerBase} = SrcGer,
           GerBase2 = GerBase#gerBase{gerQuality= NewQuality},
           SrcGer2 = SrcGer#ger{gerBase=GerBase2},
           if Type =:= ger ->
                  SrcGer3 = ger_attr:recacl(SrcGer2, PosList2),
                  PosList3 = [SrcGer3|PosList2],
                  LPosList3 = LPosList2,
                  GerBag3 = GerBag2,
                  GuardGerList3 = GuardGerList2;
              true ->
                  SrcGer3 = ger_attr:recacl_lieu(SrcGer2, LPosList2),
                  PosList3 = PosList2,
                  LPosList3 = [SrcGer3|LPosList2],
                  GerBag3 = GerBag2,
                  GuardGerList3 = GuardGerList2
           end
    end,
    role_gather:hook_add_ger_list([{GerTypeID,NewQuality}]),
    
    behavior_ger_downrank:log(RoleID, SrcGerID, GerTypeID, SrcGerLevel, OldQuality, NewQuality, erlang:localtime()),
    role_data:set_guardPosList(GuardGerList3),
    role_data:set_gerBag(GerBag3),
    ger_lib:notify_update(SrcGer3),
    %% 提醒客户端更新武将
    role_data:set_lieuposList(LPosList3),
    case Type of
        guard ->
            PosList4 = ger_attr:refresh_fightPower([ger_attr:recacl(E, lists:delete(E, PosList3))||E<-PosList3]);
        _ ->
            PosList4 = ger_attr:refresh_fightPower(PosList3)
    end,
    role_data:set_posList(PosList4),
    role_reward:handle_ger_f([#new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}],  ?MONEY_ADD_TYPE_GER_DOWN_RANK, 0, ""),
    case Type of
        guard ->
            #role_guard{infoList=RoleGuardInfo} = role_data:get_roleGuardInfo(),
            ?sendself(#sc_ger_guard_info{list=[ger_lib:to_p_ger_guard(GuardGer, RoleGuardInfo)||GuardGer<-GuardGerList3]});
        _ ->
            next
    end,
    ?sendself(#sc_ger_down_rank{result=0, add_ger_list=[#p_ger_view{gerQuality=0,gerLevel=1,gerTypeID=GerTypeID}]}).

%% 升品，
cs_ger_up_rank(#cs_ger_up_rank{foodGerID=FoodGerID,srcGerID=SrcGerID}) ->
    case check_up_rank(FoodGerID, SrcGerID) of
        {true, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3,LPosList3, GerBag3,GuardGerList3,Type,SrcGerLevel,SrcGerExp,FoodGerExp} ->
            do_up_rank(SrcGerID, FoodGerID, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3,LPosList3,  GerBag3,GuardGerList3,Type,SrcGerLevel,SrcGerExp,FoodGerExp);
        {true, SrcGerID, Role, SrcGer, NewQuality,PosList3,LPosList3,GerBag3,GuardGerList3,Type, BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu}->
            do_up_rank2(SrcGerID, Role, SrcGer, NewQuality, PosList3,LPosList3, GerBag3,GuardGerList3,Type, BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu);
        {false, Reason} ->
            ?sendself(#sc_ger_up_rank{foodGerID=FoodGerID,srcGerID=SrcGerID,result=Reason})
    end.

do_up_rank(SrcGerID, FoodGerID, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3,LPosList3, GerBag3,GuardGerList3,Type,SrcGerLevel,SrcGerExp,FoodExp) ->
	%?ERR("ger in up:~w,\n~w\n~w\n~w",[SrcGer,FoodGer, GerBag3,Type]),
	#role{roleID=RoleID} = Role,
	role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
	{_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level),
    if is_record(SrcGer, gerSimple) -> %% Type = bag
           #gerSimple{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID} = SrcGer,
           SrcGer2 = SrcGer#gerSimple{gerLevel=Level2,gerExp=Exp2,gerQuality=NewQuality},
           SrcGer3 = SrcGer2,
           if
               Type =:= bag ->
                   GerBag4 = [SrcGer3|GerBag3],
                   LPosList4 = LPosList3,
                   PosList4 = PosList3,
                   GuardGerList4 = GuardGerList3;
               true ->
                   GerBag4 = GerBag3,
                   LPosList4 = LPosList3,
                   PosList4 = PosList3,
                   GuardGerList4 = [SrcGer3|GuardGerList3]
           end;
       true ->
           #ger{gerBase=#gerBase{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID}} = SrcGer,
           #ger{gerBase=GerBase} = SrcGer,
           GerBase2 = GerBase#gerBase{gerExp=Exp2,gerLevel=Level2, gerQuality= NewQuality},
           SrcGer2 = SrcGer#ger{gerBase=GerBase2},
           if Type =:= ger -> %% Type = ger
                  SrcGer3 = ger_attr:recacl(SrcGer2, PosList3),
                  ger_lib:notify_update(SrcGer3),
                  PosList4 = [SrcGer3|PosList3],
                  GerBag4 = GerBag3,
                  LPosList4 = LPosList3,
                  GuardGerList4 = GuardGerList3;
              true -> %% Type = lieu
                  SrcGer3 = ger_attr:recacl_lieu(SrcGer2, LPosList3),
                  ger_lib:notify_update(SrcGer3),
                  LPosList4 = [SrcGer3|LPosList3],
                  PosList4 = PosList3,
                  GerBag4 = GerBag3,
                  GuardGerList4 = GuardGerList3
           end
	end,
	%% 写武将消耗日志和升品日志
	{Date, _} = Time = erlang:localtime(),
	if is_record(FoodGer, gerSimple) ->
		   LogGerList = [[FoodGer#gerSimple.gerID,FoodGer#gerSimple.gerTypeID,FoodGer#gerSimple.gerLevel,FoodGer#gerSimple.gerQuality]];
	   true ->
		   #ger{gerBase=FoodGerBase} = FoodGer,
		   LogGerList = [[FoodGer#ger.gerID,FoodGerBase#gerBase.gerTypeID,FoodGerBase#gerBase.gerLevel,FoodGerBase#gerBase.gerQuality]]
	end,
	behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, integer_to_list(SrcGerID)),
	behavior_ger_uprank:log(RoleID, SrcGerID, GerTypeID, SrcGerLevel, SrcGerExp, SrcGerRank, Level2, Exp2, NewQuality, FoodGerID, Date, Time),
	
	
	role_data:set_gerBag(GerBag4),
    role_data:set_guardPosList(GuardGerList4),
	ger_lib:notify_update(SrcGer3),
	role_data:set_lieuposList(LPosList4),
	%% 刷新主将
    case Type of
        guard ->
            PosList5 = ger_attr:refresh_fightPower([ger_attr:recacl(E, lists:delete(E, PosList4))||E<-PosList4]);
        _ ->
            PosList5 = ger_attr:refresh_fightPower(PosList4)
    end,
	role_data:set_posList(PosList5),
	#role{roleName=RoleName} = Role,
	?CATCH(role_task_trigger:handle({dispach_task,ger_up_quality,SrcGerID,GerTypeID,NewQuality})),
    case Type of
        guard ->
            #role_guard{infoList=RoleGuardInfo} = role_data:get_roleGuardInfo(),
            ?sendself(#sc_ger_guard_info{list=[ger_lib:to_p_ger_guard(GuardGer, RoleGuardInfo)||GuardGer<-GuardGerList4]});
        _ ->
            next
    end,
	?sendself(#sc_ger_up_rank{foodGerID=FoodGerID, result=1,srcGerID=SrcGerID}),
    case data_ger:get(GerTypeID) of
        #data_ger{gerStar=GerStarLevel} when GerStarLevel >= 5->
            broadcast_server:bc(#sc_message_ger_upLevel{roleName=RoleName,gerInfo=ger_lib:ger2p_ger_view(SrcGer3)});
        _ ->
            next
    end,
	role_homestead:hook_ger_delete(SrcGerID,NewQuality,Level2,[FoodGerID]).

do_up_rank2(SrcGerID, Role, SrcGer, NewQuality, PosList3, LPosList3, GerBag3, GuardGerList3, Type,BagItem2, DelItem, UpdateItem2, UpdateLogList,NeedCoin,NeedGold, NeedRepu)->
	#role{roleID=RoleID} = Role,
	role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
	role_lib:deduct_reputation_f(Role, NeedRepu, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
	role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
    if is_record(SrcGer, gerSimple) ->
           #gerSimple{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID} = SrcGer,
           SrcGer2 = SrcGer#gerSimple{gerQuality=NewQuality},
           SrcGer3 = SrcGer2,
           if Type =:= bag ->
                  PosList4 = PosList3,
                  LPosList4 = LPosList3,
                  GerBag4 = [SrcGer2|GerBag3],
                  GuardGerList4 = GuardGerList3;
              true ->
                  PosList4 = PosList3,
                  LPosList4 = LPosList3,
                  GerBag4 = GerBag3,
                  GuardGerList4 = [SrcGer2|GuardGerList3]
           end;
	   true->
		   #ger{gerBase=#gerBase{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID}=GerBase} = SrcGer,
		   GerBase2 = GerBase#gerBase{gerQuality= NewQuality},
		   SrcGer2 = SrcGer#ger{gerBase=GerBase2},
		   if Type =:= ger ->
				  SrcGer3 = ger_attr:recacl(SrcGer2, PosList3),
				  ger_lib:notify_update(SrcGer3),
				  PosList4 = [SrcGer3|PosList3],
				  LPosList4 = LPosList3,
				  GerBag4 = GerBag3,
                  GuardGerList4 = GuardGerList3;
			  true ->
				  SrcGer3 = ger_attr:recacl_lieu(SrcGer2, LPosList3),
				  ger_lib:notify_update(SrcGer3),
				  PosList4 = PosList3,
				  LPosList4 = [SrcGer3|LPosList3],
				  GerBag4 = GerBag3,
                  GuardGerList4 = GuardGerList3
		   end
	end,
    role_gather:hook_add_ger_list([{GerTypeID,NewQuality}]),
	{Date, _} = Time = erlang:localtime(),
	if DelItem == [] andalso UpdateItem2 == [] ->
		   ignore;
	   true ->
		   %% 写道具日志
		   LogItemList = role_item:itemList2logItemList(DelItem, UpdateLogList),
		   behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_GER_UP_RANK, 1, ""),
		   role_data:set_bagItem(BagItem2),
		   DelItemIDList = [E||#item{itemUID=E}<-DelItem],
		   UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem2],
		   %% 提醒客户端更新物品
		   ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
		   ?sendself(#sc_item_update{updateList=UpdateList})
	end,
	behavior_ger_uprank:log(RoleID, SrcGerID, GerTypeID, SrcGerLevel, SrcGerExp, SrcGerRank, SrcGerLevel, SrcGerExp, NewQuality, 0, Date, Time),
	role_data:set_gerBag(GerBag4),
    role_data:set_guardPosList(GuardGerList4),
	ger_lib:notify_update(SrcGer3),
	%% 提醒客户端更新武将
	role_data:set_lieuposList(LPosList4),
    case Type of
        guard ->
            PosList5 = ger_attr:refresh_fightPower([ger_attr:recacl(E, lists:delete(E, PosList4))||E<-PosList4]);
        _ ->
            PosList5 = ger_attr:refresh_fightPower(PosList4)
    end,
	role_data:set_posList(PosList5),
	#role{roleName=RoleName} = Role,
    ?CATCH(role_task_trigger:handle({dispach_task,ger_up_quality,SrcGerID,GerTypeID,NewQuality})),
    case Type of
        guard ->
            #role_guard{infoList=RoleGuardInfo} = role_data:get_roleGuardInfo(),
            ?sendself(#sc_ger_guard_info{list=[ger_lib:to_p_ger_guard(GuardGer, RoleGuardInfo)||GuardGer<-GuardGerList4]});
        _ ->
            next
    end,
	?sendself(#sc_ger_up_rank{foodGerID=0, result=1,srcGerID=SrcGerID}), 
    case data_ger:get(GerTypeID) of
        #data_ger{gerStar=GerStarLevel} when GerStarLevel >= 5->
            broadcast_server:bc(#sc_message_ger_upLevel{roleName=RoleName,gerInfo=ger_lib:ger2p_ger_view(SrcGer3)});
        _ ->
            ok
    end.


check_up_rank(FoodGerID, SrcGerID) ->
    case role_data:get_ger2(SrcGerID) of		
        {value, SrcGer, PosList2, LPosList2, GerBag2, GuardGerList2, Type} ->
            if is_record(SrcGer, gerSimple) ->
                   #gerSimple{gerQuality=GerQuality,gerTypeID=GerTypeID,gerLevel=SrcGerLevel,gerExp=SrcGerExp} = SrcGer;
               true ->
                   #ger{gerBase=#gerBase{gerQuality=GerQuality,gerTypeID=GerTypeID,gerLevel=SrcGerLevel,gerExp=SrcGerExp}} = SrcGer
            end,
            case util:is_exp_card(GerTypeID) of
                false ->
                    #data_ger{gerStar=GerStar,breakthroughIDList=BreakThroughIDList} = data_ger:get(GerTypeID),
                    QualityLimitList = get_breakthrough_Limit(BreakThroughIDList),
                    Need = lists:keyfind(GerQuality, 4, QualityLimitList) ,
                    {CanUpRank, NeedList} = 
                        case Need of
                            false ->
                                {true, []};
                            #data_ger_breakthrough{condition=Condition}->
                                if Condition =:= [] ->
                                       {false, []};
                                   true->
                                       {true, Condition}
                                end
                        end,
                    
                    %?ERR("what :~w,~w,~w,~w,~w,~w",[GerQuality,NeedList, CanUpRank,Need,QualityLimitList,BreakThroughIDList]),
                    case GerQuality < ?MAX_GER_RANK andalso CanUpRank of 
                        true ->
                            if 
                                NeedList =:= [] ->
                                    case take_ger(FoodGerID, PosList2, LPosList2, GerBag2) of
                                        {value, FoodGer, PosList3, LPosList3, GerBag3, bag} ->
                                            if is_record(FoodGer, gerSimple) ->
                                                   #gerSimple{gerQuality=FoodGerQuality,gerTypeID=FoodGerTypeID,gerExp=FoodGerExp} = FoodGer;
                                               true ->
                                                   #ger{gerBase=#gerBase{gerQuality=FoodGerQuality,gerTypeID=FoodGerTypeID,gerExp=FoodGerExp}} = FoodGer
                                            end,
                                            case util:is_exp_card(FoodGerTypeID) of
                                                false ->
                                                    #data_ger{gerStar=SrcGerStar} = data_ger:get(GerTypeID),
                                                    #data_ger{gerStar=FoodGerStar} = data_ger:get(FoodGerTypeID),
                                                    case SrcGerStar =:= FoodGerStar of
                                                        false ->
                                                            {false,5};
                                                        true ->
                                                            QualityLimitList2 = [E#data_ger_breakthrough.rank_condition||E<-QualityLimitList],
                                                            case catch check_quality(GerQuality, FoodGerQuality, QualityLimitList2) of
                                                                {false, Reason}->
                                                                    {false, Reason};
                                                                {true, NewQuality} ->
                                                                    Role = role_data:get_roleInfo(),
                                                                    NeedCoinT = cacl_up_rank_coin(NewQuality, GerQuality, FoodGerQuality, GerStar),
                                                                    case NeedCoinT < 0 of
                                                                        true ->
                                                                            NeedCoin = 0;
                                                                        false ->
                                                                            NeedCoin = NeedCoinT
                                                                    end,
                                                                    case Role#role.coin >= NeedCoin of
                                                                        true ->
                                                                            case homestead_server:has_homestead_ger(Role#role.roleID, [FoodGerID]) of
                                                                                true->
                                                                                    {false,12};
                                                                                false->
                                                                                    {true, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3, LPosList3, GerBag3, GuardGerList2,Type, SrcGerLevel,SrcGerExp,FoodGerExp}
                                                                            end;
                                                                        false ->
                                                                            {false, 6}
                                                                    end
                                                            end
                                                    end;
                                                true ->
                                                    {false, 11}
                                            end;
                                        _ ->
                                            {false, 4}
                                    end;
                                true ->
                                    if
                                        FoodGerID =:= 0 ->
                                            Role = role_data:get_roleInfo(),
                                            BagItem = role_data:get_bagItem(),
                                            %?ERR("bag:~w",[BagItem]),
                                            case check_need_list(Role, NeedList, BagItem) of
                                                false ->
                                                    {false,8}; 
                                                {BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu} ->
                                                    NewQuality = erlang:min(GerQuality + 1, ?MAX_GER_RANK),
                                                    {true, SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,GuardGerList2,Type, BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu}
                                            end;
                                        true ->
                                            {false, 13}
                                    end
                            end;
                        false ->
                            {false, 2}
                    end;
                true ->
                    {false, 10}
            end;
        false ->
            {false,3}
    end.

check_quality(GerQuality, FoodGerQuality, QualityLimitList)->
	case lists:member(GerQuality, QualityLimitList) of
        true ->
            erlang:throw({false, 9});
        false ->
            next
    end,
    MaxQuality = get_max_quality(GerQuality, lists:sort(QualityLimitList)),
    NextQuality = GerQuality + FoodGerQuality + 1,
    NewQuality = erlang:min(NextQuality, MaxQuality),
    {true, NewQuality}.

get_max_quality(_GerQuality, []) ->
    erlang:throw({false, 7});
get_max_quality(GerQuality, [Quality|QualityList]) ->
    case GerQuality < Quality of
        true ->
            Quality;
        false ->
            get_max_quality(GerQuality, QualityList)
    end.

check_need_list(Role, NeedList, BagItem)->
	if 	NeedList =:= [] ->
			false;
		true ->
			{BagItem2, Result, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu} =
				lists:foldl(fun({coin, Coin},{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc})->
									case Role#role.coin >= Coin of
										true ->
											{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc+Coin, NeedGoldAcc, NeedRepuAcc};
										false ->
											%?ERR("coin"),
											{BagItemAcc,false, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc}
									end;
							   ({gold, Gold},{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc})->
									case role_lib:check_money(Role, gold, Gold) of
										true ->
											{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc+Gold, NeedRepuAcc};
										false ->
											%?ERR("gold"),
											{BagItemAcc,false, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc,NeedRepuAcc}
									end;
							   ({reputation, Repu},{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc,NeedRepuAcc})->
									case Role#role.reputation >= Repu of
										true ->
											{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc+Repu};
										false ->
											%?ERR("repu"),
											{BagItemAcc,false, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc}
									end;
							   ({item, ItemTypeID, Num}, {BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc}) ->
									case item_lib:check_material2(BagItemAcc, ItemTypeID, Num) of
										{BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateItemLogList} ->
											{BagItemAcc2, true, DelItemList++DelItemAcc, UpdateItemList++UpdataItemAcc, UpdateItemLogAcc++UpdateItemLogList,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc};
										_ ->
											%?ERR("item"),
											{BagItemAcc, false, [],[],[],0,0,0}
									end;
							   (_, {_,false, _,_,_,_})->
									%?ERR("other"),
									{BagItem, false, [],[],[],0,0,0};
								(X,ACC) ->
									 ?ERR("unknow item in ger break throw:~w",[X]),
									ACC
							end,{BagItem, true, [], [], [],0,0,0},NeedList),
			case Result of
				true->
					{BagItem2, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu};
				_ ->
					false
			end
	end.

get_breakthrough_Limit(BreakThroughIDList)->
	lists:map(fun(E)->data_ger_breakthrough:get(E) end, BreakThroughIDList).

%% 计算升品需要的银两
cacl_up_rank_coin(NewQuality, Quality1, Quality2, GerStar) ->
	get_up_rank_coin(NewQuality, GerStar) - get_up_rank_coin(Quality1, GerStar) - get_up_rank_coin(Quality2, GerStar).

get_up_rank_coin(0, _GerStar) ->
	0;
get_up_rank_coin(Quality, GerStar) ->
	data_ger_up_rank:get({GerStar, Quality}).
			




do_eat(SrcGerID, SrcGer, NewBagItemList, AddExp, DelItemList, PosList2, LPosList2, GerBag2, GuardGerList2, Type) ->
	#role{level=RoleLevel,roleID=RoleID} = role_data:get_roleInfo(),
    role_data:set_bagItem(NewBagItemList),
	if is_record(SrcGer, gerSimple) ->
		   #gerSimple{gerExp=GerExp,gerQuality=GerRank,gerLevel=GerLevel,gerTypeID=GerTypeID} = SrcGer, 
		   #gerSimple{gerExp=NewExp,gerLevel=NewLevel} = SrcGer2 = ger_lib:add_exp_and_notify2(SrcGer, AddExp, RoleLevel),
           if Type =:= bag ->
                  GerBag3 = [SrcGer2|GerBag2],
                  GuardGerList3 = GuardGerList2;
              true ->
                  GerBag3 = GerBag2,
                  GuardGerList3 = [SrcGer2|GuardGerList2]
           end,
		   role_data:set_gerBag(GerBag3),
           role_data:set_guardPosList(GuardGerList3),
		   role_data:set_lieuposList(LPosList2),
           role_data:set_posList(PosList2);
	   true ->
		   #ger{gerBase=#gerBase{gerExp=GerExp,gerQuality=GerRank,gerLevel=GerLevel,gerTypeID=GerTypeID}} = SrcGer,
		   {_, SrcGer2,_} = ger_lib:add_exp_and_notify(SrcGer, AddExp, RoleLevel, PosList2),
		   #ger{gerBase=#gerBase{gerExp=NewExp,gerLevel=NewLevel}} = SrcGer2,
		   if Type =:= ger ->
				  PosList3 = [SrcGer2|PosList2],
				  role_data:set_gerBag(GerBag2),
				  role_data:set_lieuposList(LPosList2),
				  role_data:set_posList(PosList3);
			  true ->
				  LPosList3 = [SrcGer2|LPosList2],
				  role_data:set_gerBag(GerBag2),
				  role_data:set_posList(PosList2),
				  role_data:set_lieuposList(LPosList3)
		   end
	end,
	{Date, _} = Time = erlang:localtime(),
    LogItemList= lists:foldl(
                   fun(#item{itemUID=DelItemUID, itemTypeID=DelItemTypeID, itemNum=DelItemNum},Acc) ->
                           [[DelItemUID,DelItemTypeID,DelItemNum,DelItemNum]|Acc]
                   end, [],DelItemList),
    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_GER_EAT, 0, ""),
	behavior_ger_uplevel:log(RoleID, SrcGerID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, Date, Time),
	?CATCH(role_task_trigger:handle({dispach_task,ger_up_level,SrcGerID,GerTypeID,NewLevel})),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=DelItemUID}) -> DelItemUID end, DelItemList)}),
	?sendself(#sc_ger_eat{result=0,gerID=SrcGerID}).


	
max_ger_exp() ->
	MaxLevel = data_common:get(max_ger_level),
	data_ger_level:get(MaxLevel+1) -1.

check_eat(GerID, FoodIDList) ->
    case erlang:length(FoodIDList) > 8 of
        false ->
            next;
        true ->
            erlang:throw({false, 1})
    end,
    GerData = role_data:get_ger2(GerID),
    case GerData of
        false ->
            erlang:throw({false, 2});
        _ ->
            next
    end,
    {value, SrcGer,PosList2, LPosList2, GerBag2, GuardGerList2, Type} = GerData,
    #role{level=Level} = role_data:get_roleInfo(),
    if is_record(SrcGer,gerSimple) ->
           #gerSimple{gerLevel=GerLevel,gerExp=GerExp}=SrcGer;
       true ->
           #ger{gerBase=SrcGerBase} = SrcGer,
           #gerBase{gerLevel=GerLevel,gerExp=GerExp} = SrcGerBase
    end,
    case GerLevel < Level of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    MaxExp = max_ger_exp(),
    case GerExp >= MaxExp of
        false ->
            next;
        true ->
            erlang:throw({false, 4})
    end,
    BagItemList = role_data:get_bagItem(),
    {NewBagItemList, AllAddExp, DelItemList} =
        lists:foldr(
          fun(FoodID, {AccBagItemList, AccAddExp, AccDelItemList}) ->
                  case lists:keytake(FoodID, #item.itemUID, AccBagItemList) of
                      false ->
                          erlang:throw({false, 5});
                      {value, #item{itemTypeID=ItemTypeID}=FoodItem, AccBagItemList2} ->
                          case data_item:get(ItemTypeID) of
                              #data_item{itemType=?exp,itemEffectArg=AddExp} ->
                                  {AccBagItemList2, AccAddExp + AddExp, [FoodItem|AccDelItemList]};
                              _ ->
                                  erlang:throw({false, 6})
                          end
                  end
          end, {BagItemList, 0, []}, FoodIDList),
    {true, NewBagItemList, AllAddExp, DelItemList, PosList2, LPosList2, GerBag2, GuardGerList2, SrcGer,Type}.

sort_fighter_list(FighterList) ->
    lists:sort(fun(#ger{gerBase=#gerBase{gerPos=GerPosA}}, #ger{gerBase=#gerBase{gerPos=GerPosB}}) ->
                       GerPosA < GerPosB
               end, FighterList).

do_offline_ger_view(SrcRoleID, SrcServerName, TarRoleID) ->
	case db_sql:get_roleInfo(TarRoleID) of
		#role{}=Role ->
			{FighterList,_} = role_data:get_otherRoleFighter(TarRoleID),
			Reply = ger_view_info(Role, FighterList);
		_ ->
			Reply = #sc_ger_view_other{tarRoleID=TarRoleID,roleName="",roleLevel=0,fightPower=0,gerList=[]}
	end,
    case SrcServerName of
        ?undefined ->
            ?sendself(Reply);
        _ ->
            ?CATCH(global:send(SrcServerName, {cross_ger_view_return, SrcRoleID, Reply}))
    end.
			

ger_view_info(RolePublic, FighterList) when is_record(RolePublic,rolePublic)->
	#rolePublic{roleID=TarRoleID,roleName=RoleName,fightPower=FightPower,level=Level}=RolePublic,
	GerViewList = [ger_lib:ger2p_ger_view(E)||E<-sort_fighter_list(FighterList)],
	#sc_ger_view_other{tarRoleID=TarRoleID,roleName=RoleName,roleLevel=Level,fightPower=FightPower,gerList=GerViewList};
ger_view_info(Role, FighterList) ->
	#role{roleID=TarRoleID,roleName=RoleName,fightPower=FightPower,level=Level}=Role,
	GerViewList = [ger_lib:ger2p_ger_view(E)||E<-sort_fighter_list(FighterList)],
	#sc_ger_view_other{tarRoleID=TarRoleID,roleName=RoleName,roleLevel=Level,fightPower=FightPower,gerList=GerViewList}.

do_offline_ger_view_dtl(SrcRoleID, SrcServerName, TarRoleID) ->
	case db_sql:get_roleInfo(TarRoleID) of
		#role{}=Role ->
			%{FighterList, LieuInfoList} = db_sql:get_fighterList_and_lieuInfo(TarRoleID),
			{FighterList, AtkAdd, HpAdd,LieuInfoList} = db_sql:get_fighterList_and_lieu_add_info(TarRoleID),
			ItemList = role_data:get_otherRoleItemEquips(TarRoleID),
			Reply = ger_view_info_dtl(Role, FighterList, ItemList, AtkAdd, HpAdd,LieuInfoList);				
		_ ->
			Reply = #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName="",roleLevel=0,fightPower=0,gerList=[],equipList=[], atkAdd=0, hpAdd=0,lieuViewList=[]}
	end,
    case SrcServerName of
        ?undefined ->
            ?sendself(Reply);
        _ ->
            ?CATCH(global:send(SrcServerName, {cross_ger_view_dtl_return, SrcRoleID, Reply}))
    end.

ger_view_info_dtl(Role, FighterListT, ItemList, AtkAdd, HpAdd,LieuInfoList)->
	FighterList = ger_attr:refresh_other_fightPower(FighterListT, AtkAdd, HpAdd),
	#role{roleID = TarRoleID, roleName = RoleName, fightPower=FightPower, level=Level}=Role,
	GerViewList = [ger_lib:ger2p_ger_view_dtl(E)||E<-FighterList],
	GerPosList = [ger_lib:ger2p_ger_pos(E)||E<-FighterList],
	EquipViewList = [item_lib:item2p_item_view_dtl(E)||E<-ItemList],
	#sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName=RoleName,roleLevel=Level,fightPower=FightPower, gerList=GerViewList
						   ,equipList=EquipViewList,gerPosList=GerPosList,atkAdd=AtkAdd, hpAdd=HpAdd,lieuViewList=LieuInfoList}.

do_down(#ger{gerID=GerID,gerBase=#gerBase{gerTypeID=GerTypeID}}=DownGer, PosList2) ->
    GerBag = role_data:get_gerBag(),
    GerBag2 = [ger_lib:ger2gerSimple(DownGer)|GerBag],
    role_data:set_gerBag(GerBag2),
    PosList3 = [destiny_impact(E,GerTypeID, GerTypeID, PosList2)||E<-PosList2],
    PosList4 = ger_attr:refresh_fightPower(PosList3),
    role_data:set_posList(PosList4),
    ?sendself(#sc_ger_down{result=0,gerID=GerID}).


%%%================================上阵武将=========================================================
do_standup(GerPos, GerID, UpGer, PosList, GerBag2) ->
	GerTypeID = UpGer#gerSimple.gerTypeID,
	case util:fun_take(fun(E) ->GerPos == E#ger.gerBase#gerBase.gerPos end, PosList) of
		false ->
			GerTypeID2 = 0,
			%% 新的出站列表
			UpGer2 = ger_attr:recacl(UpGer#gerSimple{gerPos=GerPos}, PosList),
			PosList2 = [UpGer2|PosList],
			%% 重算天命
			PosList4 = [UpGer2|[destiny_impact(E,GerTypeID, GerTypeID2, PosList2)||E<-PosList]],
			?CATCH(role_task_trigger:handle({dispach_task,role_up_ger_num,length(PosList4)})),
			GerBag3 = GerBag2;
		{value, #ger{gerID=GerID2,gerBase=#gerBase{gerTypeID=GerTypeID2}}=DownGer, PosList2} ->
			%% 两个武将交换装备
			role_data:replace_equip(GerID, GerID2),
			UpGer2 = ger_attr:recacl(UpGer#gerSimple{gerPos=GerPos}, PosList2),
			PosList3 = [UpGer2|PosList2],
			if GerTypeID2 == GerTypeID ->
				   PosList4 = PosList3;
			   true ->
				   %% 重算天命
				   PosList4 = [UpGer2 | [destiny_impact(E,GerTypeID, GerTypeID2, PosList3)||E<-PosList2]]
			end,
			GerBag3 = [ger_lib:ger2gerSimple(DownGer)|GerBag2]
	end,
	role_data:set_gerBag(GerBag3),
	ger_lib:notify_update(UpGer2),
	PosList5 = ger_attr:refresh_fightPower(PosList4),
	role_data:set_posList(PosList5),
	?sendself(#sc_ger_standup{gerPos=GerPos, gerID=GerID, result=1}).
	

%% 计算受影响的出站武将的属性
destiny_impact(Ger,GerTypeID, GerTypeID2,PosList) ->
	#ger{gerID=GerID,gerBase=#gerBase{gerTypeID=T}} = Ger,
	%% 根据天命配置判断是否需要计算此武将属性
	case lists:any(fun(E) -> E=:=GerTypeID orelse E =:= GerTypeID2 end, data_destiny_rela:get(T)) of
		true ->
			Num = active_destiny_nun(GerID,T,PosList,false),
			?CATCH(role_task_trigger:handle({dispach_task,role_active_destiny_num,Num})),
			Ger2 = ger_attr:recacl(Ger,PosList),
			ger_lib:notify_update(Ger2),
			Ger2;
		false ->
			Ger
	end.
%%计算激活的天命条数
active_destiny_nun(GerID,GerTypeID,PosList,IsLieu)->
	EquipList = role_data:get_equip(GerID),
	SpLieuList=
		case IsLieu of
			true->
				[];
			false->
				ger_attr:get_sp_lieu_typeIDList(role_data:get_lieutenantInfo())
		end,
	PosTypeIDList =
		lists:map(fun(#ger{gerBase=#gerBase{gerTypeID=E}}) ->
						  E;
					 (#gerSimple{gerTypeID=E}) ->
						  E
				  end, PosList),
	ArmedGerTypeIDList = lists:delete(GerTypeID, SpLieuList++PosTypeIDList),
	EquipTypeIDList = [ItemTypeID || #item{itemTypeID=ItemTypeID} <- EquipList],
	#data_ger{destinyIDList=DesIDList} = data_ger:get(GerTypeID),
	lists:foldl(fun(DesID,C)->
						#data_destiny{destinyType=DesType, destinyNeedList=DesNeedList} = data_destiny:get(DesID),
						case ger_attr:check_destiny_add(DesType, ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) of
							true->
								C+1;
							false->
								C
						end
				end, 0, DesIDList).
	
%% 移动武将位置
cs_ger_move_pos(#cs_ger_move_pos{gerPos=GerPos,targetPos=TargetPos}) ->
	case check_move_pos(GerPos, TargetPos) of
		{true, PosList2} ->
			?sendself(#sc_ger_move_pos{gerPos=GerPos,targetPos=TargetPos,result=1}),
			PosList3 = ger_attr:refresh_fightPower(PosList2),
			role_data:set_posList(PosList3),
            guard_ger_move(GerPos, TargetPos),
			?CATCH(role_task_trigger:handle({dispach_task,role_ger_move_pos_times}));
		{false,Reason} ->
			?sendself(#sc_ger_move_pos{gerPos=GerPos,targetPos=TargetPos,result=Reason})
    end.

%% @doc 出售武将
cs_ger_sell(#cs_ger_sell{gerIDList=GerIDList}) ->
	case check_sell(GerIDList) of
		{false, Reason} ->
			?sendself(#sc_ger_sell{result=Reason,reward=[]});
		{GerBag2, InfoAcc} ->
			do_ger_sell(GerBag2, GerIDList, InfoAcc)
			end.

%% @doc 获取武将详细信息
cs_ger_detail(#cs_ger_detail{gerID=GerID}) ->
	{value, #ger{gerAttr=GerAttr},_,_,_,_} = role_data:get_ger(GerID),
	Reply = #sc_ger_detail{
						   gerID=GerID,
						   gerSpInit		=  GerAttr#gerAttr.gerSpInit		,
						   gerSpMax        =  GerAttr#gerAttr.gerSpMax        ,
						   gerCritic        =  GerAttr#gerAttr.gerCritic        ,
						   gerCriticReduce  =  GerAttr#gerAttr.gerCriticReduce  ,
						   gerDoom          =  GerAttr#gerAttr.gerDoom          ,
						   gerMiss          =  GerAttr#gerAttr.gerMiss          ,
						   gerAbsorb        =  GerAttr#gerAttr.gerAbsorb        ,
						   gerDamageBack    =  GerAttr#gerAttr.gerDamageBack    ,
						   gerReel          =  GerAttr#gerAttr.gerReel          ,
						   gerReelReduce    =  GerAttr#gerAttr.gerReelReduce    ,
						   gerPhyDefBite    =  GerAttr#gerAttr.gerPhyDefBite    ,
						   gerPhyDef        =  GerAttr#gerAttr.gerPhyDef        ,
						   gerMagDefBite    =  GerAttr#gerAttr.gerMagDefBite    ,
						   gerMagDef        =  GerAttr#gerAttr.gerMagDef        
						  },
	?sendself(Reply).
						  
						  
						  
cs_ger_lieu_pos_list(_)->
	PosList = [begin
				   EquipList=[Equip#item.itemUID || Equip <-role_data:get_equip(GerID)],				   
				   #p_ger_pos_info{gerID=GerID,gerPos=Pos,itemUIDList=EquipList}
			   end ||#ger{gerBase=#gerBase{gerPos=Pos},gerID=GerID}<-role_data:get_lieuposList()],
	Reply = #sc_ger_lieu_pos_list{gerPosInfoList=PosList},
	?sendself(Reply).
						  
cs_ger_lieu_standup(#cs_ger_lieu_standup{gerPos=Pos,gerID=ID})->
		case check_lieu_standup(Pos, ID) of
		{true, UpGer, PosList, GerBag2} ->
			do_lieu_standup(Pos, ID, UpGer, PosList, GerBag2);
		{false,Reason} ->
			?sendself(#sc_ger_lieu_standup{gerPos=Pos,gerID=ID, result=Reason})
			end.

cs_ger_lieu_move_pos(#cs_ger_lieu_move_pos{gerPos=GerPos,targetPos=TargetPos})->
		case check_lieu_move_pos(GerPos, TargetPos) of
		{true, PosList2} ->
			?sendself(#sc_ger_lieu_move_pos{gerPos=GerPos,targetPos=TargetPos,result=1}),
			%PosList3 = ger_attr:refresh_fightPower(PosList2),
			%role_data:set_lieuposList(PosList2);
			role_data:init_lieuList(PosList2);
		{false,Reason} ->
			?sendself(#sc_ger_lieu_move_pos{gerPos=GerPos,targetPos=TargetPos,result=Reason})
			end.

cs_ger_lieu_untie(#cs_ger_lieu_untie{gerPos=GerPos})->
	case check_untie(GerPos) of
		{false, Reason} ->
			?sendself(#sc_ger_lieu_untie{result=Reason,info=[]});
		{true, Role, InitInfo,Cost} ->
			do_untie(Role,InitInfo,GerPos,Cost);
		_ ->
			?sendself(#sc_ger_lieu_untie{result=4,info=[]})
	end.

cs_ger_lieu_info_list(_)->
	InfoList=
		lists:foldl(fun(#t_lieu{pos=Pos,infoID1=InfoID1,isLock1=IsLock1,infoID2=InfoID2,isLock2=IsLock2,infoID3=InfoID3,isLock3=IsLock3},Acc)->
							[#p_ger_lieu_info{gerPos=Pos,specialID=InfoID1,isLock1=IsLock1,attAddID=InfoID2,isLock2=IsLock2,hpAddID=InfoID3,isLock3=IsLock3}|Acc]
							end,[],role_data:get_lieutenantInfo()),
	?sendself(#sc_ger_lieu_info_list{info=InfoList}).

cs_ger_lieu_lock_clo(#cs_ger_lieu_lock_clo{gerPos=Pos, num=Num})->
	case check_lieu_lock(Pos, Num) of
		{false, Reason}->
			?sendself(#sc_ger_lieu_lock_clo{result=Reason});
		{true , NewLieuInfo,LieuAcc}->
			do_lieu_lock(NewLieuInfo,LieuAcc)
	end.

cs_ger_lieu_unlock_clo(#cs_ger_lieu_unlock_clo{gerPos=Pos, num=Num})->
	case check_lieu_unlock(Pos, Num) of
		{false, Reason} ->
			?sendself(#sc_ger_lieu_unlock_clo{result=Reason});
		{true, NewInfo, LieuAcc} ->
			role_data:set_lieutenantInfo([NewInfo|LieuAcc]),
			?sendself(#sc_ger_lieu_unlock_clo{result=1})
	end.

cs_ger_lieu_refresh_clo(#cs_ger_lieu_refresh_clo{gerPos=Pos})->
	case check_lieu_refresh(Pos) of
		{false, Reason} ->
			PInfo=#p_ger_lieu_info{gerPos=0, specialID=0, isLock1=0, attAddID=0, isLock2=0, hpAddID=0, isLock3=0},
			?sendself(#sc_ger_lieu_refresh_clo{result=Reason,info=PInfo});
		{true,LieuInfo, BagOther2, Role, NeedGold, DelAcc, UpdateAcc, UpdateLogAcc,LieuAcc,RefreshTimes}->
			role_data:set_bagItem(BagOther2),
			Role2 = role_lib:deduct_gold_f(Role,NeedGold, ?MONEY_DEC_TYPE_LIEU_REFRESH, 0,""),
			do_lieu_refresh(Role2, LieuInfo, DelAcc, UpdateAcc, UpdateLogAcc, LieuAcc,RefreshTimes)
	end.

cs_ger_lieu_tie_info(_)->
	LieuInfo = role_data:get_lieutenantInfo(),
	Result = lists:foldl(fun(#t_lieu{pos=Pos},Acc)->
								 [Pos|Acc]
						 end, [],LieuInfo),
	?sendself(#sc_ger_lieu_tie_info{posList=Result}).

cs_ger_lieu_refresh_freeTimes(_)->
	RoleTimes = role_data:get_roleTimes(),
	ReFreshTimes = RoleTimes#roleTimes.refreshLieuTimes,
	if ReFreshTimes > 0 ->
		   ?sendself(#sc_ger_lieu_refresh_freeTimes{times=ReFreshTimes});
	   true ->
		   ?sendself(#sc_ger_lieu_refresh_freeTimes{times=0})
	end.

do_lieu_refresh(#role{roleID=RoleID}, LieuInfo, DelAcc, UpdateAcc, UpdateLogAcc, LieuAcc, RefreshTimes)->
	case UpdateAcc =/= [] of
		true ->
			UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
			?sendself(#sc_item_update{updateList=UpdateList});
		_ ->
			ignore
	end,
	case DelAcc =/= [] of
		true ->
			DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
			?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList});
		_ ->
			ignore
	end,
	%%道具消耗,由于可能多次消耗的是相同的道具,所以这里同时产生的消耗应该将最少的结果最为最终结果
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_LIEU_REFRESH, 0, ""),

	case data_fixed_lieu_refresh:get(RefreshTimes) of
		?undefined ->
		   #t_lieu{pos=Pos, infoID1=ID1, infoID2=ID2, infoID3 = ID3, isLock1=LK1, isLock2=LK2, isLock3=LK3} = 
					  NewLieuInfo = get_random_lieu_result(LieuInfo);
	   ValueInfo ->
		   #t_lieu{pos=Pos, infoID1=ID1, infoID2=ID2, infoID3 = ID3, isLock1=LK1, isLock2=LK2, isLock3=LK3} = 
					  NewLieuInfo = get_special_lieu_result(LieuInfo,ValueInfo)
	end,
	role_data:set_lieutenantInfo([NewLieuInfo|LieuAcc]),
	role_data:set_lieuposList(role_data:get_lieuposList()),
	PInfo = #p_ger_lieu_info{gerPos=Pos, specialID=ID1, isLock1=LK1, attAddID=ID2, isLock2=LK2, hpAddID=ID3, isLock3=LK3},
	?sendself(#sc_ger_lieu_refresh_clo{result=1, info=PInfo}).

get_special_lieu_result(#t_lieu{infoID1=ID1,isLock1=LK1,infoID2=ID2,isLock2=LK2,infoID3=ID3,isLock3=LK3}=Info,{S1,S2,S3})->
	ID1N = get_lieu_special_ID(ID1, LK1, S1,special_ger),
	ID2N = get_lieu_special_ID(ID2, LK2, S2,att_add),
	ID3N = get_lieu_special_ID(ID3, LK3, S3,hp_add),
	Info#t_lieu{infoID1=ID1N, infoID2=ID2N, infoID3=ID3N}.

get_lieu_special_ID(ID, LK, List, Type) ->
	if LK =:= 1 ->
		   ID;
	   true ->
		   Random_List = 
			   case Type of
				   special_ger ->
					   lists:foldl(fun(E,Acc)->lists:delete(E, Acc) end, List, get_lieu_spID_list());
				   _ ->
					   List
			   end,
		   if Random_List == [] ->
				  get_lieu_random_ID(ID, LK, Type);
			  true ->
				  
				%  ?ERR("~w",[Random_List]),
				  util:random_one_from_list(Random_List)
		   end
	end.

get_random_lieu_result(#t_lieu{infoID1=ID1,isLock1=LK1,infoID2=ID2,isLock2=LK2,infoID3=ID3,isLock3=LK3}=Info)->
	ID1N = get_lieu_random_ID(ID1, LK1, special_ger),
	ID2N = get_lieu_random_ID(ID2, LK2, att_add),
	ID3N = get_lieu_random_ID(ID3, LK3, hp_add),
	Info#t_lieu{infoID1=ID1N, infoID2=ID2N, infoID3=ID3N}.

get_lieu_random_ID(ID1,LK1,Type)->
	if LK1 =:= 1 ->
		   ID1;
	   true ->
		   Random_List = data_lieu_clo_setting:get(Type),
		   Random_ListT = util:random_one_from_weigh_list(Random_List),
		   Random_List2 = 
			   case Type of
				   special_ger ->
					   lists:foldl(fun(E,Acc)->lists:delete(E, Acc) end, Random_ListT, get_lieu_spID_list());
				   _ ->
					   Random_ListT
			   end,
		   util:random_one_from_list(Random_List2)
	end.

check_lieu_refresh(Pos) ->
	LieuInfo0 = role_data:get_lieutenantInfo(),
	case lists:keytake(Pos, #t_lieu.pos, LieuInfo0) of
		false ->
			{false, 2};
		{value, LieuInfo, LieuAcc}->
			BagOther = role_data:get_bagItem(),
			Role = role_data:get_roleInfo(),
			%{ItemTypeID, ItemNum, GoldUnit} 
			RoleTimes = role_data:get_roleTimes(),
			ReFreshTimes = RoleTimes#roleTimes.refreshLieuTimes,
			if ReFreshTimes > 0 ->
				   role_data:set_roleTimes(RoleTimes#roleTimes{refreshLieuTimes = ReFreshTimes-1}),
				   {true, LieuInfo, BagOther, Role, 0, [], [], [], LieuAcc,0};
			   true ->
				   Refresh_lieu_need = data_lieu_clo_setting:get(refresh_lieu_need),
				   Close_lieu_need = data_lieu_clo_setting:get(get_lieu_clo_count(LieuInfo)),
				   
				   case check_need_material_or_gold(BagOther, Role, [Refresh_lieu_need, Close_lieu_need],{0, [],[],[]}) of
					   {false, Reason} ->
						   {false , Reason};
					   {BagOther2, Role, {NeedGold, DelAcc, UpdateAcc, UpdateLogAcc}} ->
						   PayRefreshLieuTimes = RoleTimes#roleTimes.alreadyPayRefreshLieuTimes,
						   role_data:set_roleTimes(RoleTimes#roleTimes{alreadyPayRefreshLieuTimes=PayRefreshLieuTimes+1}),
						   
						   {true, LieuInfo, BagOther2, Role, NeedGold, DelAcc, lists:reverse(UpdateAcc), UpdateLogAcc, LieuAcc,PayRefreshLieuTimes}
				   end
			end
	end.

check_need_material_or_gold(BagOther, Role, [],Result) ->
	{BagOther, Role,Result};
check_need_material_or_gold(BagOther, Role, [{ItemTypeID, ItemNum,GoldUnit}|T],  {NeedGold, DelAcc, UpdateAcc, UpdateLogAcc})->
	case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
		{BagOther2, 0, DelAcc2, UpdateAcc2, UpdateLogAcc2} ->
			check_need_material_or_gold(BagOther2, Role, T, {NeedGold, DelAcc2++DelAcc, UpdateAcc2++UpdateAcc, UpdateLogAcc2++UpdateLogAcc});
		{BagOther3, RestNum, DelAcc3, UpdateAcc3, UpdateLogAcc3}->
			NeedGold2 = RestNum * GoldUnit + NeedGold,
			case role_lib:check_money(Role, gold, NeedGold2) of
				true ->
					check_need_material_or_gold(BagOther3, Role, T, {NeedGold2, DelAcc3++DelAcc, UpdateAcc3++UpdateAcc, UpdateLogAcc3++UpdateLogAcc});
				false ->
					{false,3}
			end;
		_ ->
			{false, 3}
	end.
					
get_lieu_clo_count(#t_lieu{isLock1=LK1, isLock2=LK2, isLock3=LK3})->
	LK1 + LK2 + LK3.

check_lieu_unlock(Pos, Num)->
	LieuInfo=role_data:get_lieutenantInfo(),
	case lists:keytake(Pos, #t_lieu.pos, LieuInfo) of
		false ->
			{false, 3};
		{value, Info, LieuAcc}->
			case check_lieu_unlock2(Num, Info) of
				{true, NewLieuInfo}->
					{true, NewLieuInfo, LieuAcc};
				Result ->
					Result
			end
	end.

check_lieu_unlock2(Num, {t_lieu,_, _, LK1, _, LK2, _, LK3}=Info)->
	if Num =:= 1, LK1 =:= 1 ->
		   {true, Info#t_lieu{isLock1=0}};
	   Num =:= 2, LK2 =:= 1 ->
		   {true, Info#t_lieu{isLock2=0}};
	   Num =:= 3, LK3 =:= 1 ->
		   {true, Info#t_lieu{isLock3=0}};
	   true ->
		   {false, 2}
	end.


do_lieu_lock(#role{roleID=RoleID}, NewLieuInfo, DelAcc, UpdateLogAcc,LieuAcc)->
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_LIEU_LOCK, 0, ""),

	role_data:set_lieutenantInfo([NewLieuInfo|LieuAcc]),
	?sendself(#sc_ger_lieu_lock_clo{result=1}).
do_lieu_lock(NewLieuInfo, LieuAcc)->
	role_data:set_lieutenantInfo([NewLieuInfo|LieuAcc]),
	?sendself(#sc_ger_lieu_lock_clo{result=1}).
	
check_lieu_lock(Pos, Num)->
	LieuInfo0 = role_data:get_lieutenantInfo(),
	case lists:keytake(Pos, #t_lieu.pos, LieuInfo0) of
		false ->
			{false, 3};
		{value, LieuInfo, LieuAcc} ->
			case check_lieu_lock2(Num, LieuInfo) of
				{true, NewLieuInfo,LKSum}->
					if LKSum >= 0 , LKSum < 3 ->
						   {true, NewLieuInfo,LieuAcc};
					   true ->
						   {false, 5}
					end;
%% 					case data_lieu_clo_setting:get(LKSum+1) of
%% 						{ItemTypeID, ItemNum, GoldUnit} ->
%% 							BagOther = role_data:get_bagItem(),
%% 							case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
%% 								{BagOther2, 0, DelAcc, _UpdateAcc, UpdateLogAcc} ->
%% 									{true, NewLieuInfo, BagOther2, DelAcc, UpdateLogAcc,LieuAcc};
%% 								{BagOther2, RestNum, DelAcc, _UpdateAcc, UpdateLogAcc} ->
%% 									NeedGold = RestNum * GoldUnit,
%% 									Role = role_data:get_roleInfo(),
%% 									case role_lib:check_money(Role, gold, NeedGold) of
%% 										true ->
%% 											{true,NewLieuInfo, BagOther2, Role, NeedGold, DelAcc, UpdateLogAcc,LKSum,LieuAcc};
%% 										false ->
%% 											{false, 4}
%% 									end;
%% 								_ ->
%% 									{false, 4}
%% 							end;
%% 						_ ->
%% 							{false, 5}
%% 					end;
				Result ->
					Result
			end
	end.

check_lieu_lock2(Num, {t_lieu,_, _, LK1, _, LK2, _, LK3}=Info)->
	if Num =:= 1, LK1 =:= 0 ->
		   {true, Info#t_lieu{isLock1=1},LK1+LK2+LK3};
	   Num =:= 2, LK2 =:= 0 ->
		   {true, Info#t_lieu{isLock2=1},LK1+LK2+LK3};
	   Num =:= 3, LK3 =:= 0 ->
		   {true, Info#t_lieu{isLock3=1},LK1+LK2+LK3};
	   true ->
		   {false, 2}
	end.

do_untie(Role, InitInfo,GerPos,Cost)->
	role_lib:deduct_reputation_f(Role, Cost, ?MONEY_DEC_TYPE_UNTIE_LIEU, 0, ""),
	{t_lieu,_,ID1,_,ID2,_,ID3,_}=LieuInfoT=get_init_lieutenant(GerPos,InitInfo),
	LieuInfo = role_data:get_lieutenantInfo(),
	role_data:set_lieutenantInfo([LieuInfoT|LieuInfo]),
	Info=#p_ger_lieu_info{gerPos=GerPos,specialID=ID1,attAddID=ID2,hpAddID=ID3,isLock1=0,isLock2=0,isLock3=0},
	?sendself(#sc_ger_lieu_untie{result=1,info=[Info]}).

check_untie(GerPos)->
	LieuInfo = role_data:get_lieutenantInfo(),
	case lists:keyfind(GerPos, #t_lieu.pos, LieuInfo) of
		#t_lieu{} ->
			{false, 4};
		_ ->
			#data_lieu_open_charge{needLevel=NeedLevel,cost=Cost,initList=InitInfo}=data_lieu_open_charge:get(GerPos),
			#role{level=Level}=role_data:get_roleInfo(),
			if Level < NeedLevel ->
				   {false,2};
			   true ->
				   case check_untie_cost(Cost) of
					   {true, Role}->
						   {true, Role, InitInfo,Cost};
					   Result ->
						   Result
				   end
			end
	end.
	
check_untie_cost(Cost)->
	#role{reputation=Repu} = Role = role_data:get_roleInfo(),
	if Cost > Repu ->
		   {false,3};
	   true ->
		   {true, Role}
	end.

check_lieu_move_pos(GerPos, TargetPos) ->
	PosList = ger_attr:get_original_lieu_posList(),
	%PosList = role_data:get_lieuposList(),
	case util:fun_take(fun(E) -> GerPos== E#ger.gerBase#gerBase.gerPos end, PosList) of
		false ->
			{false, 3};
		{value, E, PosList2} ->
			case util:fun_take(fun(E1) -> TargetPos== E1#ger.gerBase#gerBase.gerPos end, PosList2) of
				false ->
					PosList4 = [?change_pos(E,TargetPos)|PosList2];
				{value, E2, PosList3} ->
					PosList4 = [?change_pos(E2,GerPos), ?change_pos(E,TargetPos)| PosList3]
			end,
			{true, PosList4}
	end.

% check open
check_lieu_standup(GerPos, GerID)->
    LieuInfoT = role_data:get_lieutenantInfo(),
    case lists:keyfind(GerPos, #t_lieu.pos, LieuInfoT) of
        #t_lieu{} ->
            GerBag = role_data:get_gerBag(),
            case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
                false ->
                    {false, 3};
                {value, #gerSimple{gerTypeID=GerTypeID}=UpGer, GerBag2} ->
                    case util:is_exp_card(GerTypeID) of
                        false ->
                            case (is_integer(GerPos) andalso GerPos >0 andalso GerPos =< 6) of
                                true ->
                                    LPosList = ger_attr:get_original_lieu_posList(),
                                    %LPosList = role_data:get_lieuposList(),
                                    case lists:any(fun(E) ->
                                                           #ger{gerBase=EBase} = E,
                                                           EBase#gerBase.gerTypeID =:= GerTypeID andalso EBase#gerBase.gerPos =/= GerPos 
                                                   end, LPosList) of
                                        true ->
                                            {false, 4};
                                        false ->
                                            PosList = role_data:get_posList(),
                                            case lists:any(fun(E)->
                                                                   #ger{gerBase=EBase}=E,
                                                                   EBase#gerBase.gerTypeID =:= GerTypeID
                                                           end, PosList) of
                                                true ->
                                                    {false, 4};
                                                false ->
                                                    {true, UpGer, LPosList, GerBag2}
                                            end
                                    end;
                                false ->
                                    {false, 2}
                            end;
                        true ->
                            {false, 6}
                    end
            end;
        X ->
            ?ERR("X:~w",[X]),
            {false, 5}
    end.


do_lieu_standup(GerPos, GerID, UpGer, PosList, GerBag2)->
	GerTypeID = UpGer#gerSimple.gerTypeID,
	case util:fun_take(fun(E) ->GerPos == E#ger.gerBase#gerBase.gerPos end, PosList) of
		false ->
			GerTypeID2 = 0,
			%% 新的出站列表
			UpGer2 = ger_attr:recacl_lieu(UpGer#gerSimple{gerPos=GerPos}, PosList),
			PosList2 = [UpGer2|PosList],
			%% 重算天命
			PosList4 = [UpGer2|[destiny_impact_lieu(E,GerTypeID, GerTypeID2, PosList2)||E<-PosList]],
			GerBag3 = GerBag2;
		{value, #ger{gerID=GerID2,gerBase=#gerBase{gerTypeID=GerTypeID2}}=DownGer, PosList2} ->
			%% 两个武将交换装备
			role_data:replace_equip(GerID, GerID2),
			UpGer2 = ger_attr:recacl_lieu(UpGer#gerSimple{gerPos=GerPos}, PosList2),
			PosList3 = [UpGer2|PosList2],
			if GerTypeID2 == GerTypeID ->
				   PosList4 = PosList3;
			   true ->
				   %% 重算天命
				   PosList4 = [UpGer2 | [destiny_impact_lieu(E,GerTypeID, GerTypeID2, PosList3)||E<-PosList2]]
			end,
			GerBag3 = [ger_lib:ger2gerSimple(DownGer)|GerBag2]
	end,
	role_data:set_gerBag(GerBag3),

	%ger_lib:notify_update(UpGer2),
	%role_data:init_lieuList(PosList4),
	%PosList5 = ger_attr:refresh_fightPower(PosList4),
	role_data:set_lieuposList(PosList4),
	?sendself(#sc_ger_lieu_standup{gerPos=GerPos, gerID=GerID, result=1}).

destiny_impact_lieu(Ger,GerTypeID, GerTypeID2,PosList) ->
	#ger{gerID=GerID,gerBase=#gerBase{gerTypeID=T}} = Ger,
	%% 根据天命配置判断是否需要计算此武将属性
	case lists:any(fun(E) -> E=:=GerTypeID orelse E =:= GerTypeID2 end, data_destiny_rela:get(T)) of
		true ->
			Num = active_destiny_nun(GerID,GerTypeID,PosList,true),
			?CATCH(role_task_trigger:handle({dispach_task,role_active_destiny_num,Num})),
			Ger2 = ger_attr:recacl_lieu(Ger,PosList),
			ger_lib:notify_update(Ger2),
			Ger2;
		false ->
			Ger
	end.

do_ger_sell(GerBag2, _GerIDList, InfoAcc)->
	role_data:set_gerBag(GerBag2),
	LogGerList = [ [GerID,GerSimple#gerSimple.gerTypeID,GerSimple#gerSimple.gerLevel,GerSimple#gerSimple.gerQuality]  || {_,#gerSimple{gerID=GerID}=GerSimple}<- InfoAcc],
	#role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
	{Date, _} = Time = erlang:localtime(),
	behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_SELL_GER, 0, ""),
	{AddCoin, ItemList} = calc_split(InfoAcc),
	role_lib:add_coin_f(RoleInfo, AddCoin, ?MONEY_ADD_TYPE_SELL_GER, 0, ""),
	item_lib:add_item_f(ItemList, ?MONEY_ADD_TYPE_SELL_GER, 0, ""),
	P_reward_list = [#p_reward_view3{type=1,typeID=TypeID,num=Num}||#new_item{itemTypeID=TypeID, itemNum=Num}<-ItemList],
	?sendself(#sc_ger_sell{result=1, reward=[#p_reward_view3{type=1, typeID=21100, num=AddCoin}]++P_reward_list}).

calc_split(InfoAcc)->
	lists:foldl(fun({#data_ger{price=SplitID}, #gerSimple{gerQuality = GerRank,gerLevel=GerLevel}}, {CoinAcc, ItemAcc})->
						#data_card_split{coin=Coin, itemList=ItemList} = data_card_split:get(SplitID),
						{CoinT, NewItemList} = role_item:get_item_split(Coin, ItemList, GerRank+1, ItemAcc,GerRank, GerLevel),
						{CoinAcc + CoinT, NewItemList}
						end, {0, []}, InfoAcc).

check_sell(GerIDList) ->
	GerBag = role_data:get_gerBag(),
	util:foldl(fun(GerID,{GerBagAcc, DelAcc}) ->
						case check_sell2(GerID,GerBagAcc) of
							{false, _Reason} =R->
								{return, R};
							{true, DataGer, GerInfo, GerBagAcc2} ->
								{GerBagAcc2, [{DataGer,GerInfo}|DelAcc]}
						end
			   end, {GerBag,[]}, GerIDList).

check_sell2(GerID, GerBag) ->
	case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
		{value, GerInfo, GerBag2} when is_record(GerInfo, gerSimple) ->
			#gerSimple{gerTypeID=GerTypeID} = GerInfo,
					case data_ger:get(GerTypeID) of
						DataGer when is_record(DataGer, data_ger) ->
							{true, DataGer, GerInfo, GerBag2};
						_ ->
							{false, 4}
					end;
		false ->
			{false, 2}
	end.

check_move_pos(GerPos, TargetPos) ->
	PosList = ger_attr:get_original_posList(),
	%PosList = role_data:get_posList(),
	case util:fun_take(fun(E) -> GerPos== E#ger.gerBase#gerBase.gerPos end, PosList) of
		false ->
			{false, 3};
		{value, E, PosList2} ->
			case util:fun_take(fun(E1) -> TargetPos== E1#ger.gerBase#gerBase.gerPos end, PosList2) of
				false ->
					PosList4 = [?change_pos(E,TargetPos)|PosList2];
				{value, E2, PosList3} ->
					PosList4 = [?change_pos(E2,GerPos), ?change_pos(E,TargetPos)| PosList3]
			end,
			{true, PosList4}
	end.

check_down(GerID) ->
    PosList = ger_attr:get_original_posList(),
    case erlang:length(PosList) >= 2 of
        true ->
            case lists:keytake(GerID, #ger.gerID, PosList) of
                false ->
                    {false, 1};
                {value, DownGer, PosList2} ->
                    case role_data:get_equip(GerID) of
                        [] ->
                            {true, DownGer, PosList2};
                        _ ->
                            {false, 2}
                    end
            end;
        false ->
            {false, 3}
    end.
					
	
check_standup(GerPos, GerID) ->
    GerBag = role_data:get_gerBag(),
    case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
        false ->
            {false, 3};
        {value, #gerSimple{gerTypeID=GerTypeID}=UpGer, GerBag2} ->
            case util:is_exp_card(GerTypeID) of
                false ->
                    case (is_integer(GerPos) andalso GerPos >0 andalso GerPos =< 6) of
                        true ->
                            PosList = ger_attr:get_original_posList(),
                            %PosList = role_data:get_posList(),
                            case lists:any(fun(E) ->
                                                   #ger{gerBase=EBase} = E,
                                                   EBase#gerBase.gerTypeID =:= GerTypeID andalso EBase#gerBase.gerPos =/= GerPos 
                                           end, PosList) of
                                true ->
                                    {false, 4};
                                false ->
                                    LPosList = role_data:get_lieuposList(),
                                    case lists:any(fun(E) ->
                                                           #ger{gerBase=EBase} = E,
                                                           EBase#gerBase.gerTypeID =:= GerTypeID
                                                   end, LPosList) of
                                        true ->
                                            {false, 4};
                                        false ->
                                            GuardGerList = role_data:get_guardPosList(),
                                            case lists:any(fun(E) ->
                                                                   E#gerSimple.gerTypeID =:= GerTypeID
                                                           end, GuardGerList) of
                                                true ->
                                                    {false, 6};
                                                false ->
                                                    {true, UpGer, PosList, GerBag2}
                                            end
                                    end
                            end;
                        false ->
                            {false, 2}
                    end;
                true ->
                    {false, 5}
            end
    end.
	

	
check_order(PosList) ->
	Role = role_data:get_roleInfo(),
	MaxNum = data_ger_num:get(Role#role.level),
	%% 最大出战个数
	if length(PosList) > MaxNum ->
		   {false, 3};
	   true ->
		   {Pos,IDList} = lists:foldl(fun(#posInfo{gerID=GerID,gerPos=GerPos}, {PosAcc,IDAcc}) ->
											  {[GerPos|PosAcc],[GerID|IDAcc]}
									  end, {[], []}, PosList),
		   %% 是否有重复项
		   case ((util:is_duplicate(Pos)=:=false) andalso (util:is_duplicate(IDList)=:=false)) of
			   true ->
				   %% 是否拥有该武将
				   case lists:all(fun(E) ->role_lib:has_gerID(E) end, IDList) of
					   true ->
						   %% 位置参数是否错误
						   case lists:all(fun(E) -> is_integer(E) andalso E>=0 andalso E=<6 end, Pos) of
							   true ->
								   true;
							   false ->
								   {false, 2}
						   end;
					   false ->
						   {false, 4}
				   end;
			   false ->
				   {false, 2}
		   end
	end.
						
	
%% 先从随机列表删除已经存在的属性id
get_init_lieutenant(Pos,{SpecialList,AttAddID,HpAddID})->
	SpecialList2 = lists:foldl(fun(E,Acc)->lists:delete(E, Acc) end, SpecialList, get_lieu_spID_list()),
	SpecialID = util:random_one_from_list(SpecialList2),
	{t_lieu,Pos,SpecialID,0,AttAddID,0,HpAddID,0}.

get_lieu_spID_list()->
	lists:foldl(fun(#t_lieu{infoID1=InfoID1}, Acc)->
						[InfoID1|Acc] end, [], role_data:get_lieutenantInfo()).
	
%% ====================================================================
%% Internal functions
%% ====================================================================


take_ger(GerID, PosList, GerBag) ->
	case lists:keytake(GerID, #ger.gerID, PosList) of
		false ->
			case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
				false ->
					false;
				{value, Ger, GerBag2} ->
					{value, Ger, PosList, GerBag2}
			end;
		{value, Ger, PosList2} ->
			{value, Ger, PosList2, GerBag}
	end.

set_ger(Ger, PosList, GerBag) ->
	if is_record(Ger, gerSimple) ->
			{PosList, [Ger|GerBag]};
		true ->
			{[Ger|PosList], GerBag}
	end.

take_ger(GerID, PosList, LPosList, GerBag) ->
	case lists:keytake(GerID, #ger.gerID, PosList) of
		false ->
			case lists:keytake(GerID, #ger.gerID, LPosList) of
				false ->
					case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
						false ->
							false;
						{value, Ger, GerBag2}->
							{value, Ger, PosList, LPosList, GerBag2, bag}
					end;
				{value, Ger, LPosList2}->
					{value, Ger, PosList, LPosList2, GerBag, lieu}
			end;
		{value, Ger, PosList2}->
			{value, Ger, PosList2, LPosList, GerBag, ger}
	end.

set_ger(Ger, PosList, LPosList, GerBag, Type) ->
	case Type of
		bag ->
			{PosList, LPosList, [Ger|GerBag]};
		lieu ->
			{PosList, [Ger|LPosList], GerBag};
		ger ->
			{[Ger|PosList], LPosList, GerBag}
	end.

test(RoleID)->
	role_ger:cs_ger_view_other_dtl(#cs_ger_view_other_dtl{tarRoleID = RoleID}),
	role_ger:cs_ger_view_other(#cs_ger_view_other{tarRoleID=RoleID}).