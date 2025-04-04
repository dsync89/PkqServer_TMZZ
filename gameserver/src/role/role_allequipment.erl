%% @author admin
%% @doc 套装功能


-module(role_allequipment).


-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-record(data_all_equipment,{
						all_equipment_id,         %%套装id｛10001｝套装id为唯一标示，不能重复
						all_equipment_name,   %%套装名字｛狂战士套装｝
						all_equipment_part_list, 
						all_equipment_stat_list
							}).


-define(ROLE_ALL_EQUIPMENT_INFO_LIST,role_all_equipment_info_list).%%玩家的套装信息
-define(ROLE_ALL_EQUIPMENT_ADD_ATTR_LIST,role_all_equipment_add_attr_list).%%玩家套装增加的属性
%% ====================================================================
%% Internal functions
%% ====================================================================

init_all_equipment(GerID,RoleEquipList)->
	?DEBUG("======>>>>>~w",[RoleEquipList]),
	case RoleEquipList of
		?undefined->
			set_ger_all_equipment_info_list(GerID,[]),
			set_ger_all_equipment_add_attr_list(GerID,[]);
		_->
			case length(RoleEquipList) >= 2 of
				true->
					AllEquipmentIDList = data_all_equipment:get_list(),
					{GerAllEquipmentInfoList,GerAllEquipmentAddAttrList} = lists:foldl(fun(AllEquipmentID,{InfoAcc,AttrAcc}=Acc)->
																							   #data_all_equipment{all_equipment_part_list=AllEquipmentPartList,all_equipment_stat_list=AllEquimentStatList}= data_all_equipment:get(AllEquipmentID),
																							   AllEquipmentEquipList = lists:filter(fun(AllEquipmentPart)->
																																			case lists:keyfind(AllEquipmentPart,#item.itemTypeID,RoleEquipList) of
																																				false->
																																					false;
																																				_->
																																					true
																																			end
																																	end,AllEquipmentPartList),
																							   Num = length(AllEquipmentEquipList),
																							   ?DEBUG("==================~w",[{AllEquipmentID,Num}]),
																							   case Num>=2 of
																								   true->
																									   ?CATCH(role_task_trigger:handle({dispach_task,role_allequipment,Num})),
																									   AllEquipmentAddAttrList = lists:sublist(AllEquimentStatList, Num-1),
																									   {[#p_all_equipment{all_equipment_id=AllEquipmentID,all_equipment_list=AllEquipmentEquipList}|InfoAcc],
																										lists:append(AllEquipmentAddAttrList, AttrAcc)};
																								   _->
																									   Acc
																							   end
																					   end, {[],[]}, AllEquipmentIDList),
					set_ger_all_equipment_info_list(GerID,GerAllEquipmentInfoList),
					set_ger_all_equipment_add_attr_list(GerID,GerAllEquipmentAddAttrList);
				%% 			?sendself(#sc_item_all_equipment{gerID=GerID,all_equipment_info_list=GerAllEquipmentInfoList});
				false->
					set_ger_all_equipment_info_list(GerID,[]),
					set_ger_all_equipment_add_attr_list(GerID,[])
			%% 			?sendself(#sc_item_all_equipment{gerID=GerID,all_equipment_info_list=[]})
			end
	end.

set_ger_all_equipment_info_list(GerID,RoleAllEquipmentInfoList) when is_list(RoleAllEquipmentInfoList)->
			put({?ROLE_ALL_EQUIPMENT_INFO_LIST,GerID},RoleAllEquipmentInfoList).

get_ger_all_equipment_info_list(GerID)->
			case get({?ROLE_ALL_EQUIPMENT_INFO_LIST,GerID}) of
					?undefined->
							[];
					InfoL->
							InfoL
			end.

set_ger_all_equipment_add_attr_list(GerID,RoleAllEquipmentAddAttrList) when is_list(RoleAllEquipmentAddAttrList)->
			put({?ROLE_ALL_EQUIPMENT_ADD_ATTR_LIST,GerID},RoleAllEquipmentAddAttrList).

get_ger_all_equipment_add_attr_list(GerID)->
			case get({?ROLE_ALL_EQUIPMENT_ADD_ATTR_LIST,GerID}) of
					?undefined->
							[];
					InfoL->
							InfoL
			end.







