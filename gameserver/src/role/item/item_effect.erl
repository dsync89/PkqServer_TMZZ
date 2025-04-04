%% @author admin
%% @doc 功能道具
%% Created 2013-4-10


-module(item_effect).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

add_times(ItemTypeID, Num) ->
    case catch add_times2(ItemTypeID, Num) of
        ok ->
            {ok, Num};
        {false, Reason} ->
            {false, Reason}
    end.

add_times2(ItemTypeID, Num) ->
    #data_item_use{addType=AddType,addValue=AddValue,maxTimes=MaxTimes} = data_item_use:get(ItemTypeID),
    ItemUseList = role_data:get_itemUseList(),
    #item_use_info{useTimes=UseTimes} = ItemUseInfo = lists:keyfind(ItemTypeID, #item_use_info.itemTypeID, ItemUseList),
    NewUseTimes = UseTimes + Num,
    case NewUseTimes > MaxTimes of
        true ->
            erlang:throw({false, 4});
        false ->
            next
    end,
    NowValue = get_spec_type_value(AddType),
    MaxValue = get_spec_type_max_value(AddType),
    case NowValue < MaxValue of
        true ->
            next;
        false ->
            erlang:throw({false, 5})
    end,
    NewValue = erlang:min(NowValue + AddValue * Num, MaxValue),
    update_spec_type_value(AddType, NewValue),
    NewItemUseInfo = ItemUseInfo#item_use_info{useTimes=NewUseTimes},
    NewItemUseList = lists:keyreplace(ItemTypeID, #item_use_info.itemTypeID, ItemUseList, NewItemUseInfo),
    role_data:set_itemUseList(NewItemUseList),
    ?sendself(#sc_item_use_info{use_info_list=[#p_item_use_info{type_id=ItemTypeID,left_times=MaxTimes-NewUseTimes}]}),
    ok.

get_spec_type_value(Type) ->
    RoleTimes = role_data:get_roleTimes(),
    case Type of
        energy ->
            RoleTimes#roleTimes.energy;
        dscv ->
            RoleTimes#roleTimes.discoveryTimes;
        pvp ->
            RoleTimes#roleTimes.pvpTimes;
        rule ->
            RoleTimes#roleTimes.ruleTimes
    end.

update_spec_type_value(Type, NewValue) ->
    RoleTimes = role_data:get_roleTimes(),
    case Type of
        energy ->
            role_data:set_roleTimes(RoleTimes#roleTimes{energy=NewValue}),
            IntervalSeconds = role_lib:to_sec(data_common:get(energy_recover_interval)),
            ?notify_update(?ra_energy(NewValue, IntervalSeconds + RoleTimes#roleTimes.lastEnergyTime));
        dscv ->
            role_data:set_roleTimes(RoleTimes#roleTimes{discoveryTimes=NewValue}),
            IntervalSeconds = role_lib:to_sec(data_common:get(dscv_recover_interval)),
            ?notify_update(?ra_dscv(NewValue, IntervalSeconds + RoleTimes#roleTimes.lastDscvTime));
        pvp ->
            role_data:set_roleTimes(RoleTimes#roleTimes{pvpTimes=NewValue}),
            IntervalSeconds = role_lib:to_sec(data_common:get(pvp_recover_interval)),
            ?notify_update(?ra_pvpTimes(NewValue, IntervalSeconds + RoleTimes#roleTimes.lastPvpTime));
        rule ->
            role_data:set_roleTimes(RoleTimes#roleTimes{ruleTimes=NewValue}),
            IntervalSeconds = role_lib:to_sec(data_common:get(rule_recover_interval)),
            ?notify_update(?ra_ruleTimes(NewValue, IntervalSeconds + RoleTimes#roleTimes.lastRuleTime))
    end.

get_spec_type_max_value(Type) ->
    case Type of
        energy ->
            #role{vipLevel=VipLevel} = role_data:get_roleInfo(),
            role_lib:get_max_energy(VipLevel);
        dscv ->
            #role{vipLevel=VipLevel} = role_data:get_roleInfo(),
            role_lib:get_max_dscv_times(VipLevel);
        pvp ->
            data_common:get(max_pvp_times);
        rule ->
            data_common:get(max_rule_times)
    end.






%% ====================================================================
%% Internal functions
%% ====================================================================

