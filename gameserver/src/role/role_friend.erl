%% @author crimoon26
%% @doc @todo Add description to role_enargy.


-module(role_friend).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-compile(export_all).
-include("def_role.hrl").
%% ====================================================================
%% Internal functions
%% ====================================================================

cs_friend_give_enargy(#cs_friend_give_enargy{roleIDList=RoleIDList})->
	case check_lock() of
		unlock->
			#roleTimes{energy=Enargy} = role_data:get_roleTimes(),
			case role_data:get_roleInfo() of
				#role{vipLevel=VipLevel} ->
					next;
				_ ->
					VipLevel = 0
			end,
			MaxEnargy = role_lib:get_max_energy(VipLevel),
			NewTimes = MaxEnargy - Enargy,
			case NewTimes>0 of
				true->
					RoleID = role_data:get_roleID(),
					put(give_enagy_lock,util:now() + 2),
					enargy_server:enargy_give(RoleID, lists:sublist(RoleIDList,NewTimes));
				_->
					Record = #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=1},
					?sendself(Record)
			end;
		_->
			ignore
	end.

cs_friend_give_all_enargy(_)->
	case check_lock() of
		unlock->
			#roleTimes{energy=Enargy} = role_data:get_roleTimes(),
			case role_data:get_roleInfo() of
				#role{vipLevel=VipLevel} ->
					next;
				_ ->
					VipLevel = 0
			end,
			MaxEnargy = role_lib:get_max_energy(VipLevel),
			NewTimes = MaxEnargy - Enargy,
			case NewTimes>0 of
				true->
					RoleID = role_data:get_roleID(),
					#friend_enargy{toMeList=ToMeList} = enargy_server:get_ets_friend_enargy(RoleID),
					RoleIDList = [R||{R,_}<-lists:sublist(ToMeList, NewTimes)],
					case RoleIDList of
						[]->
							Record = #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=3},
							?sendself(Record);
						_->
							put(give_enagy_lock,util:now() + 2),
							enargy_server:enargy_give(RoleID, RoleIDList)
					end;
				_->
					Record = #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=1},
					?sendself(Record)
			end;
		_->
			ignore
	end.

enargy_give_reward({enargy_give_reward,AddEnargy})->
	erlang:erase(give_enagy_lock),
	#roleTimes{energy=Energy,lastEnergyTime=LastEnergyTime} = RoleTimes = role_data:get_roleTimes(),
	EInterval = role_lib:to_sec(data_common:get(energy_recover_interval)),
	NextTick = LastEnergyTime + EInterval,
	NewEnergy = Energy+AddEnargy,
	?notify_update(?ra_energy(NewEnergy, NextTick)),
	RoleTimes2 = RoleTimes#roleTimes{energy=NewEnergy},
	role_data:set_roleTimes(RoleTimes2).


check_lock()->
	case get(give_enagy_lock) of
		?undefined->
			unlock;
		T->
			case T > util:now() of
				true->
					lock;
				false->
					unlock
			end
	end.

do_friend_fight({do_friend_fight, TarRoleID}) ->
    {Result, FightRecord, _State} = role_lib:pvp(TarRoleID),
    case Result of
        true ->
            {AddCoin, AddRep} = data_common:get(friend_fight_win),
            ArgID = 1;
        false ->
            {AddCoin, AddRep} = data_common:get(friend_fight_lose),
            ArgID = 0
    end,
    RoleInfo2 = role_lib:add_reputation_f(role_data:get_roleInfo(), AddRep,?MONEY_ADD_TYPE_FRIEND_PVP,ArgID,""),
    role_lib:add_coin_f(RoleInfo2, AddCoin, ?MONEY_ADD_TYPE_FRIEND_PVP, ArgID, ""),
    ?sendself(#sc_friend_fight{result=0,addCoin=AddCoin, addRepu=AddRep, fightInfo=[FightRecord]}).

