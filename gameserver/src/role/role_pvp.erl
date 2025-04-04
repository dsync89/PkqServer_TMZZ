%% @author admin
%% @doc 玩家pvp
%% Created 2013-4-8


-module(role_pvp).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_pvp_fight(Record) ->
	PVPTimes = role_data:get_pvp_times(),
	RoleID = role_data:get_roleID(),
	case PVPTimes >= 1 of
		true ->
			erlang:send(pvp_server, {fight,RoleID, Record});
		false ->
%% 			?sendself(#sc_pvp_fight{fightInfo=[],newRank=0,result=3,reward=[]})
			?sendself(#sc_pvp_fight{fightInfo=[],newRank=0,result=3,addRepu=0})
	end.

get_pvp(RoleInfo, RoleFighterPower) ->
	#p_pvp{fightPower=RoleFighterPower,
		   isMale=(RoleInfo#role.isMale),
		   roleID=RoleInfo#role.roleID,
		   level=RoleInfo#role.level,
		   title=RoleInfo#role.title,
		   roleName=RoleInfo#role.roleName,
		   head=RoleInfo#role.head}.
		   

do_pvp_fight({do_pvp_fight, TarRoleID}) ->
	{Result, FightRecord, _State} = role_lib:pvp(TarRoleID),
	RoleInfo = role_data:get_roleInfo(),
	role_data:dec_pvp_times(),
	RoleFightPower = role_data:get_roleFightPower(),
	PVP = get_pvp(RoleInfo, RoleFightPower),
	%%刷新目标TarRole的信息
	case role_lib:get_rolePublic(TarRoleID) of
		#rolePublic{} = RolePublic ->
			TarPVP = #p_pvp{
			fightPower=RolePublic#rolePublic.fightPower,
		   	isMale=RolePublic#rolePublic.isMale,
		   	roleID=RolePublic#rolePublic.roleID,
		   	level=RolePublic#rolePublic.level,
		   	title=RolePublic#rolePublic.title,
		   	roleName=RolePublic#rolePublic.roleName,
			head=RolePublic#rolePublic.head};
		_ ->
			TarPVP = []
	end,
	#role{roleID=RoleID,roleName=RoleName} = RoleInfo,
	case Result of
		true ->
			AddRep = data_common:get(pvp_fight_win_add_rep),
			erlang:send(pvp_server, {fight_win, FightRecord, RoleID,TarRoleID, PVP, TarPVP,AddRep});
		false ->
			AddRep = data_common:get(pvp_fight_fail_add_rep),
			erlang:send(pvp_server, {fight_fail, FightRecord, RoleID,
									 RoleName, TarRoleID, PVP, TarPVP,AddRep})
	end,
	role_lib:add_reputation_f(RoleInfo, AddRep,?MONEY_ADD_TYPE_PVP_FIGHT,0,"").

cs_pvp_get_list(_) ->
    case catch gen_server:call(pvp_server, {pvp_get_list,role_data:get_roleID()}) of
        {ok, Record} ->
            ?sendself(Record);
        Err ->
            ?ERR("~w", [Err]),
            ?sendself(#sc_pvp_get_list{pvpList=[],rank=0})
    end.
	
%% ====================================================================
%% Internal functions
%% ====================================================================


