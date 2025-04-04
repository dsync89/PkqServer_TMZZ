%% @author admin
%% @doc 虎牢关 玩家处理模块
%% Created 2013-5-6


-module(role_hula).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
%% API functions
-export([]).

%% Internal functions 
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% 请求战斗
cs_hula_fight(#cs_hula_fight{}) ->
	hula_server:fight(role_data:get_roleID(),role_data:get_fighter_list(), role_data:get_lieu_add_attr()).


%% 请求复活
cs_hula_reborn(_) ->
    #role{roleID=RoleID} = Role = role_data:get_roleInfo(),
    NeedGold = data_hula:get(recover_gold),
    case role_lib:check_money(Role, gold, NeedGold) of
        true ->
            case hula_server:is_state_begin() of
                true ->
                    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_HULA_REBORN, 0, ""),
                    hula_server:reborn(RoleID, role_data:get_fighter_list(), role_data:get_lieu_add_attr());
                false ->
                    ?sendself(#sc_hula_reborn{result=3})
            end;
        false ->
            ?sendself(#sc_hula_reborn{result=2})
    end.


