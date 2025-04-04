-module(role_rank).
-compile(export_all).

-include("def_role.hrl").


cs_rank_info(#cs_rank_info{rankType=?RANK_TYPE_LEVEL}=Record) ->
    erlang:send(rank_server, {client_msg, role_data:get_roleID(), Record});
cs_rank_info(#cs_rank_info{rankType=?RANK_TYPE_FIGHT_POWER}=Record) ->
    erlang:send(rank_server, {client_msg, role_data:get_roleID(), Record});
cs_rank_info(#cs_rank_info{rankType=?RANK_TYPE_PVP}=Record) ->
    erlang:send(pvp_server, {client_msg, role_data:get_roleID(), Record});
cs_rank_info(#cs_rank_info{rankType=?RANK_TYPE_BATTLE}=Record) ->
    erlang:send(battle_rank_server, {client_msg, role_data:get_roleID(), Record}).