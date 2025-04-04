%% @author admin
%% @doc 图鉴功能
%% Created 2013-5-30


-module(role_gather).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([hook_add_item_list/1,hook_add_ger_list/1,hook_add_equip_list/1]).

%% Internal functions
-export([]).

-define(SHAPE_1, 1).
-define(SHAPE_2, 2).
-define(SHAPE_3, 3).

-define(QUALITY_TO_SHAPE_3, 20).
-define(QUALITY_TO_SHAPE_2, 10).

%% ====================================================================
%% API functions
%% ====================================================================
cs_gather_get_list(#cs_gather_get_list{type=Type}) ->
	if Type =:= ?GATHER_TYPE_GER orelse Type =:= ?GATHER_TYPE_ITEM orelse Type =:= ?GATHER_TYPE_EQUIP ->
		   List = role_data:get_gatherInfo(Type),
		   ?sendself(#sc_gather_get_list{type=Type,idList=List});
	   true ->
		   ?sendself(#sc_gather_get_list{type=Type,idList=[]})
		   end.


%% ====================================================================
%% Internal functions
%% ====================================================================
hook_add_item_list(ItemTypeIDList) ->
	hook_gather(?GATHER_TYPE_ITEM, ItemTypeIDList).

hook_add_ger_list(GerList) ->
    GerTypeIDList = lists:map(fun({GerTypeID, GerQuality}) ->
                                  ?SHAPE_BASE * quality_to_shape(GerQuality) + GerTypeID
                              end, GerList),
	hook_gather(?GATHER_TYPE_GER, GerTypeIDList).

quality_to_shape(GerQuality) when GerQuality >= ?QUALITY_TO_SHAPE_3 ->
    ?SHAPE_3;
quality_to_shape(GerQuality) when GerQuality >= ?QUALITY_TO_SHAPE_2 ->
    ?SHAPE_2;
quality_to_shape(_GerQuality) ->
    ?SHAPE_1.

hook_add_equip_list(EquipTypeIDList) ->
    hook_gather(?GATHER_TYPE_EQUIP, EquipTypeIDList).

hook_gather(Type, TypeIDList) ->
	GatherSet = role_data:get_gatherInfo(Type),
	{GatherSet2, NewList} =
	lists:foldl(fun(E, {L,NL}=Acc) ->
						case lists:member(E,L) of
							true ->
								Acc;
							false ->
								{[E|L],[E|NL]}
						end
				end, {GatherSet,[]}, TypeIDList),
	if NewList =/= [] ->		   
		   role_data:set_gatherInfo(Type, GatherSet2),		
		   ?sendself(#sc_gather_new{type=Type,newIDList=NewList}),
		   db_sql:add_gatherInfo(role_data:get_roleID(), Type, NewList);
	   true ->
		   ignore
	end.
						