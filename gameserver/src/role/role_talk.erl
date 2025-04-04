%% @author 李兴龙
%% @doc 聊天处理
%% Created 2013-11-13


-module(role_talk).
-compile(export_all).
-include("def_role.hrl").

%% API functions
-export([]).

%% Internal functions
-export([]).

-define(last_talk_time,last_talk_time).
-define(last_family_talk_time,last_family_talk_time).
-define(last_person_talk_time,last_person_talk_time).

%% ====================================================================
%% API functions
%% ====================================================================

cs_talk_person_offline(_) ->
    case erlang:whereis(talk_server) of
        ?undefined ->
            ?sendself(#sc_talk_person_offline{list=[]});
        _ ->
            erlang:send(talk_server, {get_talk_person_cache, role_data:get_roleID()})
    end.

cs_talk_world(#cs_talk_world{talkMessage=Message, channel=Channel})->
	case check_talk(Message, Channel) of
		{talk_world,Role,NeedGold,Now,IsGag}->
			do_talk_world(Now,Message,Role,NeedGold,IsGag);
        {talk_family, Now, FamilyID, Role} ->
            do_talk_family(Now,Message,Role,FamilyID);
		{false, Reason}->
%%             ?ERR("Reason:~w", [Reason]),
			?sendself(#sc_talk_world{result=Reason, channel=Channel})
	end.

cs_talk_person(#cs_talk_person{roleID=TarRoleID,talkMessage=Message}) ->
    case check_talk(Message, ?CHAT_CHANNEL_PERSON) of
        {talk_person, Now, Role} ->
            do_talk_person(Now,Message,Role,TarRoleID);
        {false, Reason} ->
            ?ERR("Reason:~w", [Reason]),
            ?sendself(#sc_talk_person{result=Reason})  
    end.

cs_talk_gag_one(#cs_talk_gag_one{roleName=RoleName})->
	GagList = role_data:get_gag_list(),
	case lists:member(RoleName, GagList) of
		true ->
			GagList2 = GagList;
		_ ->
			GagList2 = [RoleName|GagList]
	end,
	role_data:set_gag_list(GagList2).

cs_talk_ungag_one(#cs_talk_ungag_one{roleName=RoleName})->
	GagList = role_data:get_gag_list(),
	role_data:set_gag_list(lists:delete(RoleName, GagList)).


cs_talk_get_gag_list(_)->
	?sendself(#sc_talk_get_gag_list{nameList=role_data:get_gag_list()}).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_talk(Message, ?CHAT_CHANNEL_WORLD) ->
	case erlang:length(Message) < data_talk:get(talk_words_limit) * 3 of
		true ->
			Now=util:now(),
			case Now - get_last_talk_time() > data_talk:get(talk_interval_limit) of
				true ->
					#role{level=RoleLevel,vipLevel=VIPLevel}=Role = role_data:get_roleInfo(),
					NeedGold = 
						case  VIPLevel >= data_talk:get(free_vipLevel) of
							true ->
							   0;
						   false ->
							   data_talk:get(talk_need_gold)
						end,
					case RoleLevel >= data_talk:get(talk_need_level) of
						true ->
							case role_lib:check_money(Role, gold, NeedGold) of
								true ->
									{talk_world, Role, NeedGold,Now, check_role_gag()};
								false ->
									{false, 2}
							end;
						_ ->
							{false, 5}
					end;
				_ ->
					{false,3}
			end;
		_ ->
			{false,4}
	end;
check_talk(Message, ?CHAT_CHANNEL_PERSON) ->
    Role = role_data:get_roleInfo(),
    case erlang:length(Message) =< data_family:get(talk_words_limit) * 3 of
        true ->
            Now = util:now(),
            case get_last_person_talk_time() + data_talk:get(talk_person_interval_limit) < Now of
                true ->
                    {talk_person, Now, Role};
                false ->
                    {false, 2}
            end;
        false ->
            {false, 3}
    end;
check_talk(Message, ?CHAT_CHANNEL_FAMILY) ->
    #role{familyID=FamilyID} = Role = role_data:get_roleInfo(),
    case erlang:length(Message) =< data_family:get(talk_words_limit) * 3 of
        true ->
            case FamilyID > 0 of
                true ->
                    Now = util:now(),
                    case get_last_family_talk_time() + data_family:get(talk_interval_limit) < Now of
                        true ->
                            {talk_family, Now, FamilyID, Role};
                        false ->
                            {false, 8}
                    end;
                false ->
                    {false, 7}     
            end;
        false ->
            {false, 4}
    end.

check_role_gag()->
	case talk_server:is_gaged(role_data:get_roleID()) of
		true ->
			true;
		_ ->
			false
	end.


do_talk_world(Now,Message,Role,NeedGold,IsGag)->
    case NeedGold > 0 of
        true ->
            role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_WORLD_TALK, 0, "");
        false ->
            next
    end,
    
    case IsGag of
        false ->
            send_every_one_message(util:words_filter(Message),Now,Role);
        true ->
            next
    end,
    
    set_last_talk_time(Now),
	?CATCH(role_task_trigger:handle({dispach_task,role_chat_times})),
    ?sendself(#sc_talk_world{result=1, channel=?CHAT_CHANNEL_WORLD}).


do_talk_family(Now,Message,Role,FamilyID)->
    send_family_message(FamilyID, util:words_filter(Message),Now,Role),
    set_last_family_talk_time(Now),
	?CATCH(role_task_trigger:handle({dispach_task,role_chat_times})),
    ?sendself(#sc_talk_world{result=1, channel=?CHAT_CHANNEL_FAMILY}).

do_talk_person(Now,Message,Role, TarRoleID) ->
    send_person_message(util:words_filter(Message),Now,Role,TarRoleID),
    set_last_person_talk_time(Now).

get_last_talk_time()->
	case erlang:get(?last_talk_time) of
		?undefined ->
			0;
		X ->
			X
	end.

set_last_talk_time(Time) ->
	erlang:put(?last_talk_time,Time).

get_last_family_talk_time()->
    case erlang:get(?last_family_talk_time) of
        ?undefined ->
            0;
        X ->
            X
    end.

set_last_family_talk_time(Time) ->
    erlang:put(?last_family_talk_time,Time).

get_last_person_talk_time()->
    case erlang:get(?last_person_talk_time) of
        ?undefined ->
            0;
        X ->
            X
    end.

set_last_person_talk_time(Time) ->
    erlang:put(?last_person_talk_time,Time).

send_every_one_message(Message,Now,#role{title=Title,roleName=Name,roleID=RoleID,location=Location,head=Head,isMale=IsMale})->
	Data = #sc_talk_message{channel=?CHAT_CHANNEL_WORLD,roleName=Name,roleTitle=Title,message=Message,timeStamp=Now,roleID=RoleID,familyTitle=0,location=Location,
                            head=Head,isMale=IsMale},
	broadcast_server:bc(Data),
	erlang:send(talk_server, {talk_channel_world, Data}).

send_family_message(FamilyID, Message,Now,#role{title=Title,roleName=Name,roleID=RoleID,location=Location,head=Head,isMale=IsMale})->
    Data = #sc_talk_message{channel=?CHAT_CHANNEL_FAMILY,roleName=Name,roleTitle=Title,message=Message,timeStamp=Now,roleID=RoleID,familyTitle=0,location=Location,
                            head=Head,isMale=IsMale},
    family_misc:router_to_family_process(FamilyID, {bc, Data}),
    erlang:send(talk_server, {talk_channel_family, Data}).

send_person_message(Message,Now,#role{title=Title,roleName=Name,roleID=RoleID,location=Location,head=Head,isMale=IsMale},TarRoleID)->
    Data = #sc_talk_message{channel=?CHAT_CHANNEL_PERSON,roleName=Name,roleTitle=Title,message=Message,timeStamp=Now,roleID=RoleID,familyTitle=0,location=Location,
                            head=Head,isMale=IsMale},
    ?CATCH(erlang:send(friend_server, {talk_person, RoleID, TarRoleID, Data})).








