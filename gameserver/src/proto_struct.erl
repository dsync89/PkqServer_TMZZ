-module(proto_struct).
-compile(export_all).

string(Str) ->
	Str2 = iolist_to_binary(Str),
	[<<(byte_size(Str2)):16>>, Str2].

-define(int8(V), <<V:8>>).
-define(int16(V), <<V:16>>).
-define(int32(V), <<V:32>>).
-define(int64(V), <<V:64>>).
-define(bool(V), (case V of true -> <<1:8>>; false -> <<0:8>> end)).
-define(string(V), (string(V))).
-define(tuple(V), (encode_def(element(1,V),V))).
-define(any(V), (if is_integer(V) ->
						if abs(V) < 128 ->
							   [<<241:8>>,?int8(V)];
						   abs(V) < 32768 ->
							   [<<242:8>>,?int16(V)];
						   abs(V) < 2147483648 ->
							   [<<243:8>>,?int32(V)];
						   true ->
							   [<<244:8>>,?int64(V)]
						end;
					is_boolean(V) ->
						[<<245:8>>,?bool(V)];
					is_list(V) orelse is_binary(V) ->
						[<<246:8>>,?string(V)];
					true ->
						[<<(get_id(element(1,V))):16>>,?tuple(V)]
				 end)).

-define(list_int8(List),	[<<(length(List)):16>>, [ ?int8(E) || E<-List ] ]).
-define(list_int16(List),	[<<(length(List)):16>>, [ ?int16(E) || E<-List ]]).
-define(list_int32(List), 	[<<(length(List)):16>>, [ ?int32(E) || E<-List ]]).
-define(list_int64(List), 	[<<(length(List)):16>>, [ ?int64(E) || E<-List ]]).
-define(list_bool(List), 	[<<(length(List)):16>>, [ ?bool(E) || E <- List ]]).
-define(list_string(List),	[<<(length(List)):16>>, [ ?string(E) || E<-List ]]).
-define(list_tuple(List),	[<<(length(List)):16>>, [ ?tuple(E) || E<-List ]]).
-define(list_any(List),		[<<(length(List)):16>>, [ ?any(E) || E<-List ]]).

encode_def(cs_melee_info, R)->
	{_}=R,
	[];
encode_def(sc_melee_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int8(V3),?int32(V4),?int32(V5),?int32(V6),?int16(V7),?int8(V8),?int8(V9)];
encode_def(cs_melee_sign, R)->
	{_}=R,
	[];
encode_def(sc_melee_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_melee_fight, R)->
	{_}=R,
	[];
encode_def(p_id_num, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(p_mail_reward, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(p_action, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int8(V3),?list_int8(V4),?int8(V5),?int32(V6),?int8(V7)];
encode_def(p_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int64(V2),?int16(V3),?int8(V4),?int64(V5),?int64(V6),?int32(V7),?int8(V8),?int16(V9)];
encode_def(sc_fight_request, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?bool(V4)];
encode_def(sc_melee_fight, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int8(V2),?int64(V3),?string(V4),?int16(V5),?int32(V6),?int32(V7),?tuple(V8),?list_tuple(V9),?int8(V10)];
encode_def(p_homestead_log, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?string(V2),?int8(V3),?int8(V4),?string(V5),?int8(V6),?int32(V7)];
encode_def(p_homestead_machine, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int16(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int8(V8)];
encode_def(p_homestead, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?string(V2),?int8(V3),?int8(V4),?int32(V5),?int8(V6),?int16(V7),?int64(V8),?int16(V9),?int16(V10),?int32(V11)];
encode_def(sc_homestead_error, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_homestead_get_info, R)->
	{_}=R,
	[];
encode_def(sc_homestead_get_info, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?list_tuple(V3)];
encode_def(cs_homestead_get_friend_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_homestead_get_friend_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?tuple(V3),?list_tuple(V4)];
encode_def(cs_homestead_unlock_machine, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_homestead_unlock_machine, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_homestead_uproot_seed, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_homestead_uproot_seed, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_homestead_harvest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_reward_view, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_homestead_harvest, R)->
	{_,V2,V3}=R,
	[?tuple(V2),?tuple(V3)];
encode_def(cs_homestead_seeding, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_homestead_seeding, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_homestead_update_machine, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_homestead_change_ger, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_homestead_change_ger, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(cs_homestead_mating, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_homestead_mating, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int8(V6)];
encode_def(sc_homestead_mating_to_friend, R)->
	{_,V2,V3,V4}=R,
	[?tuple(V2),?int32(V3),?int8(V4)];
encode_def(cs_homestead_addenergy, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_homestead_addenergy, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int8(V3),?int8(V4),?int32(V5),?int32(V6),?list_tuple(V7)];
encode_def(sc_homestead_addenergy_to_friend, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?tuple(V3),?int32(V4),?int32(V5)];
encode_def(cs_homestead_get_log, R)->
	{_}=R,
	[];
encode_def(sc_homestead_get_log, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_homestead_get_friend_log, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_homestead_get_friend_log, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(sc_homestead_sync_mating_cool_second, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_homestead_sync_ger, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int16(V4)];
encode_def(sc_homestead_sync_add_enagy, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(p_task, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int8(V3),?int32(V4)];
encode_def(cs_task_get_info, R)->
	{_}=R,
	[];
encode_def(sc_task_get_info, R)->
	{_,V2,V3,V4}=R,
	[?list_tuple(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_task_operate, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_task_operate, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int8(V4),?list_tuple(V5)];
encode_def(sc_task_error, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_task_notify_change, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_int32(V4)];
encode_def(cs_fight_request, R)->
	{_}=R,
	[];
encode_def(cs_road_info, R)->
	{_}=R,
	[];
encode_def(sc_road_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5)];
encode_def(cs_road_reset, R)->
	{_}=R,
	[];
encode_def(sc_road_reset, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_road_fight, R)->
	{_}=R,
	[];
encode_def(sc_road_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?bool(V3),?int8(V4),?list_tuple(V5),?list_tuple(V6)];
encode_def(cs_road_fight_ext, R)->
	{_}=R,
	[];
encode_def(sc_road_fight_ext, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?bool(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_road_box, R)->
	{_}=R,
	[];
encode_def(sc_road_box, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_hula_open, R)->
	{_}=R,
	[];
encode_def(p_hula_boss_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int64(V3),?int16(V4),?string(V5),?bool(V6),?int8(V7),?int32(V8)];
encode_def(sc_hula_open, R)->
	{_,V2,V3,V4}=R,
	[?bool(V2),?list_tuple(V3),?int32(V4)];
encode_def(p_hula_boss_state, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int64(V3),?int64(V4),?string(V5),?bool(V6),?int8(V7),?int32(V8)];
encode_def(sc_hula_init_state, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?list_tuple(V2),?int64(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_hula_close, R)->
	{_}=R,
	[];
encode_def(p_hula_info, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(cs_hula_cur_info, R)->
	{_}=R,
	[];
encode_def(sc_hula_cur_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_hula_hp_sync, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_hula_harm, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(sc_hula_harm_broadcast, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_role_stastic, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int32(V3),?int32(V4)];
encode_def(sc_hula_stop, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?tuple(V4)];
encode_def(cs_hula_rank_sync, R)->
	{_}=R,
	[];
encode_def(sc_hula_rank_sync, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_hula_fight, R)->
	{_}=R,
	[];
encode_def(sc_hula_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_hula_reborn, R)->
	{_}=R,
	[];
encode_def(sc_hula_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_hula_open_time, R)->
	{_}=R,
	[];
encode_def(sc_hula_open_time, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_festival_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_festival_rank_reward, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?tuple(V4)];
encode_def(sc_festival_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int16(V8),?int16(V9),?int16(V10),?string(V11),?list_tuple(V12)];
encode_def(cs_festival_click, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_festival_click, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int8(V3),?int8(V4),?int16(V5),?int32(V6),?int32(V7),?int32(V8),?list_tuple(V9)];
encode_def(cs_festival_rank, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int16(V3),?int16(V4)];
encode_def(p_festival_rank, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?bool(V3),?int8(V4),?int32(V5),?int16(V6),?string(V7),?int32(V8),?int16(V9)];
encode_def(sc_festival_rank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int8(V3),?int16(V4),?list_tuple(V5)];
encode_def(cs_rule_info, R)->
	{_}=R,
	[];
encode_def(p_rule_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int32(V10),?int16(V11),?int16(V12),?int16(V13),?int16(V14)];
encode_def(p_hist, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int64(V2),?int8(V3),?string(V4),?int32(V5),?int32(V6),?int16(V7),?int32(V8)];
encode_def(sc_rule_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?int16(V4),?int16(V5),?int16(V6),?int16(V7),?tuple(V8),?list_tuple(V9)];
encode_def(cs_rule_rank, R)->
	{_}=R,
	[];
encode_def(sc_rule_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_rule_last_rank, R)->
	{_}=R,
	[];
encode_def(sc_rule_last_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_rule_fight, R)->
	{_}=R,
	[];
encode_def(sc_rule_fight, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int8(V2),?bool(V3),?string(V4),?int32(V5),?int32(V6),?int16(V7),?int16(V8),?int16(V9),?int16(V10),?tuple(V11),?tuple(V12),?list_tuple(V13)];
encode_def(cs_rule_leave, R)->
	{_}=R,
	[];
encode_def(cs_rank_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5)];
encode_def(p_rank_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?int32(V3),?int16(V4),?string(V5),?int32(V6),?int8(V7),?bool(V8),?int64(V9),?int16(V10),?string(V11)];
encode_def(sc_rank_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_alien_info, R)->
	{_}=R,
	[];
encode_def(p_alien_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int16(V10),?int8(V11),?int32(V12)];
encode_def(sc_alien_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?bool(V2),?int32(V3),?int8(V4),?bool(V5),?int8(V6),?list_tuple(V7),?int32(V8),?int16(V9),?int8(V10),?int8(V11)];
encode_def(sc_alien_sign_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?bool(V4),?int32(V5)];
encode_def(cs_alien_first_five, R)->
	{_}=R,
	[];
encode_def(sc_alien_first_five, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_kill_num_rank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_alien_fighter2, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int16(V10),?int16(V11)];
encode_def(sc_alien_kill_num_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_kill_continuous_rank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_alien_fighter3, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int16(V10),?int16(V11),?bool(V12)];
encode_def(sc_alien_kill_continuous_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_guess_info, R)->
	{_}=R,
	[];
encode_def(sc_alien_guess_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?bool(V3),?int32(V4),?int32(V5),?list_int32(V6)];
encode_def(cs_alien_guess, R)->
	{_,V2,V3}=R,
	[?int32(V2),?bool(V3)];
encode_def(sc_alien_guess, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_alien_reset, R)->
	{_}=R,
	[];
encode_def(sc_alien_reset, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4)];
encode_def(cs_alien_fight, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_alien_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4),?int32(V5),?list_tuple(V6)];
encode_def(cs_alien_sign, R)->
	{_}=R,
	[];
encode_def(sc_alien_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_alien_self_record, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_alien_self_record, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?bool(V2),?bool(V3),?string(V4),?int16(V5),?int64(V6),?int32(V7)];
encode_def(sc_alien_self_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_record, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(p_alien_record, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?string(V3),?string(V4),?int16(V5),?int16(V6),?int64(V7),?int32(V8)];
encode_def(sc_alien_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_alien_update_times, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_alien_self_fight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_alien_self_fight_replay, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_alien_fight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_alien_fight_repaly, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_alien_new_fighter_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_alien_leave, R)->
	{_}=R,
	[];
encode_def(sc_alien_new_self_record, R)->
	{_}=R,
	[];
encode_def(cs_alien_view_other, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_ger_view, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int16(V4)];
encode_def(sc_alien_view_other, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6),?int32(V7),?string(V8),?int16(V9),?int64(V10),?list_tuple(V11)];
encode_def(cs_alien_view_other_dtl, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_alien_buy_times, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_alien_buy_times, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_alien_active, R)->
	{_}=R,
	[];
encode_def(sc_alien_active, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_team_pk_info, R)->
	{_}=R,
	[];
encode_def(p_team_member, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8)];
encode_def(sc_team_pk_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int64(V2),?int32(V3),?int32(V4),?int8(V5),?int8(V6),?list_tuple(V7),?list_tuple(V8),?int32(V9)];
encode_def(sc_team_pk_close, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int64(V2),?int32(V3),?int32(V4),?int32(V5),?list_tuple(V6)];
encode_def(cs_team_refresh, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_team_refresh, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_team_fight, R)->
	{_}=R,
	[];
encode_def(p_team_member2, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?bool(V9)];
encode_def(sc_team_fight_result, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?bool(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7),?int8(V8),?int8(V9),?list_tuple(V10),?list_tuple(V11),?list_tuple(V12),?list_tuple(V13),?list_tuple(V14)];
encode_def(cs_team_rank, R)->
	{_}=R,
	[];
encode_def(p_team_member3, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?int64(V3),?bool(V4),?int8(V5),?int32(V6),?int16(V7),?string(V8),?int32(V9),?int32(V10)];
encode_def(sc_team_rank, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_team_record, R)->
	{_}=R,
	[];
encode_def(p_team_record, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?bool(V2),?int32(V3),?string(V4),?string(V5),?list_int64(V6)];
encode_def(sc_team_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_team_self_record, R)->
	{_}=R,
	[];
encode_def(p_team_self_record, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?bool(V3),?int32(V4),?int32(V5),?int32(V6),?list_string(V7),?list_string(V8),?list_int64(V9)];
encode_def(sc_team_self_record, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_team_move, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_team_move, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_team_pk_not_open, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_team_fight_error, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_team_fight_replay, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_team_fight_replay, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_team_self_fight_replay, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_team_self_fight_replay, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?list_tuple(V5)];
encode_def(cs_team_view_other, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_team_view_other, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6)];
encode_def(cs_team_view_other_dtl, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_lieu_view, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_equip, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int64(V2),?int16(V3),?int8(V4),?int8(V5),?int64(V6),?int8(V7),?int32(V8),?int16(V9)];
encode_def(p_ger_pos, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(p_ger, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int16(V5),?int32(V6),?int32(V7),?int64(V8),?int64(V9)];
encode_def(sc_team_view_other_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6),?list_tuple(V7),?list_tuple(V8),?int16(V9),?int16(V10),?list_tuple(V11)];
encode_def(cs_team_new_status, R)->
	{_}=R,
	[];
encode_def(sc_team_new_status, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(p_race_rec, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17}=R,
	[?string(V2),?string(V3),?list_int64(V4),?int32(V5),?int32(V6),?int64(V7),?int64(V8),?int8(V9),?int8(V10),?bool(V11),?bool(V12),?int8(V13),?int8(V14),?int32(V15),?int32(V16),?list_bool(V17)];
encode_def(sc_race_new_fight, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(p_race_fighter, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?string(V3),?int64(V4),?int16(V5),?bool(V6),?int8(V7),?int32(V8)];
encode_def(cs_race_history, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5)];
encode_def(sc_race_history, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_race_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_race_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_race_fight_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_race_fight_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_race_sign, R)->
	{_}=R,
	[];
encode_def(sc_race_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race_info, R)->
	{_}=R,
	[];
encode_def(p_race_pos, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?string(V3),?bool(V4),?int8(V5),?int32(V6),?int8(V7)];
encode_def(sc_race_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int8(V2),?int32(V3),?int16(V4),?bool(V5),?list_tuple(V6),?string(V7),?int8(V8),?bool(V9)];
encode_def(cs_race_enter, R)->
	{_}=R,
	[];
encode_def(cs_race_leave, R)->
	{_}=R,
	[];
encode_def(cs_race_pos_history, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_race_pos_history, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_race_new_first, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(sc_race_new_status, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_race_is_open, R)->
	{_}=R,
	[];
encode_def(sc_race_is_open, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(cs_race_auto_sign, R)->
	{_}=R,
	[];
encode_def(sc_race_auto_sign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race_auto_unsign, R)->
	{_}=R,
	[];
encode_def(sc_race_auto_unsign, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_race_self_history, R)->
	{_}=R,
	[];
encode_def(sc_race_self_history, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_race_guess_info, R)->
	{_}=R,
	[];
encode_def(sc_race_guess_info, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?list_int32(V4)];
encode_def(cs_race_guess, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(sc_race_guess, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_family_member_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12}=R,
	[?int32(V2),?string(V3),?int32(V4),?int32(V5),?int32(V6),?int8(V7),?bool(V8),?bool(V9),?int16(V10),?int64(V11),?int8(V12)];
encode_def(p_family_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14}=R,
	[?int32(V2),?string(V3),?int16(V4),?int32(V5),?string(V6),?int32(V7),?string(V8),?int16(V9),?int32(V10),?string(V11),?list_tuple(V12),?int32(V13),?int32(V14)];
encode_def(p_family_request, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?int32(V6),?int32(V7)];
encode_def(p_family_summary, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int32(V2),?string(V3),?string(V4),?int16(V5),?int16(V6),?int32(V7),?string(V8),?bool(V9),?int32(V10)];
encode_def(cs_family_get_list, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(sc_family_get_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_family_create, R)->
	{_,V2,V3}=R,
	[?string(V2),?bool(V3)];
encode_def(sc_family_create, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?int32(V4)];
encode_def(cs_family_request_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_request_join, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?int32(V4)];
encode_def(cs_family_cancel_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_cancel_join, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(cs_family_agree_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_agree_join, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?tuple(V4)];
encode_def(cs_family_refuse_join, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_refuse_join, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(cs_family_get_info, R)->
	{_}=R,
	[];
encode_def(sc_family_get_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_family_kick, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_family_kick, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?tuple(V4)];
encode_def(cs_family_create_consume, R)->
	{_}=R,
	[];
encode_def(sc_family_create_consume, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int32(V3)];
encode_def(cs_family_leave, R)->
	{_}=R,
	[];
encode_def(sc_family_leave, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?tuple(V4)];
encode_def(cs_family_change_notice, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_family_change_notice, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?bool(V3),?string(V4)];
encode_def(cs_family_request_list, R)->
	{_}=R,
	[];
encode_def(sc_family_request_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_family_del_request, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_combine_do, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int8(V4),?list_int64(V5)];
encode_def(sc_combine_fail, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_newGer, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int8(V3),?int8(V4)];
encode_def(sc_combine_ger, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_newEquip, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int8(V4),?int8(V5)];
encode_def(sc_combine_equip, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_combine_info, R)->
	{_}=R,
	[];
encode_def(sc_combine_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?string(V3),?list_int8(V4),?list_int8(V5)];
encode_def(cs_firecracker_open, R)->
	{_}=R,
	[];
encode_def(p_discount, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(sc_firecracker_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16}=R,
	[?int8(V2),?string(V3),?string(V4),?string(V5),?int32(V6),?int32(V7),?int32(V8),?int32(V9),?int8(V10),?int8(V11),?int32(V12),?int8(V13),?int8(V14),?int16(V15),?list_tuple(V16)];
encode_def(sc_firecracker_info_sync, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(cs_firecracker_close, R)->
	{_}=R,
	[];
encode_def(cs_firecracker_setoff, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_item_view, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int8(V3),?int8(V4),?int16(V5)];
encode_def(p_reward_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?list_tuple(V7),?list_tuple(V8)];
encode_def(sc_firecracker_setoff, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int32(V3),?int16(V4),?int8(V5),?list_tuple(V6)];
encode_def(cs_firecracker_rank, R)->
	{_}=R,
	[];
encode_def(p_firecracker_rank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?string(V3),?int32(V4),?list_tuple(V5)];
encode_def(sc_firecracker_rank, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_firecracker_get_reward, R)->
	{_}=R,
	[];
encode_def(sc_firecracker_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_treaHouse_get_list, R)->
	{_}=R,
	[];
encode_def(p_treaHouse_card, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?int32(V6)];
encode_def(p_baseBoxOpenInfo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_treaHouse_get_list, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int8(V2),?int8(V3),?int8(V4),?list_tuple(V5),?int32(V6),?list_tuple(V7),?int16(V8),?int16(V9),?int32(V10)];
encode_def(cs_treaHouse_is_open, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_is_open, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_treaHouse_explore_one, R)->
	{_}=R,
	[];
encode_def(p_treaHouse_card_oneTime, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_tuple(V3)];
encode_def(sc_treaHouse_explore_one, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_treaHouse_explore_ten, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_explore_ten, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_treaHouse_refresh, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_treaHouse_open_base_box, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_treaHouse_open_base_box, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(p_treaHouse_ranker, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int16(V3),?int32(V4),?string(V5),?tuple(V6)];
encode_def(cs_treaHouse_get_rankInfo, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_get_rankInfo, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?tuple(V4),?list_tuple(V5)];
encode_def(cs_treaHouse_get_rank_Reward, R)->
	{_}=R,
	[];
encode_def(sc_treaHouse_get_rank_Reward, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?tuple(V4)];
encode_def(cs_treaHouse_get_baseBoxRewardInfo, R)->
	{_}=R,
	[];
encode_def(p_treaHouse_BaseReward_Info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?tuple(V4)];
encode_def(sc_treaHouse_get_baseBoxRewardInfo, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_treaHouse_change_state, R)->
	{_}=R,
	[];
encode_def(cs_challengeGod_info, R)->
	{_}=R,
	[];
encode_def(sc_challengeGod_info, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int16(V3),?int8(V4),?int8(V5)];
encode_def(cs_challengeGod_select_ger, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_challengeGod_select_ger, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_challengeGod_challenge_dungeon_one, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_ger_add_exp, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(p_reward, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4),?int32(V5),?list_tuple(V6),?list_tuple(V7),?int32(V8)];
encode_def(sc_challengeGod_challenge_dungeon_one, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4)];
encode_def(cs_challengeGod_challenge_dungeon_ten, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_challengeGod_result, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(sc_challengeGod_challenge_ten, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_talk_world, R)->
	{_,V2,V3}=R,
	[?int8(V2),?string(V3)];
encode_def(sc_talk_world, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_talk_message, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int8(V2),?string(V3),?string(V4),?int8(V5),?int64(V6),?int32(V7),?int8(V8),?string(V9),?bool(V10),?int32(V11)];
encode_def(cs_talk_gag_one, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_talk_ungag_one, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_talk_get_gag_list, R)->
	{_}=R,
	[];
encode_def(sc_talk_get_gag_list, R)->
	{_,V2}=R,
	[?list_string(V2)];
encode_def(cs_talk_recent_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_talk_recent_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_talk_person, R)->
	{_,V2,V3}=R,
	[?int32(V2),?string(V3)];
encode_def(sc_talk_person, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_talk_person_offline, R)->
	{_}=R,
	[];
encode_def(sc_talk_person_offline, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_talk_gm, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_talk_gm, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_push_highlight_Info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_nanm_open, R)->
	{_}=R,
	[];
encode_def(sc_nanm_open, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?bool(V2),?int64(V3),?int16(V4),?bool(V5),?int16(V6),?bool(V7),?int32(V8)];
encode_def(sc_nanm_init_state, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int64(V3),?int32(V4),?int32(V5)];
encode_def(cs_nanm_close, R)->
	{_}=R,
	[];
encode_def(cs_nanm_buff, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_nanm_buff, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_nanm_last_info, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_nanm_last_info_ignore, R)->
	{_}=R,
	[];
encode_def(p_nanm_info, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(sc_nanm_last_info_win, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int16(V2),?int32(V3),?int64(V4),?list_tuple(V5),?list_string(V6)];
encode_def(sc_nanm_last_info_fail, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(cs_nanm_cur_info, R)->
	{_}=R,
	[];
encode_def(sc_nanm_cur_info_ignore, R)->
	{_}=R,
	[];
encode_def(sc_nanm_cur_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_nanm_hp_sync, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(p_nanm_harm, R)->
	{_,V2,V3}=R,
	[?string(V2),?int64(V3)];
encode_def(sc_nanm_harm_broadcast, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_nanm_buff_sync, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_nanm_stop, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_nanm_rank_sync, R)->
	{_}=R,
	[];
encode_def(sc_nanm_rank_sync, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_nanm_fight, R)->
	{_}=R,
	[];
encode_def(sc_nanm_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int32(V6)];
encode_def(cs_nanm_reborn, R)->
	{_}=R,
	[];
encode_def(sc_nanm_reborn, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_nanm_offline_play, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(sc_nanm_offline_play, R)->
	{_,V2,V3}=R,
	[?int8(V2),?bool(V3)];
encode_def(cs_nanm_open_time, R)->
	{_}=R,
	[];
encode_def(sc_nanm_open_time, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_version, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_version, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_gift_request, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_gift_request, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_box_item, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_box_item, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?int16(V4),?int8(V5)];
encode_def(cs_box_shop, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(p_reward_view2, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_box_shop, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_box_shop_info, R)->
	{_}=R,
	[];
encode_def(p_shop_box_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int16(V2),?int32(V3),?int32(V4),?int8(V5),?int32(V6),?int32(V7)];
encode_def(sc_box_shop_info, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?int32(V3)];
encode_def(p_reward_view3, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int32(V4)];
encode_def(cs_box_get_spirit_equip_count, R)->
	{_}=R,
	[];
encode_def(sc_box_get_spirit_equip_count, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int32(V3),?int32(V4),?int32(V5),?int32(V6),?int32(V7)];
encode_def(cs_activity_get_list, R)->
	{_}=R,
	[];
encode_def(p_activity_icon, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?string(V3),?string(V4)];
encode_def(sc_activity_get_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_activity_ext, R)->
	{_,V2,V3,V4,V5}=R,
	[?int32(V2),?int32(V3),?string(V4),?string(V5)];
encode_def(cs_activity_info, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_activity_draw, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int16(V2),?string(V3),?int16(V4),?int16(V5),?int16(V6),?tuple(V7),?list_tuple(V8)];
encode_def(sc_activity_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int16(V2),?int8(V3),?string(V4),?list_tuple(V5),?int32(V6),?int32(V7),?int32(V8),?int8(V9),?int8(V10)];
encode_def(cs_activity_month, R)->
	{_}=R,
	[];
encode_def(sc_activity_month, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?int8(V3),?bool(V4),?int16(V5),?int16(V6),?int16(V7)];
encode_def(cs_activity_month_buy, R)->
	{_}=R,
	[];
encode_def(sc_activity_month_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_activity_month_draw, R)->
	{_}=R,
	[];
encode_def(sc_activity_month_draw, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_activity_draw, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int16(V3)];
encode_def(sc_activity_draw, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int16(V5),?int16(V6)];
encode_def(sc_activity_update, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int16(V4)];
encode_def(sc_activity_record_update, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(p_energy_activity, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int32(V2),?int32(V3),?int8(V4),?int8(V5),?int8(V6),?int8(V7),?int8(V8)];
encode_def(cs_activity_energy, R)->
	{_}=R,
	[];
encode_def(sc_activity_energy, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_activity_sign_emperor_info, R)->
	{_}=R,
	[];
encode_def(sc_activity_sign_emperor_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?int8(V4),?int8(V5),?string(V6)];
encode_def(cs_activity_sign_get_reward, R)->
	{_}=R,
	[];
encode_def(sc_activity_sign_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_activity_sign_up, R)->
	{_}=R,
	[];
encode_def(sc_activity_sign_up, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_activity_rebate_info, R)->
	{_}=R,
	[];
encode_def(p_rebate_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(p_rebate_list, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?string(V3),?int8(V4),?int32(V5),?int32(V6),?int32(V7),?list_tuple(V8)];
encode_def(sc_rebate_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?string(V3),?string(V4),?string(V5),?int32(V6),?int32(V7),?list_tuple(V8)];
encode_def(cs_activity_rebate_get_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_rebate_reward, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?int32(V4)];
encode_def(sc_rebate_get_reward, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_rebate_update, R)->
	{_}=R,
	[];
encode_def(cs_activity_levelRank_open, R)->
	{_}=R,
	[];
encode_def(levelRank_rankerInfo, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int8(V3),?string(V4),?tuple(V5)];
encode_def(sc_activity_levelRank_open, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?int32(V3),?list_tuple(V4)];
encode_def(cs_activity_levelRank_refresh, R)->
	{_}=R,
	[];
encode_def(sc_activity_levelRank_refresh, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_activity_day_pay_mul, R)->
	{_}=R,
	[];
encode_def(sc_activity_day_pay_mul, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_invite_info, R)->
	{_}=R,
	[];
encode_def(sc_invite_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?bool(V2),?bool(V3),?int16(V4),?string(V5),?int16(V6)];
encode_def(cs_invite_bind_weibo, R)->
	{_}=R,
	[];
encode_def(sc_invite_bind_weibo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_invite_weibo_share_levelup, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_invite_weibo_share_levelup, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_invite_input_invite_code, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_invite_input_invite_code, R)->
	{_,V2,V3}=R,
	[?int8(V2),?string(V3)];
encode_def(cs_invite_list, R)->
	{_}=R,
	[];
encode_def(p_invite, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?bool(V3),?int8(V4),?int8(V5),?string(V6),?bool(V7)];
encode_def(sc_invite_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_get_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_friend, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21}=R,
	[?int32(V2),?bool(V3),?int8(V4),?int8(V5),?string(V6),?int64(V7),?int32(V8),?string(V9),?int32(V10),?int32(V11),?int32(V12),?int16(V13),?int8(V14),?int8(V15),?int32(V16),?int32(V17),?int32(V18),?int32(V19),?int32(V20),?bool(V21)];
encode_def(sc_friend_get_list, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?int8(V4),?int8(V5),?int8(V6)];
encode_def(cs_friend_fight, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_friend_fight, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int32(V3),?int32(V4),?list_tuple(V5)];
encode_def(cs_friend_get_add_list, R)->
	{_}=R,
	[];
encode_def(p_stranger, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?bool(V3),?int8(V4),?int8(V5),?string(V6),?int64(V7),?int32(V8),?string(V9),?int32(V10),?int8(V11)];
encode_def(sc_friend_get_add_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_add, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_friend_add, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_friend_add, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?bool(V4),?int8(V5),?int8(V6),?string(V7),?int64(V8),?int32(V9)];
encode_def(cs_friend_add_list, R)->
	{_}=R,
	[];
encode_def(sc_friend_add_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_friend_new_add, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_agree, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_friend_agree, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_friend_refuse, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_friend_refuse, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_friend_explore, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_friend_explore, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_friend_delete, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_friend_delete, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(sc_friend_notify_delete, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_friend_new, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_friend_send_enargy, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_friend_send_enargy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_friend_send_enargy_me, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_friend_give_enargy, R)->
	{_,V2}=R,
	[?list_int32(V2)];
encode_def(sc_friend_give_enargy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_int32(V3),?int8(V4)];
encode_def(sc_frend_give_enargy_me, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int8(V3)];
encode_def(cs_friend_give_all_enargy, R)->
	{_}=R,
	[];
encode_def(sc_friend_remove_request, R)->
	{_}=R,
	[];
encode_def(cs_gather_get_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_gather_get_list, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_int32(V3)];
encode_def(sc_gather_new, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_int32(V3)];
encode_def(cs_hist_get_list, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_hist_get_list, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?bool(V3),?list_tuple(V4),?int16(V5)];
encode_def(sc_hist_new, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_hist_replay, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_hist_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_hist_unreadNum, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_mail_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(p_mail, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10}=R,
	[?int64(V2),?int8(V3),?int32(V4),?string(V5),?string(V6),?int32(V7),?int16(V8),?list_any(V9),?list_tuple(V10)];
encode_def(sc_mail_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int8(V3),?bool(V4),?list_tuple(V5),?list_int8(V6)];
encode_def(cs_mail_draw_reward, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_mail_draw_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_mail_delete, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_mail_delete, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_mail_new, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?string(V3),?string(V4)];
encode_def(sc_mail_new, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_mail_unread_num, R)->
	{_}=R,
	[];
encode_def(sc_mail_unread_num, R)->
	{_,V2}=R,
	[?list_int8(V2)];
encode_def(cs_mail_more, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_mail_more, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4),?list_int8(V5)];
encode_def(cs_mail_agree_friend, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_mail_agree_friend, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_mail_del_spec_mail, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_mail_del_spec_mail, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(cs_hron_info, R)->
	{_}=R,
	[];
encode_def(sc_hron_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int16(V2),?int16(V3),?int16(V4),?int16(V5),?int8(V6),?bool(V7),?int32(V8),?int8(V9),?int8(V10),?int16(V11)];
encode_def(cs_hron_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_hron_buy, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(cs_hron_fight, R)->
	{_}=R,
	[];
encode_def(sc_hron_fight, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int8(V5),?list_tuple(V6),?list_tuple(V7)];
encode_def(cs_hron_raids, R)->
	{_}=R,
	[];
encode_def(sc_hron_raids, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?list_tuple(V4)];
encode_def(cs_card_get_list, R)->
	{_}=R,
	[];
encode_def(p_card, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(p_opened_card, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int32(V4)];
encode_def(sc_card_get_list, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?bool(V2),?list_tuple(V3),?list_tuple(V4),?list_int8(V5),?int32(V6)];
encode_def(cs_card_draw, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_card_draw, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_card_refresh, R)->
	{_}=R,
	[];
encode_def(sc_card_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_card_onekey, R)->
	{_}=R,
	[];
encode_def(sc_card_onekey, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_daily_get_list, R)->
	{_}=R,
	[];
encode_def(p_daily, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?bool(V4)];
encode_def(sc_daily_get_list, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_int8(V3)];
encode_def(cs_daily_draw, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_daily_draw, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?tuple(V3),?list_int8(V4)];
encode_def(cs_daily_reward_list, R)->
	{_}=R,
	[];
encode_def(p_daily_reward, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?bool(V3),?tuple(V4)];
encode_def(p_daily_reward_info, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?list_tuple(V4)];
encode_def(sc_daily_reward_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_daily_reward_get, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_daily_reward_get, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?int8(V4)];
encode_def(cs_daily_vip_info, R)->
	{_}=R,
	[];
encode_def(sc_daily_vip_info, R)->
	{_,V2,V3}=R,
	[?bool(V2),?tuple(V3)];
encode_def(cs_daily_vip_draw, R)->
	{_}=R,
	[];
encode_def(sc_daily_vip_draw, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_pvp_get_list, R)->
	{_}=R,
	[];
encode_def(p_pvp, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?bool(V3),?int8(V4),?int8(V5),?string(V6),?int64(V7),?int16(V8),?int32(V9)];
encode_def(sc_pvp_get_list, R)->
	{_,V2,V3}=R,
	[?int16(V2),?list_tuple(V3)];
encode_def(cs_pvp_fight, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_pvp_fight, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int32(V4),?list_tuple(V5)];
encode_def(cs_pvp_get_first_eight_replays, R)->
	{_}=R,
	[];
encode_def(p_pvp_replay_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?string(V2),?string(V3),?int16(V4),?int16(V5),?int64(V6),?int32(V7)];
encode_def(sc_pvp_get_first_eight_replays, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_pvp_eight_replay, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_pvp_eight_replay, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_shop_buy_num, R)->
	{_}=R,
	[];
encode_def(p_shop_num, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int16(V4)];
encode_def(sc_shop_buy_num, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_shop_buy, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int8(V4)];
encode_def(sc_shop_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_shop_encounter, R)->
	{_}=R,
	[];
encode_def(p_shop_random, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int32(V3),?list_int16(V4)];
encode_def(sc_shop_encounter, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_shop_new, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_shop_refresh, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_shop_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_shop_auto_refresh, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_shop_treasure_info, R)->
	{_}=R,
	[];
encode_def(p_treasure, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int16(V3),?int32(V4),?int8(V5),?int32(V6),?bool(V7),?int8(V8)];
encode_def(sc_shop_treasure_info, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?string(V2),?int32(V3),?int8(V4),?int16(V5),?int32(V6),?list_tuple(V7)];
encode_def(cs_shop_treasure_buy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_shop_treasure_buy, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_shop_treasure_new_activity, R)->
	{_,V2,V3,V4,V5}=R,
	[?string(V2),?int32(V3),?int8(V4),?int16(V5)];
encode_def(sc_shop_treasure_new_shop, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_tuple(V3)];
encode_def(cs_shop_refresh2, R)->
	{_}=R,
	[];
encode_def(sc_shop_refresh2, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_item_bag, R)->
	{_}=R,
	[];
encode_def(p_item, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int64(V2),?int16(V3),?int8(V4),?int8(V5),?int16(V6),?int32(V7),?int16(V8)];
encode_def(sc_item_bag, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_equip, R)->
	{_}=R,
	[];
encode_def(sc_item_equip, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_sell, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_item_sell, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4)];
encode_def(cs_item_down_equip, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_item_down_equip, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int8(V4)];
encode_def(cs_item_up_equip, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int8(V3),?int64(V4),?int64(V5)];
encode_def(sc_item_up_equip, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int8(V4),?int64(V5)];
encode_def(sc_item_new, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_item_num_update, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int16(V3)];
encode_def(sc_item_update, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_use, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_item_use, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int8(V4)];
encode_def(sc_item_delete_notify, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(cs_item_reinforce, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_reinforce, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int16(V4)];
encode_def(cs_item_max_reinforce, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_item_max_reinforce, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?list_int16(V4)];
encode_def(sc_item_update_rank, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?int32(V4)];
encode_def(cs_item_up_rank, R)->
	{_,V2,V3,V4,V5}=R,
	[?int64(V2),?int64(V3),?int64(V4),?int64(V5)];
encode_def(sc_item_up_rank, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int64(V3),?int64(V4),?int8(V5),?int8(V6)];
encode_def(sc_item_more, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_compound, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_item_compound, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(cs_item_eat, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int64(V3),?list_int64(V4)];
encode_def(sc_item_eat, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int8(V4),?int16(V5)];
encode_def(p_all_equipment, R)->
	{_,V2,V3}=R,
	[?int32(V2),?list_int32(V3)];
encode_def(sc_item_all_equipment, R)->
	{_,V2,V3}=R,
	[?list_int64(V2),?list_tuple(V3)];
encode_def(cs_item_use_info, R)->
	{_}=R,
	[];
encode_def(p_item_use_info, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_item_use_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_item_up_all_equip, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_item_up_all_equip, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_explore_one, R)->
	{_}=R,
	[];
encode_def(sc_explore_one, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?list_tuple(V3),?int32(V4),?int32(V5),?int32(V6),?int8(V7)];
encode_def(p_echapter, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?int32(V3),?int32(V4),?bool(V5)];
encode_def(cs_explore_dungeon_list, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(p_edungeon, R)->
	{_,V2,V3}=R,
	[?int16(V2),?bool(V3)];
encode_def(sc_explore_dungeon_list, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?list_tuple(V3),?int8(V4)];
encode_def(cs_explore_challenge_encounter, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_challenge_encounter, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?int8(V5),?int8(V6)];
encode_def(sc_explore_delete_encounter, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_explore_giveup_encounter, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_giveup_encounter, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_explore_list, R)->
	{_}=R,
	[];
encode_def(sc_explore_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_explore_collect, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_collect, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(cs_explore_force_collect, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int8(V3)];
encode_def(sc_explore_force_collect, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int8(V3),?int8(V4)];
encode_def(cs_explore_auto_explore_check, R)->
	{_}=R,
	[];
encode_def(sc_explore_auto_explore_check, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_explore_encounter_pass_reward, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_encounter_pass_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_explore_encounter_dungeon_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_explore_encounter_dungeon_state, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int8(V5)];
encode_def(cs_explore_free, R)->
	{_}=R,
	[];
encode_def(sc_explore_free, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_info, R)->
	{_}=R,
	[];
encode_def(sc_ger_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(p_ger_pos_info, R)->
	{_,V2,V3,V4}=R,
	[?int64(V2),?int8(V3),?list_int64(V4)];
encode_def(sc_ger_update, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int32(V5),?int32(V6),?int64(V7),?int64(V8)];
encode_def(sc_ger_new, R)->
	{_,V2}=R,
	[?tuple(V2)];
encode_def(cs_ger_standup, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_standup, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int64(V4)];
encode_def(cs_ger_move_pos, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_move_pos, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(cs_ger_pos_list, R)->
	{_}=R,
	[];
encode_def(sc_ger_pos_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_sell, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(sc_ger_sell, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_ger_detail, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_ger_detail, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16}=R,
	[?int64(V2),?int16(V3),?int16(V4),?int16(V5),?int16(V6),?int16(V7),?int16(V8),?int16(V9),?int16(V10),?int16(V11),?int16(V12),?int16(V13),?int16(V14),?int16(V15),?int16(V16)];
encode_def(cs_ger_view_other, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_ger_view_other, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6)];
encode_def(sc_ger_update_exp, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(cs_ger_eat, R)->
	{_,V2,V3}=R,
	[?int64(V2),?list_int64(V3)];
encode_def(sc_ger_eat, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(cs_ger_up_rank, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int64(V3)];
encode_def(sc_ger_up_rank, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int64(V3),?int64(V4)];
encode_def(sc_ger_update_standlist, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_down, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13}=R,
	[?int64(V2),?int64(V3),?int64(V4),?int64(V5),?int64(V6),?int64(V7),?int64(V8),?int64(V9),?int64(V10),?int64(V11),?int64(V12),?int64(V13)];
encode_def(sc_ger_down, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_more, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_ger_del, R)->
	{_,V2}=R,
	[?list_int64(V2)];
encode_def(p_ger_power, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_refresh_power, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_view_other_dtl, R)->
	{_,V2,V3}=R,
	[?int32(V2),?int16(V3)];
encode_def(sc_ger_view_other_dtl, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11}=R,
	[?int32(V2),?string(V3),?int16(V4),?int64(V5),?list_tuple(V6),?list_tuple(V7),?list_tuple(V8),?int16(V9),?int16(V10),?list_tuple(V11)];
encode_def(cs_ger_guard_info, R)->
	{_}=R,
	[];
encode_def(p_ger_guard_attr, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(p_ger_guard, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int8(V2),?int64(V3),?int16(V4),?int8(V5),?list_tuple(V6),?list_tuple(V7)];
encode_def(sc_ger_guard_info, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_guard_set, R)->
	{_,V2,V3}=R,
	[?int64(V2),?int8(V3)];
encode_def(sc_ger_guard_set, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int64(V3),?int8(V4),?list_tuple(V5)];
encode_def(cs_ger_guard_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_bool(V3)];
encode_def(sc_ger_guard_refresh, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_ger_lieu_pos_list, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_pos_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_lieu_standup, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int64(V3)];
encode_def(sc_ger_lieu_standup, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int64(V4)];
encode_def(cs_ger_lieu_untie, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(p_ger_lieu_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int16(V3),?int8(V4),?int16(V5),?int8(V6),?int16(V7),?int8(V8)];
encode_def(sc_ger_lieu_untie, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_ger_lieu_info_list, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_info_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_lieu_move_pos, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_lieu_move_pos, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int8(V4)];
encode_def(cs_ger_lieu_lock_clo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_lieu_lock_clo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_lieu_unlock_clo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(sc_ger_lieu_unlock_clo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_ger_lieu_refresh_clo, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_ger_lieu_refresh_clo, R)->
	{_,V2,V3}=R,
	[?int8(V2),?tuple(V3)];
encode_def(cs_ger_lieu_tie_info, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_tie_info, R)->
	{_,V2}=R,
	[?list_int8(V2)];
encode_def(cs_ger_lieu_refresh_freeTimes, R)->
	{_}=R,
	[];
encode_def(sc_ger_lieu_refresh_freeTimes, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_ger_new_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_ger_down_rank, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_ger_down_rank, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(cs_message_notice, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(p_notice, R)->
	{_,V2,V3,V4}=R,
	[?int32(V2),?string(V3),?string(V4)];
encode_def(sc_message_notice, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?list_int32(V3),?list_tuple(V4),?list_tuple(V5),?list_tuple(V6),?list_tuple(V7),?list_int32(V8)];
encode_def(cs_message_certain_notice, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_message_certain_notice, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(sc_message_bc, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_message_bc_id, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_message_bc_id2, R)->
	{_,V2,V3}=R,
	[?int16(V2),?list_any(V3)];
encode_def(cs_message_test, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_message_test, R)->
	{_,V2,V3}=R,
	[?int8(V2),?string(V3)];
encode_def(sc_message_best_card, R)->
	{_,V2,V3,V4}=R,
	[?string(V2),?int8(V3),?int32(V4)];
encode_def(sc_message_ger_upLevel, R)->
	{_,V2,V3}=R,
	[?string(V2),?tuple(V3)];
encode_def(sc_message_item_uprank, R)->
	{_,V2,V3}=R,
	[?string(V2),?tuple(V3)];
encode_def(cs_battle_progress, R)->
	{_}=R,
	[];
encode_def(p_battle_progress, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?int16(V3),?int16(V4),?int16(V5)];
encode_def(sc_battle_progress, R)->
	{_,V2,V3}=R,
	[?list_tuple(V2),?list_int16(V3)];
encode_def(cs_battle_info, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(p_dungeon, R)->
	{_,V2,V3,V4}=R,
	[?int16(V2),?int16(V3),?int8(V4)];
encode_def(sc_battle_info, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int16(V3),?bool(V4),?list_tuple(V5),?int16(V6)];
encode_def(cs_battle_challenge, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_battle_challenge, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?list_tuple(V3),?list_tuple(V4),?int8(V5)];
encode_def(cs_battle_perfect_reward, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_battle_perfect_reward, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_battle_broadcast_get_item, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?string(V2),?int32(V3),?int8(V4),?int16(V5),?int16(V6)];
encode_def(cs_battle_dungeon_raids, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_battle_dungeon_raids, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?list_tuple(V4)];
encode_def(cs_battle_coin_info, R)->
	{_}=R,
	[];
encode_def(sc_battle_coin_info, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(cs_battle_coin_fight, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_battle_coin_fight, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?int8(V2),?int32(V3),?bool(V4),?int32(V5),?list_tuple(V6)];
encode_def(cs_role_info, R)->
	{_}=R,
	[];
encode_def(sc_role_info, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23,V24,V25,V26,V27,V28,V29,V30,V31,V32,V33,V34,V35,V36,V37,V38,V39,V40,V41,V42,V43,V44}=R,
	[?int32(V2),?string(V3),?bool(V4),?string(V5),?int32(V6),?int16(V7),?int64(V8),?int32(V9),?int32(V10),?int32(V11),?int32(V12),?int32(V13),?int8(V14),?int32(V15),?int16(V16),?int16(V17),?int32(V18),?int8(V19),?int32(V20),?int8(V21),?int8(V22),?int8(V23),?int8(V24),?int8(V25),?int8(V26),?bool(V27),?bool(V28),?int16(V29),?int16(V30),?int16(V31),?int16(V32),?int8(V33),?int32(V34),?int32(V35),?int16(V36),?int16(V37),?int32(V38),?int32(V39),?int32(V40),?bool(V41),?int8(V42),?int32(V43),?list_int16(V44)];
encode_def(sc_role_update_level, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_update_list, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_role_buy_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_buy_energy, R)->
	{_,V2,V3,V4,V5,V6,V7,V8}=R,
	[?int8(V2),?int8(V3),?int16(V4),?int16(V5),?int32(V6),?int32(V7),?list_tuple(V8)];
encode_def(sc_role_update_exp, R)->
	{_,V2}=R,
	[?int64(V2)];
encode_def(sc_role_update_coin, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_reputation, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_gold, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_goldBonus, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_vipLevel, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int16(V3),?int16(V4)];
encode_def(sc_role_update_energy, R)->
	{_,V2,V3}=R,
	[?int16(V2),?int32(V3)];
encode_def(sc_role_update_discoveryTimes, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_role_update_pvpTimes, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_role_update_ruleTimes, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(sc_role_update_randomPVPTimes, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_singlePVPTimes, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_goldUsed, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_update_title, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_encounterFreeNum, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_update_weiboCount, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_setting, R)->
	{_}=R,
	[];
encode_def(sc_role_setting, R)->
	{_,V2}=R,
	[?list_int8(V2)];
encode_def(cs_role_get_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_get_energy, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_buy_coin_value, R)->
	{_}=R,
	[];
encode_def(sc_role_buy_coin_value, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_role_weixin_share, R)->
	{_}=R,
	[];
encode_def(sc_role_update_pay_ext, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_role_suggest_open, R)->
	{_}=R,
	[];
encode_def(sc_role_suggest_open, R)->
	{_,V2}=R,
	[?bool(V2)];
encode_def(cs_role_suggest, R)->
	{_,V2,V3}=R,
	[?string(V2),?string(V3)];
encode_def(sc_role_suggest, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_log_guide_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_role_pay_tencent, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_pay_tencent, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(cs_role_login_reward, R)->
	{_}=R,
	[];
encode_def(sc_role_login_reward, R)->
	{_,V2}=R,
	[?list_tuple(V2)];
encode_def(cs_role_change_name, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_role_change_name, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_check_tencent, R)->
	{_}=R,
	[];
encode_def(sc_role_check_tencent, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_push_setting, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int16(V3)];
encode_def(sc_role_push_setting, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int8(V3),?int16(V4)];
encode_def(cs_role_get_guide_state, R)->
	{_}=R,
	[];
encode_def(sc_role_get_guide_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(cs_role_set_guide_state, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_set_guide_state, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_change_head, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(sc_role_change_head, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int32(V3)];
encode_def(cs_role_change_location, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(cs_role_token, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_role_token, R)->
	{_}=R,
	[];
encode_def(cs_role_select_ger, R)->
	{_,V2}=R,
	[?int16(V2)];
encode_def(sc_role_select_ger, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_role_demo_fight, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_role_demo_fight, R)->
	{_,V2,V3}=R,
	[?int8(V2),?list_tuple(V3)];
encode_def(sc_role_base_config, R)->
	{_,V2,V3}=R,
	[?int8(V2),?int8(V3)];
encode_def(cs_role_pay_ios, R)->
	{_,V2,V3,V4,V5,V6}=R,
	[?string(V2),?int32(V3),?string(V4),?string(V5),?int8(V6)];
encode_def(sc_role_pay_ios, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?string(V3),?int32(V4),?bool(V5)];
encode_def(cs_role_pay_91, R)->
	{_,V2,V3,V4,V5}=R,
	[?string(V2),?int32(V3),?string(V4),?string(V5)];
encode_def(sc_role_pay_91, R)->
	{_,V2,V3,V4,V5}=R,
	[?int8(V2),?string(V3),?int32(V4),?bool(V5)];
encode_def(sc_role_pay_uc, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_dl, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_zz, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_360, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_wdj, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_dk, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_mi, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(sc_role_pay_az, R)->
	{_,V2,V3,V4}=R,
	[?int8(V2),?int32(V3),?bool(V4)];
encode_def(cs_account_login, R)->
	{_,V2,V3,V4,V5,V6,V7,V8,V9}=R,
	[?int32(V2),?int32(V3),?string(V4),?string(V5),?string(V6),?int16(V7),?string(V8),?int16(V9)];
encode_def(sc_account_login, R)->
	{_,V2,V3,V4,V5}=R,
	[?int16(V2),?bool(V3),?bool(V4),?int8(V5)];
encode_def(cs_account_create, R)->
	{_,V2,V3}=R,
	[?string(V2),?int8(V3)];
encode_def(sc_account_create, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_account_enter_game, R)->
	{_}=R,
	[];
encode_def(sc_account_enter_game, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(sc_account_kick, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_account_heart, R)->
	{_}=R,
	[];
encode_def(sc_account_heart, R)->
	{_,V2}=R,
	[?int32(V2)];
encode_def(cs_account_logout, R)->
	{_}=R,
	[];
encode_def(cs_account_check_rolename, R)->
	{_,V2}=R,
	[?string(V2)];
encode_def(sc_account_check_rolename, R)->
	{_,V2}=R,
	[?int8(V2)];
encode_def(cs_account_pay_arg, R)->
	{_,V2,V3,V4,V5,V6,V7}=R,
	[?int32(V2),?string(V3),?string(V4),?string(V5),?string(V6),?string(V7)];
encode_def(sc_account_pay_arg, R)->
	{_}=R,
	[];
encode_def(sc_account_update_pay_arg, R)->
	{_}=R,
	[];
encode_def(_, _) -> <<>>.

decode_def(20301)->
	[cs_melee_info];
decode_def(20302)->
	[sc_melee_info,int8,int8,int32,int32,int32,int16,int8,int8];
decode_def(20303)->
	[cs_melee_sign];
decode_def(20304)->
	[sc_melee_sign,int8];
decode_def(20305)->
	[cs_melee_fight];
decode_def(11403)->
	[p_id_num,int16,int32];
decode_def(11404)->
	[p_mail_reward,{list, 11403},{list, 11403}];
decode_def(20004)->
	[p_action,int8,int8,{list, int8},int8,int32,int8];
decode_def(20003)->
	[p_fighter,int64,int16,int8,int64,int64,int32,int8,int16];
decode_def(20002)->
	[sc_fight_request,{list, 20003},{list, 20004},bool];
decode_def(20306)->
	[sc_melee_fight,int8,int64,string,int16,int32,int32,11404,{list, 20002},int8];
decode_def(20201)->
	[p_homestead_log,string,int8,int8,string,int8,int32];
decode_def(20202)->
	[p_homestead_machine,int8,int16,int32,int32,int32,int32,int8];
decode_def(20203)->
	[p_homestead,string,int8,int8,int32,int8,int16,int64,int16,int16,int32];
decode_def(20204)->
	[sc_homestead_error,int8];
decode_def(20205)->
	[cs_homestead_get_info];
decode_def(20206)->
	[sc_homestead_get_info,20203,{list, 20202}];
decode_def(20207)->
	[cs_homestead_get_friend_info,int32];
decode_def(20208)->
	[sc_homestead_get_friend_info,int32,20203,{list, 20202}];
decode_def(20209)->
	[cs_homestead_unlock_machine,int8];
decode_def(20210)->
	[sc_homestead_unlock_machine,20202];
decode_def(20211)->
	[cs_homestead_uproot_seed,int8];
decode_def(20212)->
	[sc_homestead_uproot_seed,int8];
decode_def(20213)->
	[cs_homestead_harvest,int8];
decode_def(12005)->
	[p_reward_view,int8,int32];
decode_def(20214)->
	[sc_homestead_harvest,20202,12005];
decode_def(20215)->
	[cs_homestead_seeding,int8,int16];
decode_def(20216)->
	[sc_homestead_seeding,20202];
decode_def(20217)->
	[sc_homestead_update_machine,20202];
decode_def(20218)->
	[cs_homestead_change_ger,int64];
decode_def(20219)->
	[sc_homestead_change_ger,int64];
decode_def(20220)->
	[cs_homestead_mating,int32];
decode_def(20221)->
	[sc_homestead_mating,int8,{list, 12005},int32,int32,int8];
decode_def(20222)->
	[sc_homestead_mating_to_friend,20201,int32,int8];
decode_def(20223)->
	[cs_homestead_addenergy,int32,int8];
decode_def(20224)->
	[sc_homestead_addenergy,int32,int8,int8,int32,int32,{list, 12005}];
decode_def(20225)->
	[sc_homestead_addenergy_to_friend,int8,20201,int32,int32];
decode_def(20226)->
	[cs_homestead_get_log];
decode_def(20227)->
	[sc_homestead_get_log,{list, 20201}];
decode_def(20228)->
	[cs_homestead_get_friend_log,int32];
decode_def(20229)->
	[sc_homestead_get_friend_log,int32,{list, 20201}];
decode_def(20230)->
	[sc_homestead_sync_mating_cool_second,int32,int32];
decode_def(20231)->
	[sc_homestead_sync_ger,int32,int32,int16];
decode_def(20232)->
	[sc_homestead_sync_add_enagy,int32,int32,int32,int32,int32];
decode_def(20101)->
	[p_task,int32,int8,int32];
decode_def(20103)->
	[cs_task_get_info];
decode_def(20104)->
	[sc_task_get_info,{list, 20101},{list, 20101},{list, 20101}];
decode_def(20105)->
	[cs_task_operate,int8,int32];
decode_def(20106)->
	[sc_task_operate,int8,int32,int8,{list, 12005}];
decode_def(20107)->
	[sc_task_error,int8];
decode_def(20108)->
	[sc_task_notify_change,int8,{list, 20101},{list, int32}];
decode_def(20001)->
	[cs_fight_request];
decode_def(14101)->
	[cs_road_info];
decode_def(14102)->
	[sc_road_info,int8,int8,int8,int8];
decode_def(14103)->
	[cs_road_reset];
decode_def(14104)->
	[sc_road_reset,int8];
decode_def(14105)->
	[cs_road_fight];
decode_def(14106)->
	[sc_road_fight,int8,bool,int8,{list, 20002},{list, 11404}];
decode_def(14107)->
	[cs_road_fight_ext];
decode_def(14108)->
	[sc_road_fight_ext,int8,bool,{list, 20002},{list, 11404}];
decode_def(14109)->
	[cs_road_box];
decode_def(14110)->
	[sc_road_box,int8,{list, 11404}];
decode_def(14001)->
	[cs_hula_open];
decode_def(14003)->
	[p_hula_boss_open,int8,int64,int16,string,bool,int8,int32];
decode_def(14002)->
	[sc_hula_open,bool,{list, 14003},int32];
decode_def(14005)->
	[p_hula_boss_state,int8,int64,int64,string,bool,int8,int32];
decode_def(14004)->
	[sc_hula_init_state,{list, 14005},int64,int32,int32,int32];
decode_def(14006)->
	[cs_hula_close];
decode_def(14007)->
	[p_hula_info,string,int64];
decode_def(14008)->
	[cs_hula_cur_info];
decode_def(14009)->
	[sc_hula_cur_info,{list, 14007}];
decode_def(14010)->
	[sc_hula_hp_sync,int8,int64];
decode_def(14012)->
	[p_hula_harm,string,int64];
decode_def(14011)->
	[sc_hula_harm_broadcast,{list, 14012}];
decode_def(14022)->
	[p_role_stastic,int64,int32,int32];
decode_def(14013)->
	[sc_hula_stop,int8,int32,14022];
decode_def(14014)->
	[cs_hula_rank_sync];
decode_def(14015)->
	[sc_hula_rank_sync,int16];
decode_def(14016)->
	[cs_hula_fight];
decode_def(14017)->
	[sc_hula_fight,int8,{list, 20002},int32,int32,int32];
decode_def(14018)->
	[cs_hula_reborn];
decode_def(14019)->
	[sc_hula_reborn,int8];
decode_def(14020)->
	[cs_hula_open_time];
decode_def(14021)->
	[sc_hula_open_time,int32];
decode_def(13901)->
	[cs_festival_info,int32];
decode_def(13908)->
	[p_festival_rank_reward,int16,int16,11404];
decode_def(13902)->
	[sc_festival_info,int32,int32,int32,int32,int32,int32,int16,int16,int16,string,{list, 13908}];
decode_def(13903)->
	[cs_festival_click,int32,int8];
decode_def(13904)->
	[sc_festival_click,int32,int8,int8,int16,int32,int32,int32,{list, 11404}];
decode_def(13905)->
	[cs_festival_rank,int32,int16,int16];
decode_def(13907)->
	[p_festival_rank,int32,bool,int8,int32,int16,string,int32,int16];
decode_def(13906)->
	[sc_festival_rank,int32,int8,int16,{list, 13907}];
decode_def(13801)->
	[cs_rule_info];
decode_def(13809)->
	[p_rule_fighter,int32,int64,bool,int8,int32,int16,string,int32,int32,int16,int16,int16,int16];
decode_def(11503)->
	[p_hist,int64,int8,string,int32,int32,int16,int32];
decode_def(13802)->
	[sc_rule_info,int32,int32,int16,int16,int16,int16,13809,{list, 11503}];
decode_def(13803)->
	[cs_rule_rank];
decode_def(13804)->
	[sc_rule_rank,{list, 13809}];
decode_def(13805)->
	[cs_rule_last_rank];
decode_def(13806)->
	[sc_rule_last_rank,{list, 13809}];
decode_def(13807)->
	[cs_rule_fight];
decode_def(13808)->
	[sc_rule_fight,int8,bool,string,int32,int32,int16,int16,int16,int16,11404,13809,{list, 20002}];
decode_def(13810)->
	[cs_rule_leave];
decode_def(13701)->
	[cs_rank_info,int8,int8,int8,int8];
decode_def(13703)->
	[p_rank_info,int8,int32,int16,string,int32,int8,bool,int64,int16,string];
decode_def(13702)->
	[sc_rank_info,int8,int8,{list, 13703}];
decode_def(13601)->
	[cs_alien_info];
decode_def(13606)->
	[p_alien_fighter,int32,int64,bool,int8,int32,int16,string,int32,int16,int8,int32];
decode_def(13602)->
	[sc_alien_info,bool,int32,int8,bool,int8,{list, 13606},int32,int16,int8,int8];
decode_def(13603)->
	[sc_alien_sign_info,int8,int16,bool,int32];
decode_def(13604)->
	[cs_alien_first_five];
decode_def(13605)->
	[sc_alien_first_five,{list, 13606}];
decode_def(13607)->
	[cs_alien_kill_num_rank,int8,int8];
decode_def(13609)->
	[p_alien_fighter2,int32,int64,bool,int8,int32,int16,string,int32,int16,int16];
decode_def(13608)->
	[sc_alien_kill_num_rank,{list, 13609}];
decode_def(13610)->
	[cs_alien_kill_continuous_rank,int8,int8];
decode_def(13612)->
	[p_alien_fighter3,int32,int64,bool,int8,int32,int16,string,int32,int16,int16,bool];
decode_def(13611)->
	[sc_alien_kill_continuous_rank,{list, 13612}];
decode_def(13613)->
	[cs_alien_guess_info];
decode_def(13614)->
	[sc_alien_guess_info,int32,bool,int32,int32,{list, int32}];
decode_def(13615)->
	[cs_alien_guess,int32,bool];
decode_def(13616)->
	[sc_alien_guess,int8];
decode_def(13617)->
	[cs_alien_reset];
decode_def(13618)->
	[sc_alien_reset,int8,int32,{list, 13606}];
decode_def(13619)->
	[cs_alien_fight,int32,int16];
decode_def(13620)->
	[sc_alien_fight,int8,{list, 20002},int16,int32,{list, 13606}];
decode_def(13621)->
	[cs_alien_sign];
decode_def(13622)->
	[sc_alien_sign,int8];
decode_def(13623)->
	[cs_alien_self_record,int8,int8];
decode_def(13627)->
	[p_alien_self_record,bool,bool,string,int16,int64,int32];
decode_def(13624)->
	[sc_alien_self_record,{list, 13627}];
decode_def(13625)->
	[cs_alien_record,int8,int8];
decode_def(13628)->
	[p_alien_record,int8,string,string,int16,int16,int64,int32];
decode_def(13626)->
	[sc_alien_record,{list, 13628}];
decode_def(13629)->
	[sc_alien_update_times,int8,int32];
decode_def(13630)->
	[cs_alien_self_fight_replay,int64];
decode_def(13631)->
	[sc_alien_self_fight_replay,20002];
decode_def(13632)->
	[cs_alien_fight_replay,int64];
decode_def(13633)->
	[sc_alien_fight_repaly,20002];
decode_def(13634)->
	[sc_alien_new_fighter_list,{list, 13606}];
decode_def(13635)->
	[cs_alien_leave];
decode_def(13636)->
	[sc_alien_new_self_record];
decode_def(13637)->
	[cs_alien_view_other,int32];
decode_def(10418)->
	[p_ger_view,int16,int16,int16];
decode_def(13638)->
	[sc_alien_view_other,int32,string,int16,int64,{list, 10418},int32,string,int16,int64,{list, 10418}];
decode_def(13639)->
	[cs_alien_view_other_dtl,int32];
decode_def(13640)->
	[cs_alien_buy_times,int8];
decode_def(13641)->
	[sc_alien_buy_times,int8,int8];
decode_def(13642)->
	[cs_alien_active];
decode_def(13643)->
	[sc_alien_active,int8];
decode_def(13501)->
	[cs_team_pk_info];
decode_def(13503)->
	[p_team_member,int32,int64,bool,int8,int32,int16,string];
decode_def(13502)->
	[sc_team_pk_open,int64,int32,int32,int8,int8,{list, 13503},{list, 13503},int32];
decode_def(13504)->
	[sc_team_pk_close,int64,int32,int32,int32,{list, 13503}];
decode_def(13505)->
	[cs_team_refresh,int8];
decode_def(13506)->
	[sc_team_refresh,int8,int8,{list, 13503}];
decode_def(13507)->
	[cs_team_fight];
decode_def(13509)->
	[p_team_member2,int32,int64,bool,int8,int32,int16,string,bool];
decode_def(13508)->
	[sc_team_fight_result,bool,int32,int32,int32,int32,int32,int8,int8,{list, 13509},{list, 13509},{list, 20002},{list, 13503},{list, 13503}];
decode_def(13510)->
	[cs_team_rank];
decode_def(13512)->
	[p_team_member3,int32,int64,bool,int8,int32,int16,string,int32,int32];
decode_def(13511)->
	[sc_team_rank,int32,{list, 13512}];
decode_def(13513)->
	[cs_team_record];
decode_def(13515)->
	[p_team_record,bool,int32,string,string,{list, int64}];
decode_def(13514)->
	[sc_team_record,{list, 13515}];
decode_def(13516)->
	[cs_team_self_record];
decode_def(13518)->
	[p_team_self_record,int32,bool,int32,int32,int32,{list, string},{list, string},{list, int64}];
decode_def(13517)->
	[sc_team_self_record,{list, 13518}];
decode_def(13519)->
	[cs_team_move,int8,int8];
decode_def(13520)->
	[sc_team_move,int8];
decode_def(13521)->
	[sc_team_pk_not_open,int16];
decode_def(13522)->
	[sc_team_fight_error,int8];
decode_def(13523)->
	[cs_team_fight_replay,{list, int64}];
decode_def(13524)->
	[sc_team_fight_replay,int8,{list, 20002},{list, 13509},{list, 13509}];
decode_def(13525)->
	[cs_team_self_fight_replay,{list, int64}];
decode_def(13526)->
	[sc_team_self_fight_replay,int8,{list, 20002},{list, 13509},{list, 13509}];
decode_def(13527)->
	[cs_team_view_other,int32];
decode_def(13528)->
	[sc_team_view_other,int32,string,int16,int64,{list, 10418}];
decode_def(13529)->
	[cs_team_view_other_dtl,int32];
decode_def(10471)->
	[p_lieu_view,int16];
decode_def(10606)->
	[p_equip,int64,int16,int8,int8,int64,int8,int32,int16];
decode_def(10413)->
	[p_ger_pos,int64,int8];
decode_def(10403)->
	[p_ger,int64,int16,int16,int16,int32,int32,int64,int64];
decode_def(13530)->
	[sc_team_view_other_dtl,int32,string,int16,int64,{list, 10403},{list, 10606},{list, 10413},int16,int16,{list, 10471}];
decode_def(13531)->
	[cs_team_new_status];
decode_def(13532)->
	[sc_team_new_status,bool];
decode_def(13402)->
	[p_race_rec,string,string,{list, int64},int32,int32,int64,int64,int8,int8,bool,bool,int8,int8,int32,int32,{list, bool}];
decode_def(13401)->
	[sc_race_new_fight,13402];
decode_def(13403)->
	[p_race_fighter,int32,string,int64,int16,bool,int8,int32];
decode_def(13404)->
	[cs_race_history,int8,int8,int16,int16];
decode_def(13405)->
	[sc_race_history,int8,int8,{list, 13402}];
decode_def(13406)->
	[cs_race_replay,int64];
decode_def(13407)->
	[sc_race_replay,int8,20002];
decode_def(13408)->
	[cs_race_fight_list,int8];
decode_def(13409)->
	[sc_race_fight_list,int8,{list, 13403}];
decode_def(13410)->
	[cs_race_sign];
decode_def(13411)->
	[sc_race_sign,int8];
decode_def(13412)->
	[cs_race_info];
decode_def(13416)->
	[p_race_pos,int32,string,bool,int8,int32,int8];
decode_def(13413)->
	[sc_race_info,int8,int32,int16,bool,{list, 13416},string,int8,bool];
decode_def(13414)->
	[cs_race_enter];
decode_def(13415)->
	[cs_race_leave];
decode_def(13417)->
	[cs_race_pos_history,int8];
decode_def(13418)->
	[sc_race_pos_history,int8,13402];
decode_def(13419)->
	[sc_race_new_first,13416];
decode_def(13420)->
	[sc_race_new_status,int8,int32];
decode_def(13421)->
	[cs_race_is_open];
decode_def(13422)->
	[sc_race_is_open,bool];
decode_def(13423)->
	[cs_race_auto_sign];
decode_def(13424)->
	[sc_race_auto_sign,int8];
decode_def(13425)->
	[cs_race_auto_unsign];
decode_def(13426)->
	[sc_race_auto_unsign,int8];
decode_def(13427)->
	[cs_race_self_history];
decode_def(13428)->
	[sc_race_self_history,{list, 13402}];
decode_def(13429)->
	[cs_race_guess_info];
decode_def(13430)->
	[sc_race_guess_info,int32,int32,{list, int32}];
decode_def(13431)->
	[cs_race_guess,int32,int32];
decode_def(13432)->
	[sc_race_guess,int8];
decode_def(13302)->
	[p_family_member_info,int32,string,int32,int32,int32,int8,bool,bool,int16,int64,int8];
decode_def(13301)->
	[p_family_info,int32,string,int16,int32,string,int32,string,int16,int32,string,{list, 13302},int32,int32];
decode_def(13303)->
	[p_family_request,int32,string,int16,int64,int32,int32];
decode_def(13304)->
	[p_family_summary,int32,string,string,int16,int16,int32,string,bool,int32];
decode_def(13305)->
	[cs_family_get_list,int16,int16];
decode_def(13306)->
	[sc_family_get_list,int8,{list, 13304}];
decode_def(13307)->
	[cs_family_create,string,bool];
decode_def(13308)->
	[sc_family_create,int8,13301,int32];
decode_def(13309)->
	[cs_family_request_join,int32];
decode_def(13310)->
	[sc_family_request_join,int8,bool,int32];
decode_def(13311)->
	[cs_family_cancel_join,int32];
decode_def(13312)->
	[sc_family_cancel_join,int8,bool];
decode_def(13313)->
	[cs_family_agree_join,int32];
decode_def(13314)->
	[sc_family_agree_join,int8,bool,13301];
decode_def(13315)->
	[cs_family_refuse_join,int32];
decode_def(13316)->
	[sc_family_refuse_join,int8,bool];
decode_def(13317)->
	[cs_family_get_info];
decode_def(13318)->
	[sc_family_get_info,int8,13301];
decode_def(13319)->
	[cs_family_kick,int32];
decode_def(13320)->
	[sc_family_kick,int8,bool,13301];
decode_def(13321)->
	[cs_family_create_consume];
decode_def(13322)->
	[sc_family_create_consume,int32,int32];
decode_def(13323)->
	[cs_family_leave];
decode_def(13324)->
	[sc_family_leave,int8,bool,13301];
decode_def(13325)->
	[cs_family_change_notice,string];
decode_def(13326)->
	[sc_family_change_notice,int8,bool,string];
decode_def(13327)->
	[cs_family_request_list];
decode_def(13328)->
	[sc_family_request_list,int8,{list, 13303}];
decode_def(13329)->
	[sc_family_del_request,int32];
decode_def(13201)->
	[cs_combine_do,int8,int16,int8,{list, int64}];
decode_def(13202)->
	[sc_combine_fail,int8];
decode_def(13205)->
	[p_newGer,int16,int8,int8];
decode_def(13203)->
	[sc_combine_ger,{list, 13205}];
decode_def(13206)->
	[p_newEquip,int16,int16,int8,int8];
decode_def(13204)->
	[sc_combine_equip,{list, 13206}];
decode_def(13207)->
	[cs_combine_info];
decode_def(13208)->
	[sc_combine_info,int32,string,{list, int8},{list, int8}];
decode_def(13101)->
	[cs_firecracker_open];
decode_def(13103)->
	[p_discount,int32,int8];
decode_def(13102)->
	[sc_firecracker_open,int8,string,string,string,int32,int32,int32,int32,int8,int8,int32,int8,int8,int16,{list, 13103}];
decode_def(13104)->
	[sc_firecracker_info_sync,int32,int8];
decode_def(13105)->
	[cs_firecracker_close];
decode_def(13106)->
	[cs_firecracker_setoff,int8];
decode_def(10616)->
	[p_item_view,int16,int8,int8,int16];
decode_def(11930)->
	[p_reward_info,int32,int32,int32,int32,int32,{list, 10616},{list, 10418}];
decode_def(13107)->
	[sc_firecracker_setoff,int8,int32,int16,int8,{list, 11930}];
decode_def(13108)->
	[cs_firecracker_rank];
decode_def(13110)->
	[p_firecracker_rank,int8,string,int32,{list, 11930}];
decode_def(13109)->
	[sc_firecracker_rank,{list, 13110}];
decode_def(13111)->
	[cs_firecracker_get_reward];
decode_def(13112)->
	[sc_firecracker_get_reward,int8,{list, 11930}];
decode_def(13001)->
	[cs_treaHouse_get_list];
decode_def(13002)->
	[p_treaHouse_card,int8,int8,int8,int8,int32];
decode_def(13022)->
	[p_baseBoxOpenInfo,int8,int8];
decode_def(13003)->
	[sc_treaHouse_get_list,int8,int8,int8,{list, 13022},int32,{list, 13002},int16,int16,int32];
decode_def(13004)->
	[cs_treaHouse_is_open];
decode_def(13005)->
	[sc_treaHouse_is_open,int8];
decode_def(13006)->
	[cs_treaHouse_explore_one];
decode_def(13008)->
	[p_treaHouse_card_oneTime,{list, 13002},{list, 13002}];
decode_def(13007)->
	[sc_treaHouse_explore_one,int8,int8,{list, 13008}];
decode_def(13009)->
	[cs_treaHouse_explore_ten];
decode_def(13010)->
	[sc_treaHouse_explore_ten,int8,int8,int8,{list, 13008}];
decode_def(13011)->
	[cs_treaHouse_refresh];
decode_def(13012)->
	[sc_treaHouse_refresh,int8,{list, 13002}];
decode_def(13013)->
	[cs_treaHouse_open_base_box,int8];
decode_def(13014)->
	[sc_treaHouse_open_base_box,int8,{list, 13022}];
decode_def(13015)->
	[p_treaHouse_ranker,int8,int16,int32,string,11930];
decode_def(13016)->
	[cs_treaHouse_get_rankInfo];
decode_def(13017)->
	[sc_treaHouse_get_rankInfo,int8,int8,13015,{list, 13015}];
decode_def(13018)->
	[cs_treaHouse_get_rank_Reward];
decode_def(13019)->
	[sc_treaHouse_get_rank_Reward,int8,int8,11930];
decode_def(13020)->
	[cs_treaHouse_get_baseBoxRewardInfo];
decode_def(13023)->
	[p_treaHouse_BaseReward_Info,int8,int32,11930];
decode_def(13021)->
	[sc_treaHouse_get_baseBoxRewardInfo,{list, 13023}];
decode_def(13024)->
	[sc_treaHouse_change_state];
decode_def(12701)->
	[cs_challengeGod_info];
decode_def(12702)->
	[sc_challengeGod_info,int16,int16,int8,int8];
decode_def(12703)->
	[cs_challengeGod_select_ger,int8];
decode_def(12704)->
	[sc_challengeGod_select_ger,int8];
decode_def(12705)->
	[cs_challengeGod_challenge_dungeon_one,int16];
decode_def(10211)->
	[p_ger_add_exp,int8,int32,bool];
decode_def(10210)->
	[p_reward,int32,int32,{list, 10211},int32,{list, 10616},{list, 10418},int32];
decode_def(12706)->
	[sc_challengeGod_challenge_dungeon_one,int8,{list, 20002},{list, 10210}];
decode_def(12707)->
	[cs_challengeGod_challenge_dungeon_ten,int16];
decode_def(12708)->
	[p_challengeGod_result,int8,10210];
decode_def(12709)->
	[sc_challengeGod_challenge_ten,int8,{list, 12708}];
decode_def(12601)->
	[cs_talk_world,int8,string];
decode_def(12602)->
	[sc_talk_world,int8,int8];
decode_def(12603)->
	[sc_talk_message,int8,string,string,int8,int64,int32,int8,string,bool,int32];
decode_def(12604)->
	[cs_talk_gag_one,string];
decode_def(12605)->
	[cs_talk_ungag_one,string];
decode_def(12606)->
	[cs_talk_get_gag_list];
decode_def(12607)->
	[sc_talk_get_gag_list,{list, string}];
decode_def(12608)->
	[cs_talk_recent_list,int8];
decode_def(12609)->
	[sc_talk_recent_list,int8,{list, 12603}];
decode_def(12610)->
	[cs_talk_person,int32,string];
decode_def(12611)->
	[sc_talk_person,int8];
decode_def(12612)->
	[cs_talk_person_offline];
decode_def(12613)->
	[sc_talk_person_offline,{list, 12603}];
decode_def(12614)->
	[cs_talk_gm,int32];
decode_def(12615)->
	[sc_talk_gm,int8];
decode_def(12501)->
	[sc_push_highlight_Info,int8,int8];
decode_def(12401)->
	[cs_nanm_open];
decode_def(12402)->
	[sc_nanm_open,bool,int64,int16,bool,int16,bool,int32];
decode_def(12403)->
	[sc_nanm_init_state,int64,int64,int32,int32];
decode_def(12404)->
	[cs_nanm_close];
decode_def(12405)->
	[cs_nanm_buff,int8];
decode_def(12406)->
	[sc_nanm_buff,int8,int8];
decode_def(12411)->
	[cs_nanm_last_info,int32];
decode_def(12412)->
	[sc_nanm_last_info_ignore];
decode_def(12415)->
	[p_nanm_info,string,int64];
decode_def(12413)->
	[sc_nanm_last_info_win,int16,int32,int64,{list, 12415},{list, string}];
decode_def(12414)->
	[sc_nanm_last_info_fail,int16,int32];
decode_def(12416)->
	[cs_nanm_cur_info];
decode_def(12417)->
	[sc_nanm_cur_info_ignore];
decode_def(12418)->
	[sc_nanm_cur_info,{list, 12415}];
decode_def(12420)->
	[sc_nanm_hp_sync,int64];
decode_def(12422)->
	[p_nanm_harm,string,int64];
decode_def(12421)->
	[sc_nanm_harm_broadcast,{list, 12422}];
decode_def(12424)->
	[sc_nanm_buff_sync,int16];
decode_def(12425)->
	[sc_nanm_stop,int8,14022];
decode_def(12426)->
	[cs_nanm_rank_sync];
decode_def(12427)->
	[sc_nanm_rank_sync,int16];
decode_def(12431)->
	[cs_nanm_fight];
decode_def(12432)->
	[sc_nanm_fight,int8,{list, 20002},int32,int32,int32];
decode_def(12441)->
	[cs_nanm_reborn];
decode_def(12442)->
	[sc_nanm_reborn,int8];
decode_def(12443)->
	[cs_nanm_offline_play,bool];
decode_def(12444)->
	[sc_nanm_offline_play,int8,bool];
decode_def(12450)->
	[cs_nanm_open_time];
decode_def(12451)->
	[sc_nanm_open_time,int32];
decode_def(12301)->
	[cs_version,string];
decode_def(12302)->
	[sc_version,int8];
decode_def(12201)->
	[cs_gift_request,string];
decode_def(12202)->
	[sc_gift_request,int8,{list, 11930}];
decode_def(12001)->
	[cs_box_item,int16,int8];
decode_def(12002)->
	[sc_box_item,int8,{list, 12005},int16,int8];
decode_def(12003)->
	[cs_box_shop,int32,int8];
decode_def(12006)->
	[p_reward_view2,int8,int16,int16];
decode_def(12004)->
	[sc_box_shop,int8,{list, 12006}];
decode_def(12007)->
	[cs_box_shop_info];
decode_def(12009)->
	[p_shop_box_info,int16,int32,int32,int8,int32,int32];
decode_def(12008)->
	[sc_box_shop_info,{list, 12009},int32];
decode_def(12010)->
	[p_reward_view3,int8,int16,int32];
decode_def(12011)->
	[cs_box_get_spirit_equip_count];
decode_def(12012)->
	[sc_box_get_spirit_equip_count,int32,int32,int32,int32,int32,int32];
decode_def(11901)->
	[cs_activity_get_list];
decode_def(11903)->
	[p_activity_icon,int16,string,string];
decode_def(11902)->
	[sc_activity_get_list,{list, 11903}];
decode_def(11904)->
	[p_activity_ext,int32,int32,string,string];
decode_def(11910)->
	[cs_activity_info,int16];
decode_def(11912)->
	[p_activity_draw,int16,string,int16,int16,int16,11930,{list, 12005}];
decode_def(11911)->
	[sc_activity_info,int16,int8,string,{list, 11912},int32,int32,int32,int8,int8];
decode_def(11913)->
	[cs_activity_month];
decode_def(11914)->
	[sc_activity_month,int32,int8,bool,int16,int16,int16];
decode_def(11915)->
	[cs_activity_month_buy];
decode_def(11916)->
	[sc_activity_month_buy,int8];
decode_def(11917)->
	[cs_activity_month_draw];
decode_def(11918)->
	[sc_activity_month_draw,int8];
decode_def(11920)->
	[cs_activity_draw,int16,int16];
decode_def(11921)->
	[sc_activity_draw,int8,int16,int16,int16,int16];
decode_def(11940)->
	[sc_activity_update,int16,int16,int16];
decode_def(11941)->
	[sc_activity_record_update,int16,int32];
decode_def(11942)->
	[p_energy_activity,int32,int32,int8,int8,int8,int8,int8];
decode_def(11943)->
	[cs_activity_energy];
decode_def(11944)->
	[sc_activity_energy,{list, 11942}];
decode_def(11945)->
	[cs_activity_sign_emperor_info];
decode_def(11946)->
	[sc_activity_sign_emperor_info,int8,int8,int8,int8,string];
decode_def(11947)->
	[cs_activity_sign_get_reward];
decode_def(11948)->
	[sc_activity_sign_get_reward,int8,{list, 12005}];
decode_def(11949)->
	[cs_activity_sign_up];
decode_def(11950)->
	[sc_activity_sign_up,int8,{list, 12005}];
decode_def(11951)->
	[cs_activity_rebate_info];
decode_def(11954)->
	[p_rebate_info,int8,int8,int32];
decode_def(11953)->
	[p_rebate_list,int8,string,int8,int32,int32,int32,{list, 11954}];
decode_def(11952)->
	[sc_rebate_info,int8,string,string,string,int32,int32,{list, 11953}];
decode_def(11955)->
	[cs_activity_rebate_get_reward,int8];
decode_def(11957)->
	[p_rebate_reward,int32,int32,int32];
decode_def(11956)->
	[sc_rebate_get_reward,int8,{list, 11957}];
decode_def(11958)->
	[sc_rebate_update];
decode_def(11959)->
	[cs_activity_levelRank_open];
decode_def(11961)->
	[levelRank_rankerInfo,int64,int8,string,11930];
decode_def(11960)->
	[sc_activity_levelRank_open,int32,int32,{list, 11961}];
decode_def(11962)->
	[cs_activity_levelRank_refresh];
decode_def(11963)->
	[sc_activity_levelRank_refresh,{list, 11961}];
decode_def(11964)->
	[cs_activity_day_pay_mul];
decode_def(11965)->
	[sc_activity_day_pay_mul,int8];
decode_def(11801)->
	[cs_invite_info];
decode_def(11802)->
	[sc_invite_info,bool,bool,int16,string,int16];
decode_def(11810)->
	[cs_invite_bind_weibo];
decode_def(11811)->
	[sc_invite_bind_weibo,int8];
decode_def(11812)->
	[cs_invite_weibo_share_levelup,int16];
decode_def(11813)->
	[sc_invite_weibo_share_levelup,int8];
decode_def(11820)->
	[cs_invite_input_invite_code,string];
decode_def(11821)->
	[sc_invite_input_invite_code,int8,string];
decode_def(11830)->
	[cs_invite_list];
decode_def(11832)->
	[p_invite,int32,bool,int8,int8,string,bool];
decode_def(11831)->
	[sc_invite_list,{list, 11832}];
decode_def(11701)->
	[cs_friend_get_list,int8];
decode_def(11703)->
	[p_friend,int32,bool,int8,int8,string,int64,int32,string,int32,int32,int32,int16,int8,int8,int32,int32,int32,int32,int32,bool];
decode_def(11702)->
	[sc_friend_get_list,int8,{list, 11703},int8,int8,int8];
decode_def(11704)->
	[cs_friend_fight,int32];
decode_def(11705)->
	[sc_friend_fight,int8,int32,int32,{list, 20002}];
decode_def(11706)->
	[cs_friend_get_add_list];
decode_def(11722)->
	[p_stranger,int32,bool,int8,int8,string,int64,int32,string,int32,int8];
decode_def(11707)->
	[sc_friend_get_add_list,{list, 11722}];
decode_def(11708)->
	[cs_friend_add,{list, int32}];
decode_def(11709)->
	[sc_friend_add,int8];
decode_def(11710)->
	[p_friend_add,int32,int32,bool,int8,int8,string,int64,int32];
decode_def(11711)->
	[cs_friend_add_list];
decode_def(11712)->
	[sc_friend_add_list,{list, 11710}];
decode_def(11713)->
	[sc_friend_new_add,{list, 11710}];
decode_def(11714)->
	[cs_friend_agree,{list, int32}];
decode_def(11715)->
	[sc_friend_agree,int8];
decode_def(11716)->
	[cs_friend_refuse,{list, int32}];
decode_def(11717)->
	[sc_friend_refuse,int8];
decode_def(11720)->
	[cs_friend_explore,string];
decode_def(11721)->
	[sc_friend_explore,{list, 11722}];
decode_def(11730)->
	[cs_friend_delete,int8,int32];
decode_def(11731)->
	[sc_friend_delete,int8,int8,int32];
decode_def(11732)->
	[sc_friend_notify_delete,int8,int32];
decode_def(11740)->
	[sc_friend_new,int8,11703];
decode_def(11741)->
	[cs_friend_send_enargy,{list, int32}];
decode_def(11742)->
	[sc_friend_send_enargy,int8,int32];
decode_def(11743)->
	[sc_friend_send_enargy_me,int32];
decode_def(11744)->
	[cs_friend_give_enargy,{list, int32}];
decode_def(11745)->
	[sc_friend_give_enargy,int8,{list, int32},int8];
decode_def(11746)->
	[sc_frend_give_enargy_me,int32,int8];
decode_def(11747)->
	[cs_friend_give_all_enargy];
decode_def(11748)->
	[sc_friend_remove_request];
decode_def(11601)->
	[cs_gather_get_list,int8];
decode_def(11602)->
	[sc_gather_get_list,int8,{list, int32}];
decode_def(11603)->
	[sc_gather_new,int8,{list, int32}];
decode_def(11501)->
	[cs_hist_get_list,int8];
decode_def(11502)->
	[sc_hist_get_list,int8,bool,{list, 11503},int16];
decode_def(11504)->
	[sc_hist_new,{list, 11503}];
decode_def(11520)->
	[cs_hist_replay,int64,int8];
decode_def(11521)->
	[sc_hist_replay,int8,{list, 20002}];
decode_def(11531)->
	[sc_hist_unreadNum,int8,int16];
decode_def(11401)->
	[cs_mail_info,int8,int64];
decode_def(11405)->
	[p_mail,int64,int8,int32,string,string,int32,int16,{list, any},{list, 11404}];
decode_def(11402)->
	[sc_mail_info,int8,int8,bool,{list, 11405},{list, int8}];
decode_def(11406)->
	[cs_mail_draw_reward,int64];
decode_def(11407)->
	[sc_mail_draw_reward,int8];
decode_def(11408)->
	[cs_mail_delete,int64,int8];
decode_def(11409)->
	[sc_mail_delete,int8];
decode_def(11410)->
	[cs_mail_new,int32,string,string];
decode_def(11411)->
	[sc_mail_new,int8];
decode_def(11420)->
	[cs_mail_unread_num];
decode_def(11421)->
	[sc_mail_unread_num,{list, int8}];
decode_def(11430)->
	[cs_mail_more,int8,int64];
decode_def(11431)->
	[sc_mail_more,int8,int8,{list, 11405},{list, int8}];
decode_def(11440)->
	[cs_mail_agree_friend,int64];
decode_def(11441)->
	[sc_mail_agree_friend,int8,int64];
decode_def(11442)->
	[cs_mail_del_spec_mail,int32];
decode_def(11443)->
	[sc_mail_del_spec_mail,{list, int64}];
decode_def(11301)->
	[cs_hron_info];
decode_def(11302)->
	[sc_hron_info,int16,int16,int16,int16,int8,bool,int32,int8,int8,int16];
decode_def(11303)->
	[cs_hron_buy,int8];
decode_def(11304)->
	[sc_hron_buy,int8,int16,int16];
decode_def(11305)->
	[cs_hron_fight];
decode_def(11306)->
	[sc_hron_fight,int8,int16,int16,int8,{list, 20002},{list, 10210}];
decode_def(11307)->
	[cs_hron_raids];
decode_def(11308)->
	[sc_hron_raids,int8,int16,{list, 10210}];
decode_def(11101)->
	[cs_card_get_list];
decode_def(11104)->
	[p_card,int8,int32];
decode_def(11103)->
	[p_opened_card,int8,int8,int32];
decode_def(11102)->
	[sc_card_get_list,bool,{list, 11103},{list, 11104},{list, int8},int32];
decode_def(11105)->
	[cs_card_draw,int8];
decode_def(11106)->
	[sc_card_draw,int8,int8,{list, 11104}];
decode_def(11107)->
	[cs_card_refresh];
decode_def(11108)->
	[sc_card_refresh,int8,{list, 11104}];
decode_def(11120)->
	[cs_card_onekey];
decode_def(11121)->
	[sc_card_onekey,int8,{list, 11103}];
decode_def(11001)->
	[cs_daily_get_list];
decode_def(11003)->
	[p_daily,int8,int8,bool];
decode_def(11002)->
	[sc_daily_get_list,{list, 11003},{list, int8}];
decode_def(11005)->
	[cs_daily_draw,int8];
decode_def(11006)->
	[sc_daily_draw,int8,11003,{list, int8}];
decode_def(11007)->
	[cs_daily_reward_list];
decode_def(11010)->
	[p_daily_reward,int32,bool,11404];
decode_def(11009)->
	[p_daily_reward_info,int8,int32,{list, 11010}];
decode_def(11008)->
	[sc_daily_reward_list,{list, 11009}];
decode_def(11011)->
	[cs_daily_reward_get,int8,int32];
decode_def(11012)->
	[sc_daily_reward_get,int8,int32,int8];
decode_def(11013)->
	[cs_daily_vip_info];
decode_def(11014)->
	[sc_daily_vip_info,bool,11404];
decode_def(11015)->
	[cs_daily_vip_draw];
decode_def(11016)->
	[sc_daily_vip_draw,int8];
decode_def(10801)->
	[cs_pvp_get_list];
decode_def(10803)->
	[p_pvp,int32,bool,int8,int8,string,int64,int16,int32];
decode_def(10802)->
	[sc_pvp_get_list,int16,{list, 10803}];
decode_def(10804)->
	[cs_pvp_fight,int32,int16];
decode_def(10805)->
	[sc_pvp_fight,int8,int16,int32,{list, 20002}];
decode_def(10806)->
	[cs_pvp_get_first_eight_replays];
decode_def(10808)->
	[p_pvp_replay_info,string,string,int16,int16,int64,int32];
decode_def(10807)->
	[sc_pvp_get_first_eight_replays,{list, 10808}];
decode_def(10809)->
	[cs_pvp_eight_replay,int64];
decode_def(10810)->
	[sc_pvp_eight_replay,int8,20002];
decode_def(10701)->
	[cs_shop_buy_num];
decode_def(10703)->
	[p_shop_num,int16,int16,int16];
decode_def(10702)->
	[sc_shop_buy_num,{list, 10703}];
decode_def(10704)->
	[cs_shop_buy,int16,int16,int8];
decode_def(10705)->
	[sc_shop_buy,int8];
decode_def(10710)->
	[cs_shop_encounter];
decode_def(10712)->
	[p_shop_random,int16,int32,{list, int16}];
decode_def(10711)->
	[sc_shop_encounter,{list, 10712}];
decode_def(10713)->
	[sc_shop_new,10712];
decode_def(10720)->
	[cs_shop_refresh,int16];
decode_def(10721)->
	[sc_shop_refresh,int8,{list, 10712}];
decode_def(10730)->
	[sc_shop_auto_refresh,10712];
decode_def(10731)->
	[cs_shop_treasure_info];
decode_def(10733)->
	[p_treasure,int8,int16,int32,int8,int32,bool,int8];
decode_def(10732)->
	[sc_shop_treasure_info,string,int32,int8,int16,int32,{list, 10733}];
decode_def(10734)->
	[cs_shop_treasure_buy,int8];
decode_def(10735)->
	[sc_shop_treasure_buy,int8,int8];
decode_def(10736)->
	[sc_shop_treasure_new_activity,string,int32,int8,int16];
decode_def(10737)->
	[sc_shop_treasure_new_shop,int32,{list, 10733}];
decode_def(10738)->
	[cs_shop_refresh2];
decode_def(10739)->
	[sc_shop_refresh2,int8,{list, 10733}];
decode_def(10601)->
	[cs_item_bag];
decode_def(10603)->
	[p_item,int64,int16,int8,int8,int16,int32,int16];
decode_def(10602)->
	[sc_item_bag,{list, 10603}];
decode_def(10604)->
	[cs_item_equip];
decode_def(10605)->
	[sc_item_equip,{list, 10606}];
decode_def(10607)->
	[cs_item_sell,{list, int64}];
decode_def(10608)->
	[sc_item_sell,int8,{list, 12010},int32];
decode_def(10609)->
	[cs_item_down_equip,int64,int8];
decode_def(10610)->
	[sc_item_down_equip,int8,int64,int8];
decode_def(10611)->
	[cs_item_up_equip,int64,int8,int64,int64];
decode_def(10612)->
	[sc_item_up_equip,int8,int64,int8,int64];
decode_def(10613)->
	[sc_item_new,{list, 10603}];
decode_def(10615)->
	[p_item_num_update,int64,int16];
decode_def(10614)->
	[sc_item_update,{list, 10615}];
decode_def(10617)->
	[cs_item_use,int64,int8];
decode_def(10618)->
	[sc_item_use,int8,int64,int8];
decode_def(10619)->
	[sc_item_delete_notify,{list, int64}];
decode_def(10620)->
	[cs_item_reinforce,int64,int64];
decode_def(10621)->
	[sc_item_reinforce,int8,int64,int16];
decode_def(10622)->
	[cs_item_max_reinforce,int64,int64];
decode_def(10623)->
	[sc_item_max_reinforce,int8,int64,{list, int16}];
decode_def(10624)->
	[sc_item_update_rank,int64,int8,int32];
decode_def(10625)->
	[cs_item_up_rank,int64,int64,int64,int64];
decode_def(10626)->
	[sc_item_up_rank,int8,int64,int64,int8,int8];
decode_def(10627)->
	[sc_item_more,{list, 10603}];
decode_def(10631)->
	[cs_item_compound,int16];
decode_def(10632)->
	[sc_item_compound,int8,int16];
decode_def(10633)->
	[cs_item_eat,int64,int64,{list, int64}];
decode_def(10634)->
	[sc_item_eat,int8,int64,int8,int16];
decode_def(10636)->
	[p_all_equipment,int32,{list, int32}];
decode_def(10635)->
	[sc_item_all_equipment,{list, int64},{list, 10636}];
decode_def(10637)->
	[cs_item_use_info];
decode_def(10639)->
	[p_item_use_info,int16,int8];
decode_def(10638)->
	[sc_item_use_info,{list, 10639}];
decode_def(10640)->
	[cs_item_up_all_equip,int64];
decode_def(10641)->
	[sc_item_up_all_equip,int8];
decode_def(10501)->
	[cs_explore_one];
decode_def(10502)->
	[sc_explore_one,int8,{list, 11403},int32,int32,int32,int8];
decode_def(10503)->
	[p_echapter,int16,int32,int32,bool];
decode_def(10504)->
	[cs_explore_dungeon_list,int16];
decode_def(10506)->
	[p_edungeon,int16,bool];
decode_def(10505)->
	[sc_explore_dungeon_list,int16,{list, 10506},int8];
decode_def(10507)->
	[cs_explore_challenge_encounter,int16];
decode_def(10508)->
	[sc_explore_challenge_encounter,int8,{list, 20002},{list, 10210},int8,int8];
decode_def(10509)->
	[sc_explore_delete_encounter,int16];
decode_def(10510)->
	[cs_explore_giveup_encounter,int16];
decode_def(10511)->
	[sc_explore_giveup_encounter,int8];
decode_def(10512)->
	[cs_explore_list];
decode_def(10513)->
	[sc_explore_list,{list, 10503}];
decode_def(10514)->
	[cs_explore_collect,int16];
decode_def(10515)->
	[sc_explore_collect,int16,int8];
decode_def(10516)->
	[cs_explore_force_collect,int16,int8];
decode_def(10517)->
	[sc_explore_force_collect,int16,int8,int8];
decode_def(10518)->
	[cs_explore_auto_explore_check];
decode_def(10519)->
	[sc_explore_auto_explore_check,int8];
decode_def(10520)->
	[cs_explore_encounter_pass_reward,int16];
decode_def(10521)->
	[sc_explore_encounter_pass_reward,int8];
decode_def(10522)->
	[cs_explore_encounter_dungeon_state,int16];
decode_def(10523)->
	[sc_explore_encounter_dungeon_state,int8,int16,int16,int8];
decode_def(10530)->
	[cs_explore_free];
decode_def(10531)->
	[sc_explore_free,int8];
decode_def(10401)->
	[cs_ger_info];
decode_def(10402)->
	[sc_ger_info,{list, 10403}];
decode_def(10404)->
	[p_ger_pos_info,int64,int8,{list, int64}];
decode_def(10405)->
	[sc_ger_update,int64,int16,int16,int32,int32,int64,int64];
decode_def(10406)->
	[sc_ger_new,10403];
decode_def(10407)->
	[cs_ger_standup,int8,int64];
decode_def(10408)->
	[sc_ger_standup,int8,int8,int64];
decode_def(10409)->
	[cs_ger_move_pos,int8,int8];
decode_def(10410)->
	[sc_ger_move_pos,int8,int8,int8];
decode_def(10411)->
	[cs_ger_pos_list];
decode_def(10412)->
	[sc_ger_pos_list,{list, 10404}];
decode_def(10414)->
	[cs_ger_sell,{list, int64}];
decode_def(10415)->
	[sc_ger_sell,int8,{list, 12010}];
decode_def(10416)->
	[cs_ger_detail,int64];
decode_def(10417)->
	[sc_ger_detail,int64,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16,int16];
decode_def(10419)->
	[cs_ger_view_other,int32,int16];
decode_def(10420)->
	[sc_ger_view_other,int32,string,int16,int64,{list, 10418}];
decode_def(10421)->
	[sc_ger_update_exp,int64,int64];
decode_def(10422)->
	[cs_ger_eat,int64,{list, int64}];
decode_def(10423)->
	[sc_ger_eat,int8,int64];
decode_def(10424)->
	[cs_ger_up_rank,int64,int64];
decode_def(10425)->
	[sc_ger_up_rank,int8,int64,int64];
decode_def(10426)->
	[sc_ger_update_standlist,{list, 10413}];
decode_def(10427)->
	[cs_ger_down,int64,int64,int64,int64,int64,int64,int64,int64,int64,int64,int64,int64];
decode_def(10428)->
	[sc_ger_down,int8,int64];
decode_def(10429)->
	[sc_ger_more,{list, 10403}];
decode_def(10430)->
	[sc_ger_del,{list, int64}];
decode_def(10441)->
	[p_ger_power,int8,int64];
decode_def(10440)->
	[sc_ger_refresh_power,{list, 10441}];
decode_def(10442)->
	[cs_ger_view_other_dtl,int32,int16];
decode_def(10443)->
	[sc_ger_view_other_dtl,int32,string,int16,int64,{list, 10403},{list, 10606},{list, 10413},int16,int16,{list, 10471}];
decode_def(10444)->
	[cs_ger_guard_info];
decode_def(10455)->
	[p_ger_guard_attr,int8,int32];
decode_def(10454)->
	[p_ger_guard,int8,int64,int16,int8,{list, 10455},{list, 10455}];
decode_def(10445)->
	[sc_ger_guard_info,{list, 10454}];
decode_def(10446)->
	[cs_ger_guard_set,int64,int8];
decode_def(10447)->
	[sc_ger_guard_set,int8,int64,int8,{list, 10455}];
decode_def(10448)->
	[cs_ger_guard_refresh,int8,{list, bool}];
decode_def(10449)->
	[sc_ger_guard_refresh,int8,{list, 10455}];
decode_def(10450)->
	[cs_ger_lieu_pos_list];
decode_def(10451)->
	[sc_ger_lieu_pos_list,{list, 10404}];
decode_def(10452)->
	[cs_ger_lieu_standup,int8,int64];
decode_def(10453)->
	[sc_ger_lieu_standup,int8,int8,int64];
decode_def(10456)->
	[cs_ger_lieu_untie,int8];
decode_def(10458)->
	[p_ger_lieu_info,int8,int16,int8,int16,int8,int16,int8];
decode_def(10457)->
	[sc_ger_lieu_untie,int8,{list, 10458}];
decode_def(10459)->
	[cs_ger_lieu_info_list];
decode_def(10460)->
	[sc_ger_lieu_info_list,{list, 10458}];
decode_def(10461)->
	[cs_ger_lieu_move_pos,int8,int8];
decode_def(10462)->
	[sc_ger_lieu_move_pos,int8,int8,int8];
decode_def(10463)->
	[cs_ger_lieu_lock_clo,int8,int8];
decode_def(10464)->
	[sc_ger_lieu_lock_clo,int8];
decode_def(10465)->
	[cs_ger_lieu_unlock_clo,int8,int8];
decode_def(10466)->
	[sc_ger_lieu_unlock_clo,int8];
decode_def(10467)->
	[cs_ger_lieu_refresh_clo,int8];
decode_def(10468)->
	[sc_ger_lieu_refresh_clo,int8,10458];
decode_def(10469)->
	[cs_ger_lieu_tie_info];
decode_def(10470)->
	[sc_ger_lieu_tie_info,{list, int8}];
decode_def(10472)->
	[cs_ger_lieu_refresh_freeTimes];
decode_def(10473)->
	[sc_ger_lieu_refresh_freeTimes,int16];
decode_def(10474)->
	[sc_ger_new_list,{list, 10403}];
decode_def(10475)->
	[cs_ger_down_rank,int64];
decode_def(10476)->
	[sc_ger_down_rank,int8,{list, 10418}];
decode_def(10302)->
	[cs_message_notice,int32];
decode_def(10304)->
	[p_notice,int32,string,string];
decode_def(10303)->
	[sc_message_notice,int8,{list, int32},{list, 10304},{list, 11903},{list, 11911},{list, 11904},{list, int32}];
decode_def(10305)->
	[cs_message_certain_notice,int32];
decode_def(10306)->
	[sc_message_certain_notice,{list, 10304}];
decode_def(10301)->
	[sc_message_bc,string];
decode_def(10307)->
	[sc_message_bc_id,int16];
decode_def(10308)->
	[sc_message_bc_id2,int16,{list, any}];
decode_def(10330)->
	[cs_message_test,string];
decode_def(10331)->
	[sc_message_test,int8,string];
decode_def(30001)->
	[sc_message_best_card,string,int8,int32];
decode_def(30002)->
	[sc_message_ger_upLevel,string,10418];
decode_def(30003)->
	[sc_message_item_uprank,string,10616];
decode_def(10201)->
	[cs_battle_progress];
decode_def(10216)->
	[p_battle_progress,int8,int16,int16,int16];
decode_def(10202)->
	[sc_battle_progress,{list, 10216},{list, int16}];
decode_def(10203)->
	[cs_battle_info,int8,int16];
decode_def(10205)->
	[p_dungeon,int16,int16,int8];
decode_def(10204)->
	[sc_battle_info,int8,int16,bool,{list, 10205},int16];
decode_def(10206)->
	[cs_battle_challenge,int8,int16];
decode_def(10207)->
	[sc_battle_challenge,int8,{list, 20002},{list, 10210},int8];
decode_def(10208)->
	[cs_battle_perfect_reward,int16];
decode_def(10209)->
	[sc_battle_perfect_reward,int8];
decode_def(10212)->
	[sc_battle_broadcast_get_item,string,int32,int8,int16,int16];
decode_def(10214)->
	[cs_battle_dungeon_raids,int16];
decode_def(10215)->
	[sc_battle_dungeon_raids,int8,int8,{list, 10210}];
decode_def(10217)->
	[cs_battle_coin_info];
decode_def(10218)->
	[sc_battle_coin_info,int16,int32];
decode_def(10219)->
	[cs_battle_coin_fight,int8];
decode_def(10220)->
	[sc_battle_coin_fight,int8,int32,bool,int32,{list, 20002}];
decode_def(10101)->
	[cs_role_info];
decode_def(10102)->
	[sc_role_info,int32,string,bool,string,int32,int16,int64,int32,int32,int32,int32,int32,int8,int32,int16,int16,int32,int8,int32,int8,int8,int8,int8,int8,int8,bool,bool,int16,int16,int16,int16,int8,int32,int32,int16,int16,int32,int32,int32,bool,int8,int32,{list, int16}];
decode_def(10107)->
	[sc_role_update_level,int16];
decode_def(10103)->
	[sc_role_update_list,{list, 10107}];
decode_def(10105)->
	[cs_role_buy_energy,int8];
decode_def(10106)->
	[sc_role_buy_energy,int8,int8,int16,int16,int32,int32,{list, 20002}];
decode_def(10108)->
	[sc_role_update_exp,int64];
decode_def(10109)->
	[sc_role_update_coin,int32];
decode_def(10110)->
	[sc_role_update_reputation,int32];
decode_def(10111)->
	[sc_role_update_gold,int32];
decode_def(10112)->
	[sc_role_update_goldBonus,int32];
decode_def(10113)->
	[sc_role_update_vipLevel,int8,int16,int16];
decode_def(10114)->
	[sc_role_update_energy,int16,int32];
decode_def(10115)->
	[sc_role_update_discoveryTimes,int8,int32];
decode_def(10116)->
	[sc_role_update_pvpTimes,int8,int32];
decode_def(10117)->
	[sc_role_update_ruleTimes,int8,int32];
decode_def(10118)->
	[sc_role_update_randomPVPTimes,int8];
decode_def(10119)->
	[sc_role_update_singlePVPTimes,int8];
decode_def(10120)->
	[sc_role_update_goldUsed,int32];
decode_def(10121)->
	[sc_role_update_title,int8];
decode_def(10122)->
	[sc_role_update_encounterFreeNum,int8];
decode_def(10123)->
	[sc_role_update_weiboCount,int8];
decode_def(10124)->
	[cs_role_setting];
decode_def(10125)->
	[sc_role_setting,{list, int8}];
decode_def(10126)->
	[cs_role_get_energy,int8];
decode_def(10127)->
	[sc_role_get_energy,int8];
decode_def(10128)->
	[cs_role_buy_coin_value];
decode_def(10129)->
	[sc_role_buy_coin_value,int32];
decode_def(10130)->
	[cs_role_weixin_share];
decode_def(10131)->
	[sc_role_update_pay_ext,int32];
decode_def(10132)->
	[cs_role_suggest_open];
decode_def(10133)->
	[sc_role_suggest_open,bool];
decode_def(10134)->
	[cs_role_suggest,string,string];
decode_def(10135)->
	[sc_role_suggest,int8];
decode_def(10136)->
	[cs_role_log_guide_state,int16];
decode_def(10137)->
	[cs_role_pay_tencent,int16];
decode_def(10138)->
	[sc_role_pay_tencent,int8,int32,bool];
decode_def(10139)->
	[cs_role_login_reward];
decode_def(10140)->
	[sc_role_login_reward,{list, 11404}];
decode_def(10141)->
	[cs_role_change_name,string];
decode_def(10142)->
	[sc_role_change_name,int8];
decode_def(10143)->
	[cs_role_check_tencent];
decode_def(10144)->
	[sc_role_check_tencent,int8];
decode_def(10150)->
	[cs_role_push_setting,int8,int16];
decode_def(10151)->
	[sc_role_push_setting,int8,int8,int16];
decode_def(10153)->
	[cs_role_get_guide_state];
decode_def(10154)->
	[sc_role_get_guide_state,int16];
decode_def(10155)->
	[cs_role_set_guide_state,int16];
decode_def(10156)->
	[sc_role_set_guide_state,int8];
decode_def(10157)->
	[cs_role_change_head,int32];
decode_def(10158)->
	[sc_role_change_head,int8,int32];
decode_def(10159)->
	[cs_role_change_location,string];
decode_def(10180)->
	[cs_role_token,string];
decode_def(10181)->
	[sc_role_token];
decode_def(10182)->
	[cs_role_select_ger,int16];
decode_def(10183)->
	[sc_role_select_ger,int8];
decode_def(10184)->
	[cs_role_demo_fight,int8];
decode_def(10185)->
	[sc_role_demo_fight,int8,{list, 20002}];
decode_def(10186)->
	[sc_role_base_config,int8,int8];
decode_def(10190)->
	[cs_role_pay_ios,string,int32,string,string,int8];
decode_def(10191)->
	[sc_role_pay_ios,int8,string,int32,bool];
decode_def(10192)->
	[cs_role_pay_91,string,int32,string,string];
decode_def(10193)->
	[sc_role_pay_91,int8,string,int32,bool];
decode_def(10194)->
	[sc_role_pay_uc,int8,int32,bool];
decode_def(10195)->
	[sc_role_pay_dl,int8,int32,bool];
decode_def(10196)->
	[sc_role_pay_zz,int8,int32,bool];
decode_def(10197)->
	[sc_role_pay_360,int8,int32,bool];
decode_def(10198)->
	[sc_role_pay_wdj,int8,int32,bool];
decode_def(10199)->
	[sc_role_pay_dk,int8,int32,bool];
decode_def(10189)->
	[sc_role_pay_mi,int8,int32,bool];
decode_def(10188)->
	[sc_role_pay_az,int8,int32,bool];
decode_def(10001)->
	[cs_account_login,int32,int32,string,string,string,int16,string,int16];
decode_def(10002)->
	[sc_account_login,int16,bool,bool,int8];
decode_def(10003)->
	[cs_account_create,string,int8];
decode_def(10004)->
	[sc_account_create,int8];
decode_def(10005)->
	[cs_account_enter_game];
decode_def(10006)->
	[sc_account_enter_game,int8];
decode_def(10007)->
	[sc_account_kick,int8];
decode_def(10013)->
	[cs_account_heart];
decode_def(10014)->
	[sc_account_heart,int32];
decode_def(10015)->
	[cs_account_logout];
decode_def(10016)->
	[cs_account_check_rolename,string];
decode_def(10017)->
	[sc_account_check_rolename,int8];
decode_def(10018)->
	[cs_account_pay_arg,int32,string,string,string,string,string];
decode_def(10019)->
	[sc_account_pay_arg];
decode_def(10020)->
	[sc_account_update_pay_arg];
decode_def(_) -> [].

get_id(cs_melee_info)->20301;
get_id(sc_melee_info)->20302;
get_id(cs_melee_sign)->20303;
get_id(sc_melee_sign)->20304;
get_id(cs_melee_fight)->20305;
get_id(p_id_num)->11403;
get_id(p_mail_reward)->11404;
get_id(p_action)->20004;
get_id(p_fighter)->20003;
get_id(sc_fight_request)->20002;
get_id(sc_melee_fight)->20306;
get_id(p_homestead_log)->20201;
get_id(p_homestead_machine)->20202;
get_id(p_homestead)->20203;
get_id(sc_homestead_error)->20204;
get_id(cs_homestead_get_info)->20205;
get_id(sc_homestead_get_info)->20206;
get_id(cs_homestead_get_friend_info)->20207;
get_id(sc_homestead_get_friend_info)->20208;
get_id(cs_homestead_unlock_machine)->20209;
get_id(sc_homestead_unlock_machine)->20210;
get_id(cs_homestead_uproot_seed)->20211;
get_id(sc_homestead_uproot_seed)->20212;
get_id(cs_homestead_harvest)->20213;
get_id(p_reward_view)->12005;
get_id(sc_homestead_harvest)->20214;
get_id(cs_homestead_seeding)->20215;
get_id(sc_homestead_seeding)->20216;
get_id(sc_homestead_update_machine)->20217;
get_id(cs_homestead_change_ger)->20218;
get_id(sc_homestead_change_ger)->20219;
get_id(cs_homestead_mating)->20220;
get_id(sc_homestead_mating)->20221;
get_id(sc_homestead_mating_to_friend)->20222;
get_id(cs_homestead_addenergy)->20223;
get_id(sc_homestead_addenergy)->20224;
get_id(sc_homestead_addenergy_to_friend)->20225;
get_id(cs_homestead_get_log)->20226;
get_id(sc_homestead_get_log)->20227;
get_id(cs_homestead_get_friend_log)->20228;
get_id(sc_homestead_get_friend_log)->20229;
get_id(sc_homestead_sync_mating_cool_second)->20230;
get_id(sc_homestead_sync_ger)->20231;
get_id(sc_homestead_sync_add_enagy)->20232;
get_id(p_task)->20101;
get_id(cs_task_get_info)->20103;
get_id(sc_task_get_info)->20104;
get_id(cs_task_operate)->20105;
get_id(sc_task_operate)->20106;
get_id(sc_task_error)->20107;
get_id(sc_task_notify_change)->20108;
get_id(cs_fight_request)->20001;
get_id(cs_road_info)->14101;
get_id(sc_road_info)->14102;
get_id(cs_road_reset)->14103;
get_id(sc_road_reset)->14104;
get_id(cs_road_fight)->14105;
get_id(sc_road_fight)->14106;
get_id(cs_road_fight_ext)->14107;
get_id(sc_road_fight_ext)->14108;
get_id(cs_road_box)->14109;
get_id(sc_road_box)->14110;
get_id(cs_hula_open)->14001;
get_id(p_hula_boss_open)->14003;
get_id(sc_hula_open)->14002;
get_id(p_hula_boss_state)->14005;
get_id(sc_hula_init_state)->14004;
get_id(cs_hula_close)->14006;
get_id(p_hula_info)->14007;
get_id(cs_hula_cur_info)->14008;
get_id(sc_hula_cur_info)->14009;
get_id(sc_hula_hp_sync)->14010;
get_id(p_hula_harm)->14012;
get_id(sc_hula_harm_broadcast)->14011;
get_id(p_role_stastic)->14022;
get_id(sc_hula_stop)->14013;
get_id(cs_hula_rank_sync)->14014;
get_id(sc_hula_rank_sync)->14015;
get_id(cs_hula_fight)->14016;
get_id(sc_hula_fight)->14017;
get_id(cs_hula_reborn)->14018;
get_id(sc_hula_reborn)->14019;
get_id(cs_hula_open_time)->14020;
get_id(sc_hula_open_time)->14021;
get_id(cs_festival_info)->13901;
get_id(p_festival_rank_reward)->13908;
get_id(sc_festival_info)->13902;
get_id(cs_festival_click)->13903;
get_id(sc_festival_click)->13904;
get_id(cs_festival_rank)->13905;
get_id(p_festival_rank)->13907;
get_id(sc_festival_rank)->13906;
get_id(cs_rule_info)->13801;
get_id(p_rule_fighter)->13809;
get_id(p_hist)->11503;
get_id(sc_rule_info)->13802;
get_id(cs_rule_rank)->13803;
get_id(sc_rule_rank)->13804;
get_id(cs_rule_last_rank)->13805;
get_id(sc_rule_last_rank)->13806;
get_id(cs_rule_fight)->13807;
get_id(sc_rule_fight)->13808;
get_id(cs_rule_leave)->13810;
get_id(cs_rank_info)->13701;
get_id(p_rank_info)->13703;
get_id(sc_rank_info)->13702;
get_id(cs_alien_info)->13601;
get_id(p_alien_fighter)->13606;
get_id(sc_alien_info)->13602;
get_id(sc_alien_sign_info)->13603;
get_id(cs_alien_first_five)->13604;
get_id(sc_alien_first_five)->13605;
get_id(cs_alien_kill_num_rank)->13607;
get_id(p_alien_fighter2)->13609;
get_id(sc_alien_kill_num_rank)->13608;
get_id(cs_alien_kill_continuous_rank)->13610;
get_id(p_alien_fighter3)->13612;
get_id(sc_alien_kill_continuous_rank)->13611;
get_id(cs_alien_guess_info)->13613;
get_id(sc_alien_guess_info)->13614;
get_id(cs_alien_guess)->13615;
get_id(sc_alien_guess)->13616;
get_id(cs_alien_reset)->13617;
get_id(sc_alien_reset)->13618;
get_id(cs_alien_fight)->13619;
get_id(sc_alien_fight)->13620;
get_id(cs_alien_sign)->13621;
get_id(sc_alien_sign)->13622;
get_id(cs_alien_self_record)->13623;
get_id(p_alien_self_record)->13627;
get_id(sc_alien_self_record)->13624;
get_id(cs_alien_record)->13625;
get_id(p_alien_record)->13628;
get_id(sc_alien_record)->13626;
get_id(sc_alien_update_times)->13629;
get_id(cs_alien_self_fight_replay)->13630;
get_id(sc_alien_self_fight_replay)->13631;
get_id(cs_alien_fight_replay)->13632;
get_id(sc_alien_fight_repaly)->13633;
get_id(sc_alien_new_fighter_list)->13634;
get_id(cs_alien_leave)->13635;
get_id(sc_alien_new_self_record)->13636;
get_id(cs_alien_view_other)->13637;
get_id(p_ger_view)->10418;
get_id(sc_alien_view_other)->13638;
get_id(cs_alien_view_other_dtl)->13639;
get_id(cs_alien_buy_times)->13640;
get_id(sc_alien_buy_times)->13641;
get_id(cs_alien_active)->13642;
get_id(sc_alien_active)->13643;
get_id(cs_team_pk_info)->13501;
get_id(p_team_member)->13503;
get_id(sc_team_pk_open)->13502;
get_id(sc_team_pk_close)->13504;
get_id(cs_team_refresh)->13505;
get_id(sc_team_refresh)->13506;
get_id(cs_team_fight)->13507;
get_id(p_team_member2)->13509;
get_id(sc_team_fight_result)->13508;
get_id(cs_team_rank)->13510;
get_id(p_team_member3)->13512;
get_id(sc_team_rank)->13511;
get_id(cs_team_record)->13513;
get_id(p_team_record)->13515;
get_id(sc_team_record)->13514;
get_id(cs_team_self_record)->13516;
get_id(p_team_self_record)->13518;
get_id(sc_team_self_record)->13517;
get_id(cs_team_move)->13519;
get_id(sc_team_move)->13520;
get_id(sc_team_pk_not_open)->13521;
get_id(sc_team_fight_error)->13522;
get_id(cs_team_fight_replay)->13523;
get_id(sc_team_fight_replay)->13524;
get_id(cs_team_self_fight_replay)->13525;
get_id(sc_team_self_fight_replay)->13526;
get_id(cs_team_view_other)->13527;
get_id(sc_team_view_other)->13528;
get_id(cs_team_view_other_dtl)->13529;
get_id(p_lieu_view)->10471;
get_id(p_equip)->10606;
get_id(p_ger_pos)->10413;
get_id(p_ger)->10403;
get_id(sc_team_view_other_dtl)->13530;
get_id(cs_team_new_status)->13531;
get_id(sc_team_new_status)->13532;
get_id(p_race_rec)->13402;
get_id(sc_race_new_fight)->13401;
get_id(p_race_fighter)->13403;
get_id(cs_race_history)->13404;
get_id(sc_race_history)->13405;
get_id(cs_race_replay)->13406;
get_id(sc_race_replay)->13407;
get_id(cs_race_fight_list)->13408;
get_id(sc_race_fight_list)->13409;
get_id(cs_race_sign)->13410;
get_id(sc_race_sign)->13411;
get_id(cs_race_info)->13412;
get_id(p_race_pos)->13416;
get_id(sc_race_info)->13413;
get_id(cs_race_enter)->13414;
get_id(cs_race_leave)->13415;
get_id(cs_race_pos_history)->13417;
get_id(sc_race_pos_history)->13418;
get_id(sc_race_new_first)->13419;
get_id(sc_race_new_status)->13420;
get_id(cs_race_is_open)->13421;
get_id(sc_race_is_open)->13422;
get_id(cs_race_auto_sign)->13423;
get_id(sc_race_auto_sign)->13424;
get_id(cs_race_auto_unsign)->13425;
get_id(sc_race_auto_unsign)->13426;
get_id(cs_race_self_history)->13427;
get_id(sc_race_self_history)->13428;
get_id(cs_race_guess_info)->13429;
get_id(sc_race_guess_info)->13430;
get_id(cs_race_guess)->13431;
get_id(sc_race_guess)->13432;
get_id(p_family_member_info)->13302;
get_id(p_family_info)->13301;
get_id(p_family_request)->13303;
get_id(p_family_summary)->13304;
get_id(cs_family_get_list)->13305;
get_id(sc_family_get_list)->13306;
get_id(cs_family_create)->13307;
get_id(sc_family_create)->13308;
get_id(cs_family_request_join)->13309;
get_id(sc_family_request_join)->13310;
get_id(cs_family_cancel_join)->13311;
get_id(sc_family_cancel_join)->13312;
get_id(cs_family_agree_join)->13313;
get_id(sc_family_agree_join)->13314;
get_id(cs_family_refuse_join)->13315;
get_id(sc_family_refuse_join)->13316;
get_id(cs_family_get_info)->13317;
get_id(sc_family_get_info)->13318;
get_id(cs_family_kick)->13319;
get_id(sc_family_kick)->13320;
get_id(cs_family_create_consume)->13321;
get_id(sc_family_create_consume)->13322;
get_id(cs_family_leave)->13323;
get_id(sc_family_leave)->13324;
get_id(cs_family_change_notice)->13325;
get_id(sc_family_change_notice)->13326;
get_id(cs_family_request_list)->13327;
get_id(sc_family_request_list)->13328;
get_id(sc_family_del_request)->13329;
get_id(cs_combine_do)->13201;
get_id(sc_combine_fail)->13202;
get_id(p_newGer)->13205;
get_id(sc_combine_ger)->13203;
get_id(p_newEquip)->13206;
get_id(sc_combine_equip)->13204;
get_id(cs_combine_info)->13207;
get_id(sc_combine_info)->13208;
get_id(cs_firecracker_open)->13101;
get_id(p_discount)->13103;
get_id(sc_firecracker_open)->13102;
get_id(sc_firecracker_info_sync)->13104;
get_id(cs_firecracker_close)->13105;
get_id(cs_firecracker_setoff)->13106;
get_id(p_item_view)->10616;
get_id(p_reward_info)->11930;
get_id(sc_firecracker_setoff)->13107;
get_id(cs_firecracker_rank)->13108;
get_id(p_firecracker_rank)->13110;
get_id(sc_firecracker_rank)->13109;
get_id(cs_firecracker_get_reward)->13111;
get_id(sc_firecracker_get_reward)->13112;
get_id(cs_treaHouse_get_list)->13001;
get_id(p_treaHouse_card)->13002;
get_id(p_baseBoxOpenInfo)->13022;
get_id(sc_treaHouse_get_list)->13003;
get_id(cs_treaHouse_is_open)->13004;
get_id(sc_treaHouse_is_open)->13005;
get_id(cs_treaHouse_explore_one)->13006;
get_id(p_treaHouse_card_oneTime)->13008;
get_id(sc_treaHouse_explore_one)->13007;
get_id(cs_treaHouse_explore_ten)->13009;
get_id(sc_treaHouse_explore_ten)->13010;
get_id(cs_treaHouse_refresh)->13011;
get_id(sc_treaHouse_refresh)->13012;
get_id(cs_treaHouse_open_base_box)->13013;
get_id(sc_treaHouse_open_base_box)->13014;
get_id(p_treaHouse_ranker)->13015;
get_id(cs_treaHouse_get_rankInfo)->13016;
get_id(sc_treaHouse_get_rankInfo)->13017;
get_id(cs_treaHouse_get_rank_Reward)->13018;
get_id(sc_treaHouse_get_rank_Reward)->13019;
get_id(cs_treaHouse_get_baseBoxRewardInfo)->13020;
get_id(p_treaHouse_BaseReward_Info)->13023;
get_id(sc_treaHouse_get_baseBoxRewardInfo)->13021;
get_id(sc_treaHouse_change_state)->13024;
get_id(cs_challengeGod_info)->12701;
get_id(sc_challengeGod_info)->12702;
get_id(cs_challengeGod_select_ger)->12703;
get_id(sc_challengeGod_select_ger)->12704;
get_id(cs_challengeGod_challenge_dungeon_one)->12705;
get_id(p_ger_add_exp)->10211;
get_id(p_reward)->10210;
get_id(sc_challengeGod_challenge_dungeon_one)->12706;
get_id(cs_challengeGod_challenge_dungeon_ten)->12707;
get_id(p_challengeGod_result)->12708;
get_id(sc_challengeGod_challenge_ten)->12709;
get_id(cs_talk_world)->12601;
get_id(sc_talk_world)->12602;
get_id(sc_talk_message)->12603;
get_id(cs_talk_gag_one)->12604;
get_id(cs_talk_ungag_one)->12605;
get_id(cs_talk_get_gag_list)->12606;
get_id(sc_talk_get_gag_list)->12607;
get_id(cs_talk_recent_list)->12608;
get_id(sc_talk_recent_list)->12609;
get_id(cs_talk_person)->12610;
get_id(sc_talk_person)->12611;
get_id(cs_talk_person_offline)->12612;
get_id(sc_talk_person_offline)->12613;
get_id(cs_talk_gm)->12614;
get_id(sc_talk_gm)->12615;
get_id(sc_push_highlight_Info)->12501;
get_id(cs_nanm_open)->12401;
get_id(sc_nanm_open)->12402;
get_id(sc_nanm_init_state)->12403;
get_id(cs_nanm_close)->12404;
get_id(cs_nanm_buff)->12405;
get_id(sc_nanm_buff)->12406;
get_id(cs_nanm_last_info)->12411;
get_id(sc_nanm_last_info_ignore)->12412;
get_id(p_nanm_info)->12415;
get_id(sc_nanm_last_info_win)->12413;
get_id(sc_nanm_last_info_fail)->12414;
get_id(cs_nanm_cur_info)->12416;
get_id(sc_nanm_cur_info_ignore)->12417;
get_id(sc_nanm_cur_info)->12418;
get_id(sc_nanm_hp_sync)->12420;
get_id(p_nanm_harm)->12422;
get_id(sc_nanm_harm_broadcast)->12421;
get_id(sc_nanm_buff_sync)->12424;
get_id(sc_nanm_stop)->12425;
get_id(cs_nanm_rank_sync)->12426;
get_id(sc_nanm_rank_sync)->12427;
get_id(cs_nanm_fight)->12431;
get_id(sc_nanm_fight)->12432;
get_id(cs_nanm_reborn)->12441;
get_id(sc_nanm_reborn)->12442;
get_id(cs_nanm_offline_play)->12443;
get_id(sc_nanm_offline_play)->12444;
get_id(cs_nanm_open_time)->12450;
get_id(sc_nanm_open_time)->12451;
get_id(cs_version)->12301;
get_id(sc_version)->12302;
get_id(cs_gift_request)->12201;
get_id(sc_gift_request)->12202;
get_id(cs_box_item)->12001;
get_id(sc_box_item)->12002;
get_id(cs_box_shop)->12003;
get_id(p_reward_view2)->12006;
get_id(sc_box_shop)->12004;
get_id(cs_box_shop_info)->12007;
get_id(p_shop_box_info)->12009;
get_id(sc_box_shop_info)->12008;
get_id(p_reward_view3)->12010;
get_id(cs_box_get_spirit_equip_count)->12011;
get_id(sc_box_get_spirit_equip_count)->12012;
get_id(cs_activity_get_list)->11901;
get_id(p_activity_icon)->11903;
get_id(sc_activity_get_list)->11902;
get_id(p_activity_ext)->11904;
get_id(cs_activity_info)->11910;
get_id(p_activity_draw)->11912;
get_id(sc_activity_info)->11911;
get_id(cs_activity_month)->11913;
get_id(sc_activity_month)->11914;
get_id(cs_activity_month_buy)->11915;
get_id(sc_activity_month_buy)->11916;
get_id(cs_activity_month_draw)->11917;
get_id(sc_activity_month_draw)->11918;
get_id(cs_activity_draw)->11920;
get_id(sc_activity_draw)->11921;
get_id(sc_activity_update)->11940;
get_id(sc_activity_record_update)->11941;
get_id(p_energy_activity)->11942;
get_id(cs_activity_energy)->11943;
get_id(sc_activity_energy)->11944;
get_id(cs_activity_sign_emperor_info)->11945;
get_id(sc_activity_sign_emperor_info)->11946;
get_id(cs_activity_sign_get_reward)->11947;
get_id(sc_activity_sign_get_reward)->11948;
get_id(cs_activity_sign_up)->11949;
get_id(sc_activity_sign_up)->11950;
get_id(cs_activity_rebate_info)->11951;
get_id(p_rebate_info)->11954;
get_id(p_rebate_list)->11953;
get_id(sc_rebate_info)->11952;
get_id(cs_activity_rebate_get_reward)->11955;
get_id(p_rebate_reward)->11957;
get_id(sc_rebate_get_reward)->11956;
get_id(sc_rebate_update)->11958;
get_id(cs_activity_levelRank_open)->11959;
get_id(levelRank_rankerInfo)->11961;
get_id(sc_activity_levelRank_open)->11960;
get_id(cs_activity_levelRank_refresh)->11962;
get_id(sc_activity_levelRank_refresh)->11963;
get_id(cs_activity_day_pay_mul)->11964;
get_id(sc_activity_day_pay_mul)->11965;
get_id(cs_invite_info)->11801;
get_id(sc_invite_info)->11802;
get_id(cs_invite_bind_weibo)->11810;
get_id(sc_invite_bind_weibo)->11811;
get_id(cs_invite_weibo_share_levelup)->11812;
get_id(sc_invite_weibo_share_levelup)->11813;
get_id(cs_invite_input_invite_code)->11820;
get_id(sc_invite_input_invite_code)->11821;
get_id(cs_invite_list)->11830;
get_id(p_invite)->11832;
get_id(sc_invite_list)->11831;
get_id(cs_friend_get_list)->11701;
get_id(p_friend)->11703;
get_id(sc_friend_get_list)->11702;
get_id(cs_friend_fight)->11704;
get_id(sc_friend_fight)->11705;
get_id(cs_friend_get_add_list)->11706;
get_id(p_stranger)->11722;
get_id(sc_friend_get_add_list)->11707;
get_id(cs_friend_add)->11708;
get_id(sc_friend_add)->11709;
get_id(p_friend_add)->11710;
get_id(cs_friend_add_list)->11711;
get_id(sc_friend_add_list)->11712;
get_id(sc_friend_new_add)->11713;
get_id(cs_friend_agree)->11714;
get_id(sc_friend_agree)->11715;
get_id(cs_friend_refuse)->11716;
get_id(sc_friend_refuse)->11717;
get_id(cs_friend_explore)->11720;
get_id(sc_friend_explore)->11721;
get_id(cs_friend_delete)->11730;
get_id(sc_friend_delete)->11731;
get_id(sc_friend_notify_delete)->11732;
get_id(sc_friend_new)->11740;
get_id(cs_friend_send_enargy)->11741;
get_id(sc_friend_send_enargy)->11742;
get_id(sc_friend_send_enargy_me)->11743;
get_id(cs_friend_give_enargy)->11744;
get_id(sc_friend_give_enargy)->11745;
get_id(sc_frend_give_enargy_me)->11746;
get_id(cs_friend_give_all_enargy)->11747;
get_id(sc_friend_remove_request)->11748;
get_id(cs_gather_get_list)->11601;
get_id(sc_gather_get_list)->11602;
get_id(sc_gather_new)->11603;
get_id(cs_hist_get_list)->11501;
get_id(sc_hist_get_list)->11502;
get_id(sc_hist_new)->11504;
get_id(cs_hist_replay)->11520;
get_id(sc_hist_replay)->11521;
get_id(sc_hist_unreadNum)->11531;
get_id(cs_mail_info)->11401;
get_id(p_mail)->11405;
get_id(sc_mail_info)->11402;
get_id(cs_mail_draw_reward)->11406;
get_id(sc_mail_draw_reward)->11407;
get_id(cs_mail_delete)->11408;
get_id(sc_mail_delete)->11409;
get_id(cs_mail_new)->11410;
get_id(sc_mail_new)->11411;
get_id(cs_mail_unread_num)->11420;
get_id(sc_mail_unread_num)->11421;
get_id(cs_mail_more)->11430;
get_id(sc_mail_more)->11431;
get_id(cs_mail_agree_friend)->11440;
get_id(sc_mail_agree_friend)->11441;
get_id(cs_mail_del_spec_mail)->11442;
get_id(sc_mail_del_spec_mail)->11443;
get_id(cs_hron_info)->11301;
get_id(sc_hron_info)->11302;
get_id(cs_hron_buy)->11303;
get_id(sc_hron_buy)->11304;
get_id(cs_hron_fight)->11305;
get_id(sc_hron_fight)->11306;
get_id(cs_hron_raids)->11307;
get_id(sc_hron_raids)->11308;
get_id(cs_card_get_list)->11101;
get_id(p_card)->11104;
get_id(p_opened_card)->11103;
get_id(sc_card_get_list)->11102;
get_id(cs_card_draw)->11105;
get_id(sc_card_draw)->11106;
get_id(cs_card_refresh)->11107;
get_id(sc_card_refresh)->11108;
get_id(cs_card_onekey)->11120;
get_id(sc_card_onekey)->11121;
get_id(cs_daily_get_list)->11001;
get_id(p_daily)->11003;
get_id(sc_daily_get_list)->11002;
get_id(cs_daily_draw)->11005;
get_id(sc_daily_draw)->11006;
get_id(cs_daily_reward_list)->11007;
get_id(p_daily_reward)->11010;
get_id(p_daily_reward_info)->11009;
get_id(sc_daily_reward_list)->11008;
get_id(cs_daily_reward_get)->11011;
get_id(sc_daily_reward_get)->11012;
get_id(cs_daily_vip_info)->11013;
get_id(sc_daily_vip_info)->11014;
get_id(cs_daily_vip_draw)->11015;
get_id(sc_daily_vip_draw)->11016;
get_id(cs_pvp_get_list)->10801;
get_id(p_pvp)->10803;
get_id(sc_pvp_get_list)->10802;
get_id(cs_pvp_fight)->10804;
get_id(sc_pvp_fight)->10805;
get_id(cs_pvp_get_first_eight_replays)->10806;
get_id(p_pvp_replay_info)->10808;
get_id(sc_pvp_get_first_eight_replays)->10807;
get_id(cs_pvp_eight_replay)->10809;
get_id(sc_pvp_eight_replay)->10810;
get_id(cs_shop_buy_num)->10701;
get_id(p_shop_num)->10703;
get_id(sc_shop_buy_num)->10702;
get_id(cs_shop_buy)->10704;
get_id(sc_shop_buy)->10705;
get_id(cs_shop_encounter)->10710;
get_id(p_shop_random)->10712;
get_id(sc_shop_encounter)->10711;
get_id(sc_shop_new)->10713;
get_id(cs_shop_refresh)->10720;
get_id(sc_shop_refresh)->10721;
get_id(sc_shop_auto_refresh)->10730;
get_id(cs_shop_treasure_info)->10731;
get_id(p_treasure)->10733;
get_id(sc_shop_treasure_info)->10732;
get_id(cs_shop_treasure_buy)->10734;
get_id(sc_shop_treasure_buy)->10735;
get_id(sc_shop_treasure_new_activity)->10736;
get_id(sc_shop_treasure_new_shop)->10737;
get_id(cs_shop_refresh2)->10738;
get_id(sc_shop_refresh2)->10739;
get_id(cs_item_bag)->10601;
get_id(p_item)->10603;
get_id(sc_item_bag)->10602;
get_id(cs_item_equip)->10604;
get_id(sc_item_equip)->10605;
get_id(cs_item_sell)->10607;
get_id(sc_item_sell)->10608;
get_id(cs_item_down_equip)->10609;
get_id(sc_item_down_equip)->10610;
get_id(cs_item_up_equip)->10611;
get_id(sc_item_up_equip)->10612;
get_id(sc_item_new)->10613;
get_id(p_item_num_update)->10615;
get_id(sc_item_update)->10614;
get_id(cs_item_use)->10617;
get_id(sc_item_use)->10618;
get_id(sc_item_delete_notify)->10619;
get_id(cs_item_reinforce)->10620;
get_id(sc_item_reinforce)->10621;
get_id(cs_item_max_reinforce)->10622;
get_id(sc_item_max_reinforce)->10623;
get_id(sc_item_update_rank)->10624;
get_id(cs_item_up_rank)->10625;
get_id(sc_item_up_rank)->10626;
get_id(sc_item_more)->10627;
get_id(cs_item_compound)->10631;
get_id(sc_item_compound)->10632;
get_id(cs_item_eat)->10633;
get_id(sc_item_eat)->10634;
get_id(p_all_equipment)->10636;
get_id(sc_item_all_equipment)->10635;
get_id(cs_item_use_info)->10637;
get_id(p_item_use_info)->10639;
get_id(sc_item_use_info)->10638;
get_id(cs_item_up_all_equip)->10640;
get_id(sc_item_up_all_equip)->10641;
get_id(cs_explore_one)->10501;
get_id(sc_explore_one)->10502;
get_id(p_echapter)->10503;
get_id(cs_explore_dungeon_list)->10504;
get_id(p_edungeon)->10506;
get_id(sc_explore_dungeon_list)->10505;
get_id(cs_explore_challenge_encounter)->10507;
get_id(sc_explore_challenge_encounter)->10508;
get_id(sc_explore_delete_encounter)->10509;
get_id(cs_explore_giveup_encounter)->10510;
get_id(sc_explore_giveup_encounter)->10511;
get_id(cs_explore_list)->10512;
get_id(sc_explore_list)->10513;
get_id(cs_explore_collect)->10514;
get_id(sc_explore_collect)->10515;
get_id(cs_explore_force_collect)->10516;
get_id(sc_explore_force_collect)->10517;
get_id(cs_explore_auto_explore_check)->10518;
get_id(sc_explore_auto_explore_check)->10519;
get_id(cs_explore_encounter_pass_reward)->10520;
get_id(sc_explore_encounter_pass_reward)->10521;
get_id(cs_explore_encounter_dungeon_state)->10522;
get_id(sc_explore_encounter_dungeon_state)->10523;
get_id(cs_explore_free)->10530;
get_id(sc_explore_free)->10531;
get_id(cs_ger_info)->10401;
get_id(sc_ger_info)->10402;
get_id(p_ger_pos_info)->10404;
get_id(sc_ger_update)->10405;
get_id(sc_ger_new)->10406;
get_id(cs_ger_standup)->10407;
get_id(sc_ger_standup)->10408;
get_id(cs_ger_move_pos)->10409;
get_id(sc_ger_move_pos)->10410;
get_id(cs_ger_pos_list)->10411;
get_id(sc_ger_pos_list)->10412;
get_id(cs_ger_sell)->10414;
get_id(sc_ger_sell)->10415;
get_id(cs_ger_detail)->10416;
get_id(sc_ger_detail)->10417;
get_id(cs_ger_view_other)->10419;
get_id(sc_ger_view_other)->10420;
get_id(sc_ger_update_exp)->10421;
get_id(cs_ger_eat)->10422;
get_id(sc_ger_eat)->10423;
get_id(cs_ger_up_rank)->10424;
get_id(sc_ger_up_rank)->10425;
get_id(sc_ger_update_standlist)->10426;
get_id(cs_ger_down)->10427;
get_id(sc_ger_down)->10428;
get_id(sc_ger_more)->10429;
get_id(sc_ger_del)->10430;
get_id(p_ger_power)->10441;
get_id(sc_ger_refresh_power)->10440;
get_id(cs_ger_view_other_dtl)->10442;
get_id(sc_ger_view_other_dtl)->10443;
get_id(cs_ger_guard_info)->10444;
get_id(p_ger_guard_attr)->10455;
get_id(p_ger_guard)->10454;
get_id(sc_ger_guard_info)->10445;
get_id(cs_ger_guard_set)->10446;
get_id(sc_ger_guard_set)->10447;
get_id(cs_ger_guard_refresh)->10448;
get_id(sc_ger_guard_refresh)->10449;
get_id(cs_ger_lieu_pos_list)->10450;
get_id(sc_ger_lieu_pos_list)->10451;
get_id(cs_ger_lieu_standup)->10452;
get_id(sc_ger_lieu_standup)->10453;
get_id(cs_ger_lieu_untie)->10456;
get_id(p_ger_lieu_info)->10458;
get_id(sc_ger_lieu_untie)->10457;
get_id(cs_ger_lieu_info_list)->10459;
get_id(sc_ger_lieu_info_list)->10460;
get_id(cs_ger_lieu_move_pos)->10461;
get_id(sc_ger_lieu_move_pos)->10462;
get_id(cs_ger_lieu_lock_clo)->10463;
get_id(sc_ger_lieu_lock_clo)->10464;
get_id(cs_ger_lieu_unlock_clo)->10465;
get_id(sc_ger_lieu_unlock_clo)->10466;
get_id(cs_ger_lieu_refresh_clo)->10467;
get_id(sc_ger_lieu_refresh_clo)->10468;
get_id(cs_ger_lieu_tie_info)->10469;
get_id(sc_ger_lieu_tie_info)->10470;
get_id(cs_ger_lieu_refresh_freeTimes)->10472;
get_id(sc_ger_lieu_refresh_freeTimes)->10473;
get_id(sc_ger_new_list)->10474;
get_id(cs_ger_down_rank)->10475;
get_id(sc_ger_down_rank)->10476;
get_id(cs_message_notice)->10302;
get_id(p_notice)->10304;
get_id(sc_message_notice)->10303;
get_id(cs_message_certain_notice)->10305;
get_id(sc_message_certain_notice)->10306;
get_id(sc_message_bc)->10301;
get_id(sc_message_bc_id)->10307;
get_id(sc_message_bc_id2)->10308;
get_id(cs_message_test)->10330;
get_id(sc_message_test)->10331;
get_id(sc_message_best_card)->30001;
get_id(sc_message_ger_upLevel)->30002;
get_id(sc_message_item_uprank)->30003;
get_id(cs_battle_progress)->10201;
get_id(p_battle_progress)->10216;
get_id(sc_battle_progress)->10202;
get_id(cs_battle_info)->10203;
get_id(p_dungeon)->10205;
get_id(sc_battle_info)->10204;
get_id(cs_battle_challenge)->10206;
get_id(sc_battle_challenge)->10207;
get_id(cs_battle_perfect_reward)->10208;
get_id(sc_battle_perfect_reward)->10209;
get_id(sc_battle_broadcast_get_item)->10212;
get_id(cs_battle_dungeon_raids)->10214;
get_id(sc_battle_dungeon_raids)->10215;
get_id(cs_battle_coin_info)->10217;
get_id(sc_battle_coin_info)->10218;
get_id(cs_battle_coin_fight)->10219;
get_id(sc_battle_coin_fight)->10220;
get_id(cs_role_info)->10101;
get_id(sc_role_info)->10102;
get_id(sc_role_update_level)->10107;
get_id(sc_role_update_list)->10103;
get_id(cs_role_buy_energy)->10105;
get_id(sc_role_buy_energy)->10106;
get_id(sc_role_update_exp)->10108;
get_id(sc_role_update_coin)->10109;
get_id(sc_role_update_reputation)->10110;
get_id(sc_role_update_gold)->10111;
get_id(sc_role_update_goldBonus)->10112;
get_id(sc_role_update_vipLevel)->10113;
get_id(sc_role_update_energy)->10114;
get_id(sc_role_update_discoveryTimes)->10115;
get_id(sc_role_update_pvpTimes)->10116;
get_id(sc_role_update_ruleTimes)->10117;
get_id(sc_role_update_randomPVPTimes)->10118;
get_id(sc_role_update_singlePVPTimes)->10119;
get_id(sc_role_update_goldUsed)->10120;
get_id(sc_role_update_title)->10121;
get_id(sc_role_update_encounterFreeNum)->10122;
get_id(sc_role_update_weiboCount)->10123;
get_id(cs_role_setting)->10124;
get_id(sc_role_setting)->10125;
get_id(cs_role_get_energy)->10126;
get_id(sc_role_get_energy)->10127;
get_id(cs_role_buy_coin_value)->10128;
get_id(sc_role_buy_coin_value)->10129;
get_id(cs_role_weixin_share)->10130;
get_id(sc_role_update_pay_ext)->10131;
get_id(cs_role_suggest_open)->10132;
get_id(sc_role_suggest_open)->10133;
get_id(cs_role_suggest)->10134;
get_id(sc_role_suggest)->10135;
get_id(cs_role_log_guide_state)->10136;
get_id(cs_role_pay_tencent)->10137;
get_id(sc_role_pay_tencent)->10138;
get_id(cs_role_login_reward)->10139;
get_id(sc_role_login_reward)->10140;
get_id(cs_role_change_name)->10141;
get_id(sc_role_change_name)->10142;
get_id(cs_role_check_tencent)->10143;
get_id(sc_role_check_tencent)->10144;
get_id(cs_role_push_setting)->10150;
get_id(sc_role_push_setting)->10151;
get_id(cs_role_get_guide_state)->10153;
get_id(sc_role_get_guide_state)->10154;
get_id(cs_role_set_guide_state)->10155;
get_id(sc_role_set_guide_state)->10156;
get_id(cs_role_change_head)->10157;
get_id(sc_role_change_head)->10158;
get_id(cs_role_change_location)->10159;
get_id(cs_role_token)->10180;
get_id(sc_role_token)->10181;
get_id(cs_role_select_ger)->10182;
get_id(sc_role_select_ger)->10183;
get_id(cs_role_demo_fight)->10184;
get_id(sc_role_demo_fight)->10185;
get_id(sc_role_base_config)->10186;
get_id(cs_role_pay_ios)->10190;
get_id(sc_role_pay_ios)->10191;
get_id(cs_role_pay_91)->10192;
get_id(sc_role_pay_91)->10193;
get_id(sc_role_pay_uc)->10194;
get_id(sc_role_pay_dl)->10195;
get_id(sc_role_pay_zz)->10196;
get_id(sc_role_pay_360)->10197;
get_id(sc_role_pay_wdj)->10198;
get_id(sc_role_pay_dk)->10199;
get_id(sc_role_pay_mi)->10189;
get_id(sc_role_pay_az)->10188;
get_id(cs_account_login)->10001;
get_id(sc_account_login)->10002;
get_id(cs_account_create)->10003;
get_id(sc_account_create)->10004;
get_id(cs_account_enter_game)->10005;
get_id(sc_account_enter_game)->10006;
get_id(sc_account_kick)->10007;
get_id(cs_account_heart)->10013;
get_id(sc_account_heart)->10014;
get_id(cs_account_logout)->10015;
get_id(cs_account_check_rolename)->10016;
get_id(sc_account_check_rolename)->10017;
get_id(cs_account_pay_arg)->10018;
get_id(sc_account_pay_arg)->10019;
get_id(sc_account_update_pay_arg)->10020;
get_id(_)->0.