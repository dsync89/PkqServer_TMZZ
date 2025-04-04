-module(proto_route).
-compile(export_all).

route(cs_melee_info) ->
	{role,role_melee};
route(cs_melee_sign) ->
	{role,role_melee};
route(cs_melee_fight) ->
	{role,role_melee};
route(cs_homestead_get_info) ->
	{role,role_homestead};
route(cs_homestead_get_friend_info) ->
	{friend_server,role_homestead};
route(cs_homestead_unlock_machine) ->
	{role,role_homestead};
route(cs_homestead_uproot_seed) ->
	{role,role_homestead};
route(cs_homestead_harvest) ->
	{role,role_homestead};
route(cs_homestead_seeding) ->
	{role,role_homestead};
route(cs_homestead_change_ger) ->
	{role,role_homestead};
route(cs_homestead_mating) ->
	{friend_server,role_homestead};
route(cs_homestead_addenergy) ->
	{friend_server,role_homestead};
route(cs_homestead_get_log) ->
	{role,role_homestead};
route(cs_homestead_get_friend_log) ->
	{friend_server,role_homestead};
route(cs_task_get_info) ->
	{role,role_task};
route(cs_task_operate) ->
	{role,role_task};
route(cs_fight_request) ->
	{role,role_fight};
route(cs_road_info) ->
	{role,role_road};
route(cs_road_reset) ->
	{role,role_road};
route(cs_road_fight) ->
	{role,role_road};
route(cs_road_fight_ext) ->
	{role,role_road};
route(cs_road_box) ->
	{role,role_road};
route(cs_hula_open) ->
	{hula_server,role_hula};
route(cs_hula_close) ->
	{hula_server,role_hula};
route(cs_hula_cur_info) ->
	{hula_server,role_hula};
route(cs_hula_rank_sync) ->
	{hula_server,role_hula};
route(cs_hula_fight) ->
	{role,role_hula};
route(cs_hula_reborn) ->
	{role,role_hula};
route(cs_hula_open_time) ->
	{hula_server,role_hula};
route(cs_festival_info) ->
	{festival_server,role_festival};
route(cs_festival_click) ->
	{role,role_festival};
route(cs_festival_rank) ->
	{festival_server,role_festival};
route(cs_rule_info) ->
	{role,role_rule};
route(cs_rule_rank) ->
	{rule_server,role_rule};
route(cs_rule_last_rank) ->
	{rule_server,role_rule};
route(cs_rule_fight) ->
	{rule_server,role_rule};
route(cs_rule_leave) ->
	{rule_server,role_rule};
route(cs_rank_info) ->
	{role,role_rank};
route(cs_alien_info) ->
	{role,role_alien};
route(cs_alien_first_five) ->
	{alien_server,role_alien};
route(cs_alien_kill_num_rank) ->
	{alien_server,role_alien};
route(cs_alien_kill_continuous_rank) ->
	{alien_server,role_alien};
route(cs_alien_guess_info) ->
	{alien_server,role_alien};
route(cs_alien_guess) ->
	{role,role_alien};
route(cs_alien_reset) ->
	{role,role_alien};
route(cs_alien_fight) ->
	{role,role_alien};
route(cs_alien_sign) ->
	{role,role_alien};
route(cs_alien_self_record) ->
	{alien_server,role_alien};
route(cs_alien_record) ->
	{alien_server,role_alien};
route(cs_alien_self_fight_replay) ->
	{alien_server,role_alien};
route(cs_alien_fight_replay) ->
	{alien_server,role_alien};
route(cs_alien_leave) ->
	{alien_server,role_alien};
route(cs_alien_view_other) ->
	{alien_server,role_alien};
route(cs_alien_view_other_dtl) ->
	{alien_server,role_alien};
route(cs_alien_buy_times) ->
	{role,role_alien};
route(cs_alien_active) ->
	{alien_server,role_alien};
route(cs_team_pk_info) ->
	{role,role_team};
route(cs_team_refresh) ->
	{role,role_team};
route(cs_team_fight) ->
	{role,role_team};
route(cs_team_rank) ->
	{role,role_team};
route(cs_team_record) ->
	{team_pk_server,role_team};
route(cs_team_self_record) ->
	{role,role_team};
route(cs_team_move) ->
	{role,role_team};
route(cs_team_fight_replay) ->
	{team_pk_server,role_team};
route(cs_team_self_fight_replay) ->
	{role,role_team};
route(cs_team_view_other) ->
	{role,role_team};
route(cs_team_view_other_dtl) ->
	{role,role_team};
route(cs_team_new_status) ->
	{team_pk_server,role_team};
route(cs_race_history) ->
	{race_server,role_race};
route(cs_race_replay) ->
	{race_server,role_race};
route(cs_race_fight_list) ->
	{race_server,role_race};
route(cs_race_sign) ->
	{role,role_race};
route(cs_race_info) ->
	{race_server,role_race};
route(cs_race_enter) ->
	{race_server,role_race};
route(cs_race_leave) ->
	{race_server,role_race};
route(cs_race_pos_history) ->
	{race_server,role_race};
route(cs_race_is_open) ->
	{race_server,role_race};
route(cs_race_auto_sign) ->
	{role,role_race};
route(cs_race_auto_unsign) ->
	{race_server,role_race};
route(cs_race_self_history) ->
	{race_server,role_race};
route(cs_race_guess_info) ->
	{race_server,role_race};
route(cs_race_guess) ->
	{role,role_race};
route(cs_family_get_list) ->
	{family_manager_server,role_family};
route(cs_family_create) ->
	{role,role_family};
route(cs_family_request_join) ->
	{role,role_family};
route(cs_family_cancel_join) ->
	{role,role_family};
route(cs_family_agree_join) ->
	{role,role_family};
route(cs_family_refuse_join) ->
	{role,role_family};
route(cs_family_get_info) ->
	{role,role_family};
route(cs_family_kick) ->
	{role,role_family};
route(cs_family_create_consume) ->
	{role,role_family};
route(cs_family_leave) ->
	{role,role_family};
route(cs_family_change_notice) ->
	{role,role_family};
route(cs_family_request_list) ->
	{role,role_family};
route(cs_combine_do) ->
	{role,role_combine};
route(cs_combine_info) ->
	{role,role_combine};
route(cs_firecracker_open) ->
	{role,role_firecracker};
route(cs_firecracker_close) ->
	{fire_server,role_firecracker};
route(cs_firecracker_setoff) ->
	{role,role_firecracker};
route(cs_firecracker_rank) ->
	{fire_server,role_firecracker};
route(cs_firecracker_get_reward) ->
	{role,role_firecracker};
route(cs_treaHouse_get_list) ->
	{role,role_treaHouse};
route(cs_treaHouse_is_open) ->
	{role,role_treaHouse};
route(cs_treaHouse_explore_one) ->
	{role,role_treaHouse};
route(cs_treaHouse_explore_ten) ->
	{role,role_treaHouse};
route(cs_treaHouse_refresh) ->
	{role,role_treaHouse};
route(cs_treaHouse_open_base_box) ->
	{role,role_treaHouse};
route(cs_treaHouse_get_rankInfo) ->
	{role,role_treaHouse};
route(cs_treaHouse_get_rank_Reward) ->
	{activityRank_server,role_treaHouse};
route(cs_treaHouse_get_baseBoxRewardInfo) ->
	{role,role_treaHouse};
route(cs_challengeGod_info) ->
	{role,role_challengeGod};
route(cs_challengeGod_select_ger) ->
	{role,role_challengeGod};
route(cs_challengeGod_challenge_dungeon_one) ->
	{role,role_challengeGod};
route(cs_challengeGod_challenge_dungeon_ten) ->
	{role,role_challengeGod};
route(cs_talk_world) ->
	{role,role_talk};
route(cs_talk_gag_one) ->
	{role,role_talk};
route(cs_talk_ungag_one) ->
	{role,role_talk};
route(cs_talk_get_gag_list) ->
	{role,role_talk};
route(cs_talk_recent_list) ->
	{talk_server,role_talk};
route(cs_talk_person) ->
	{role,role_talk};
route(cs_talk_person_offline) ->
	{role,role_talk};
route(cs_talk_gm) ->
	{talk_server,role_talk};
route(cs_nanm_open) ->
	{nanm_server,role_nanm};
route(cs_nanm_close) ->
	{nanm_server,role_nanm};
route(cs_nanm_buff) ->
	{role,role_nanm};
route(cs_nanm_last_info) ->
	{nanm_server,role_nanm};
route(cs_nanm_cur_info) ->
	{nanm_server,role_nanm};
route(cs_nanm_rank_sync) ->
	{nanm_server,role_nanm};
route(cs_nanm_fight) ->
	{role,role_nanm};
route(cs_nanm_reborn) ->
	{role,role_nanm};
route(cs_nanm_offline_play) ->
	{role,role_nanm};
route(cs_nanm_open_time) ->
	{nanm_server,role_nanm};
route(cs_version) ->
	{role,role_version};
route(cs_gift_request) ->
	{role,role_gift};
route(cs_box_item) ->
	{role,role_box};
route(cs_box_shop) ->
	{role,role_box};
route(cs_box_shop_info) ->
	{role,role_box};
route(cs_box_get_spirit_equip_count) ->
	{role,role_box};
route(cs_activity_get_list) ->
	{role,role_activity};
route(cs_activity_info) ->
	{role,role_activity};
route(cs_activity_month) ->
	{role,role_activity};
route(cs_activity_month_buy) ->
	{role,role_activity};
route(cs_activity_month_draw) ->
	{role,role_activity};
route(cs_activity_draw) ->
	{role,role_activity};
route(cs_activity_energy) ->
	{activity_server,role_activity};
route(cs_activity_sign_emperor_info) ->
	{role,role_activity};
route(cs_activity_sign_get_reward) ->
	{role,role_activity};
route(cs_activity_sign_up) ->
	{role,role_activity};
route(cs_activity_rebate_info) ->
	{role,role_activity};
route(cs_activity_rebate_get_reward) ->
	{role,role_activity};
route(cs_activity_levelRank_open) ->
	{role,role_activity};
route(cs_activity_levelRank_refresh) ->
	{role,role_activity};
route(cs_activity_day_pay_mul) ->
	{role,role_activity};
route(cs_invite_info) ->
	{role,role_invite};
route(cs_invite_bind_weibo) ->
	{role,role_invite};
route(cs_invite_weibo_share_levelup) ->
	{role,role_invite};
route(cs_invite_input_invite_code) ->
	{role,role_invite};
route(cs_invite_list) ->
	{role,role_invite};
route(cs_friend_get_list) ->
	{friend_server,role_friend};
route(cs_friend_fight) ->
	{friend_server,role_friend};
route(cs_friend_get_add_list) ->
	{friend_server,role_friend};
route(cs_friend_add) ->
	{friend_server,role_friend};
route(cs_friend_add_list) ->
	{friend_server,role_friend};
route(cs_friend_agree) ->
	{friend_server,role_friend};
route(cs_friend_refuse) ->
	{friend_server,role_friend};
route(cs_friend_explore) ->
	{friend_server,role_friend};
route(cs_friend_delete) ->
	{friend_server,role_friend};
route(cs_friend_send_enargy) ->
	{friend_server,role_friend};
route(cs_friend_give_enargy) ->
	{role,role_friend};
route(cs_friend_give_all_enargy) ->
	{role,role_friend};
route(cs_gather_get_list) ->
	{role,role_gather};
route(cs_hist_get_list) ->
	{hist_server,role_hist};
route(cs_hist_replay) ->
	{hist_server,role_hist};
route(cs_mail_info) ->
	{mail_server,role_mail};
route(cs_mail_draw_reward) ->
	{mail_server,role_mail};
route(cs_mail_delete) ->
	{mail_server,role_mail};
route(cs_mail_new) ->
	{mail_server,role_mail};
route(cs_mail_unread_num) ->
	{mail_server,role_mail};
route(cs_mail_more) ->
	{mail_server,role_mail};
route(cs_mail_agree_friend) ->
	{mail_server,role_mail};
route(cs_mail_del_spec_mail) ->
	{mail_server,role_mail};
route(cs_hron_info) ->
	{role,role_hron};
route(cs_hron_buy) ->
	{role,role_hron};
route(cs_hron_fight) ->
	{role,role_hron};
route(cs_hron_raids) ->
	{role,role_hron};
route(cs_card_get_list) ->
	{role,role_card};
route(cs_card_draw) ->
	{role,role_card};
route(cs_card_refresh) ->
	{role,role_card};
route(cs_card_onekey) ->
	{role,role_card};
route(cs_daily_get_list) ->
	{role,role_daily};
route(cs_daily_draw) ->
	{role,role_daily};
route(cs_daily_reward_list) ->
	{role,role_daily};
route(cs_daily_reward_get) ->
	{role,role_daily};
route(cs_daily_vip_info) ->
	{role,role_daily};
route(cs_daily_vip_draw) ->
	{role,role_daily};
route(cs_pvp_get_list) ->
	{role,role_pvp};
route(cs_pvp_fight) ->
	{role,role_pvp};
route(cs_pvp_get_first_eight_replays) ->
	{pvp_server,role_pvp};
route(cs_pvp_eight_replay) ->
	{pvp_server,role_pvp};
route(cs_shop_buy_num) ->
	{role,role_shop};
route(cs_shop_buy) ->
	{role,role_shop};
route(cs_shop_encounter) ->
	{role,role_shop};
route(cs_shop_refresh) ->
	{role,role_shop};
route(cs_shop_treasure_info) ->
	{role,role_shop};
route(cs_shop_treasure_buy) ->
	{role,role_shop};
route(cs_shop_refresh2) ->
	{role,role_shop};
route(cs_item_bag) ->
	{role,role_item};
route(cs_item_equip) ->
	{role,role_item};
route(cs_item_sell) ->
	{role,role_item};
route(cs_item_down_equip) ->
	{role,role_item};
route(cs_item_up_equip) ->
	{role,role_item};
route(cs_item_use) ->
	{role,role_item};
route(cs_item_reinforce) ->
	{role,role_item};
route(cs_item_max_reinforce) ->
	{role,role_item};
route(cs_item_up_rank) ->
	{role,role_item};
route(cs_item_compound) ->
	{role,role_item};
route(cs_item_eat) ->
	{role,role_item};
route(cs_item_use_info) ->
	{role,role_item};
route(cs_item_up_all_equip) ->
	{role,role_item};
route(cs_explore_one) ->
	{role,role_explore};
route(cs_explore_dungeon_list) ->
	{role,role_explore};
route(cs_explore_challenge_encounter) ->
	{role,role_explore};
route(cs_explore_giveup_encounter) ->
	{role,role_explore};
route(cs_explore_list) ->
	{role,role_explore};
route(cs_explore_collect) ->
	{role,role_explore};
route(cs_explore_force_collect) ->
	{role,role_explore};
route(cs_explore_auto_explore_check) ->
	{role,role_explore};
route(cs_explore_encounter_pass_reward) ->
	{role,role_explore};
route(cs_explore_encounter_dungeon_state) ->
	{role,role_explore};
route(cs_explore_free) ->
	{role,role_explore};
route(cs_ger_info) ->
	{role,role_ger};
route(cs_ger_standup) ->
	{role,role_ger};
route(cs_ger_move_pos) ->
	{role,role_ger};
route(cs_ger_pos_list) ->
	{role,role_ger};
route(cs_ger_sell) ->
	{role,role_ger};
route(cs_ger_detail) ->
	{role,role_ger};
route(cs_ger_view_other) ->
	{role,role_ger};
route(cs_ger_eat) ->
	{role,role_ger};
route(cs_ger_up_rank) ->
	{role,role_ger};
route(cs_ger_down) ->
	{role,role_ger};
route(cs_ger_view_other_dtl) ->
	{role,role_ger};
route(cs_ger_guard_info) ->
	{role,role_ger};
route(cs_ger_guard_set) ->
	{role,role_ger};
route(cs_ger_guard_refresh) ->
	{role,role_ger};
route(cs_ger_lieu_pos_list) ->
	{role,role_ger};
route(cs_ger_lieu_standup) ->
	{role,role_ger};
route(cs_ger_lieu_untie) ->
	{role,role_ger};
route(cs_ger_lieu_info_list) ->
	{role,role_ger};
route(cs_ger_lieu_move_pos) ->
	{role,role_ger};
route(cs_ger_lieu_lock_clo) ->
	{role,role_ger};
route(cs_ger_lieu_unlock_clo) ->
	{role,role_ger};
route(cs_ger_lieu_refresh_clo) ->
	{role,role_ger};
route(cs_ger_lieu_tie_info) ->
	{role,role_ger};
route(cs_ger_lieu_refresh_freeTimes) ->
	{role,role_ger};
route(cs_ger_down_rank) ->
	{role,role_ger};
route(cs_message_notice) ->
	{role,role_message};
route(cs_message_certain_notice) ->
	{role,role_message};
route(cs_message_test) ->
	{role,role_message};
route(cs_battle_progress) ->
	{role,role_battle};
route(cs_battle_info) ->
	{role,role_battle};
route(cs_battle_challenge) ->
	{role,role_battle};
route(cs_battle_perfect_reward) ->
	{role,role_battle};
route(cs_battle_dungeon_raids) ->
	{role,role_battle};
route(cs_battle_coin_info) ->
	{role,role_battle};
route(cs_battle_coin_fight) ->
	{role,role_battle};
route(cs_role_info) ->
	{role,role_role};
route(cs_role_buy_energy) ->
	{role,role_role};
route(cs_role_setting) ->
	{role,role_role};
route(cs_role_get_energy) ->
	{role,role_role};
route(cs_role_buy_coin_value) ->
	{role,role_role};
route(cs_role_weixin_share) ->
	{role,role_role};
route(cs_role_suggest_open) ->
	{role,role_role};
route(cs_role_suggest) ->
	{role,role_role};
route(cs_role_log_guide_state) ->
	{role,role_role};
route(cs_role_pay_tencent) ->
	{role,role_role};
route(cs_role_login_reward) ->
	{role,role_role};
route(cs_role_change_name) ->
	{role,role_role};
route(cs_role_check_tencent) ->
	{role,role_role};
route(cs_role_push_setting) ->
	{push_server,role_role};
route(cs_role_get_guide_state) ->
	{role,role_role};
route(cs_role_set_guide_state) ->
	{role,role_role};
route(cs_role_change_head) ->
	{role,role_role};
route(cs_role_change_location) ->
	{role,role_role};
route(cs_role_token) ->
	{role,role_role};
route(cs_role_select_ger) ->
	{role,role_role};
route(cs_role_demo_fight) ->
	{role,role_role};
route(cs_role_pay_ios) ->
	{pay_server,role_role};
route(cs_role_pay_91) ->
	{pay_server,role_role};
route(cs_account_login) ->
	{role,role_account};
route(cs_account_create) ->
	{role,role_account};
route(cs_account_enter_game) ->
	{role,role_account};
route(cs_account_heart) ->
	{role,role_account};
route(cs_account_logout) ->
	{role,role_account};
route(cs_account_check_rolename) ->
	{role,role_account};
route(cs_account_pay_arg) ->
	{role,role_account};
route(_) ->undefined.
