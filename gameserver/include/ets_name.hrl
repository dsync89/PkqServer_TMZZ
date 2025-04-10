-define(REG_JUDG_TABLE, role_state).
-define(ROLE_PAY_ARG, role_pay_arg).
-define(ETS_ETC, ets_etc).% 通用存储ets表
-define(ETS_ROLE_ACTIVITY, ets_role_activity).
-define(ETS_HRON, ets_hron).% 华容道ets表
-define(ETS_RANDOM_SEED, ets_random_seed).% 用来存公共种子的ets
-define(ETS_ROLE_ONLINE, ets_role_online).%在线玩家列表
-define(ETS_ROLE_DUMP, ets_role_dump).%停服写数据的玩家列表
-define(ETS_CLOSE_FLAG, ets_close_flag).%停服标志
-define(ETS_ROLE_PUBLIC, ets_role_public).%玩家共享的属性
-define(ETS_DUNGEON_MON_CACHE, ets_dungeon_mon_cache). %关卡怪物的缓存
-define(ETS_ID,ets_id).% 生成ID的ets表
-define(ETS_NANM_BUFF_FLAG, ets_nanm_buff_flag).% 战南蛮 擂鼓标识
-define(ETS_FAMILY_REQUEST, ets_family_request).    %联盟请求加入的信息
-define(ETS_FAMILY_SUMMARY, ets_family_summary).    %联盟简略信息列表
-define(ETS_FAMILY_PROTECT, ets_family_protect).    %联盟处理的保护操作表，防止同时操作加入两个联盟的情况

-define(ETS_HOMESTEAD_DATA_BASE_TABLE,ets_homestead_data_base_table).										%%家园基础数据缓存表
-define(ETS_HOMESTEAD_DATA_MACHINE_TABLE,ets_homestead_data_machine_table).						%%家园机器数据缓存表
-define(ETS_HOMESTEAD_DATA_LOG_TABLE,ets_homestead_data_log_table).											%%家园日志数据缓存表

-define(ETS_ENARGY_FRIEND_DATA,ets_enargy_friend_data).					%%好友赠送体力