-include("db_name.hrl").
-include("ets_name.hrl").
-include("def_money_log.hrl").
-define(TCP_OPTIONS, [
					  binary
					 ,{packet, 0}
					 ,{active, false}
					 ,{reuseaddr, true}
					 ,{nodelay, false}
					 ,{delay_send, true}
					 ,{send_timeout, 15000}
					 ,{keepalive, true}
					 ,{exit_on_close, true}]).

-define(TICKET, "this_is_moon_game_server").

-define(undefined, undefined).
-define(is_doing_merge, is_doing_merge).
-define(AccidBase, 10000000000).
-define(ROLE_ID_BASE, ((data_setting:get(server_id)+1) * 1000000)).
-define(FAMILY_ID_BASE, ((data_setting:get(server_id)+1) * 1000000)).
-define(GER_ID_BASE,   ((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(ITEM_ID_BASE,  ((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(MAIL_ID_BASE,  ((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(REPLAY_ID_BASE,((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).

-define(REPLAY_TYPE_PVP, 1).
-define(REPLAY_TYPE_RACE, 2).
-define(REPLAY_TYPE_3V3, 3).
-define(REPLAY_TYPE_ALIEN, 4).
-define(REPLAY_TYPE_RULE, 5).

-define(REPLAY_TYPE_LIST_EXCEPT_PVP_RULE, [?REPLAY_TYPE_RACE, ?REPLAY_TYPE_3V3, ?REPLAY_TYPE_ALIEN]).

-define(CHAT_CHANNEL_WORLD, 1).     %世界频道
-define(CHAT_CHANNEL_PERSON, 2).    %私人频道
-define(CHAT_CHANNEL_FAMILY, 3).    %公会频道

-define(REWARD_TYPE_ONLINE, 1).
-define(REWARD_TYPE_LEVEL, 2).
-define(REWARD_TYPE_DAYS, 3).

-define(RANK_TYPE_LEVEL, 1).
-define(RANK_TYPE_FIGHT_POWER, 2).
-define(RANK_TYPE_PVP, 3).
-define(RANK_TYPE_BATTLE, 4).

-define(CATCH(Expression), (
							try Expression
							catch 
								ErrType:Reason ->
									?ERR("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, Reason, erlang:get_stacktrace(), ??Expression]),
									{'EXIT',{ErrType, Reason}}
							end
							)).

-define(LOOSE_CATCH(Expression), (
							try Expression
							catch 
								throw:SomeThing ->
									SomeThing;
								ErrType:Reason ->
									?ERR("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, Reason, erlang:get_stacktrace(), ??Expression]),
									{'EXIT',{ErrType, Reason}}
							end
							)).
-define(CATCH_1(Expression,Reason), (
							try Expression
							catch 
								_:Reason ->
									?ERR("ErrReason:~1000p, Stack=~100000p, Expression=~s",[Reason, erlang:get_stacktrace(), ??Expression]),
									{'EXIT',Reason}
							end
							)).

%% 致命错误
-define(CRITICAL(Format), logger:critical_msg(?MODULE,?LINE, Format, [])).
-define(CRITICAL(Format, Args), logger:critical_msg(?MODULE,?LINE, Format, Args)).

%% 信息
-define(INFO(Format), logger:info_msg(?MODULE,?LINE, Format, [])).
-define(INFO(Format, Args), logger:info_msg(?MODULE,?LINE, Format, Args)).

%% 警告
-define(WARNING(Format), logger:warning_msg(?MODULE,?LINE, Format, [])).
-define(WARNING(Format, Args), logger:warning_msg(?MODULE,?LINE, Format, Args)).

%% 开发信息
-define(DEV(Format), logger:dev_msg(?MODULE,?LINE, Format, [])).
-define(DEV(Format, Args), logger:dev_msg(?MODULE,?LINE, Format, Args)).

%% 调试信息
-define(DEBUG(Format), logger:debug_msg(?MODULE,?LINE, Format, [], [{module, ?MODULE}])).
-define(DEBUG(Format, Args), logger:debug_msg(?MODULE,?LINE, Format, Args, [{module, ?MODULE}])).

%% 错误信息
-define(ERR(Format), logger:error_msg(?MODULE,?LINE, Format, [])).
-define(ERR(Format, Args), logger:error_msg(?MODULE,?LINE, Format, Args)).

%% 不应该出现的日志
-define(UNEXPECTED, ?ERR("unexpected\~\~\~")).

%% 类型定义
-define(int8, type:int8()).
-define(int16, type:int16()).
-define(int32, type:int32()).
-define(int64, type:int64()).
-define(string, type:str()).

-define(sign(Integer), (Integer div abs(Integer))).

-define(male, 1).%男
-define(female,2).%女

-define(unicast(RoleID, Info), role_lib:send_client(RoleID,Info)).
-define(unicast2(RoleID, Info), role_lib:send_client_no_block(RoleID,Info)).

-define(ONE_DAY_SECONDS, 86400).
-define(ONE_HOUR_SECONDS,3600).

-type timerRef() :: {EndSec :: ?int32, reference()}.

-define(DB, tk).

%%任务结构数据
-record(r_task,{
	task_id	,									%%任务id
	status,										%%任务状态  1:未接  2：已接  3：已完成
	trigger_id,								%%触发类型
	trigger_num,							%%触发了的个数（比如当前杀了多少个怪）
	trigger_notes	=[]						%%触发记录(根据触发类型来确定是否用这个字段)
}).

%% 渠道类型ID
-define(ACCOUNT_TYPE_NORMAL_HD, 0).             %ios正版高清
-define(ACCOUNT_TYPE_NORMAL, 1).                %ios正版
-define(ACCOUNT_TYPE_91,2).                     %91
-define(ACCOUNT_TYPE_UC,3).                     %UC
-define(ACCOUNT_TYPE_DL,4).                     %当乐
-define(ACCOUNT_TYPE_ZZ,5).                     %中手游
-define(ACCOUNT_TYPE_360,6).                    %360
-define(ACCOUNT_TYPE_WDJ,7).                    %豌豆荚
-define(ACCOUNT_TYPE_DK,8).                     %多酷
-define(ACCOUNT_TYPE_MI,9).                     %小米
-define(ACCOUNT_TYPE_AZ,10).                    %安智
-define(ACCOUNT_TYPE_PP,11).                    %PP
-define(ACCOUNT_TYPE_KY,12).                    %快用
-define(ACCOUNT_TYPE_ZR,13).                    %卓然
-define(ACCOUNT_TYPE_IT,14).                    %itools
-define(ACCOUNT_TYPE_TBT,15).                   %同步推
-define(ACCOUNT_TYPE_HW, 16).                   %华为

-define(ACCOUNT_TYPE_SINA, 20).                 %新浪
-define(ACCOUNT_TYPE_LENOVO, 21).               %联想
-define(ACCOUNT_TYPE_OPPO, 22).                 %oppo
-define(ACCOUNT_TYPE_SOGOU, 23).                %搜狗
-define(ACCOUNT_TYPE_JIFENG, 24).               %机锋
-define(ACCOUNT_TYPE_YYB, 25).                  %应用宝
-define(ACCOUNT_TYPE_YYH, 26).                  %应用汇
-define(ACCOUNT_TYPE_VIVO, 27).                 %vivo
-define(ACCOUNT_TYPE_QQ, 28).                   %QQ
-define(ACCOUNT_TYPE_WEIXIN, 29).               %微信
-define(ACCOUNT_TYPE_OUWAN, 30).                %偶玩

-define(ACCOUNT_TYPE_JL,34).                    %金立
-define(ACCOUNT_TYPE_MZ, 35).                   %魅族
-define(ACCOUNT_TYPE_KW, 36).                   %酷我
-define(ACCOUNT_TYPE_KS, 37).                   %金山
-define(ACCOUNT_TYPE_MZW,38).                   %拇指玩
-define(ACCOUNT_TYPE_I4, 39).                   %爱思
-define(ACCOUNT_TYPE_YK, 40).                   %优酷
-define(ACCOUNT_TYPE_PPS, 41).                  %pps
-define(ACCOUNT_TYPE_TXHD_ARD, 42).             %天象安卓
-define(ACCOUNT_TYPE_TXHD_IOS, 43).             %天象ios

-define(ACCOUNT_TYPE_TXHD_ARD_EXT, 44).         %天象安卓ext
-define(ACCOUNT_TYPE_37WAN, 45).                %37玩
-define(ACCOUNT_TYPE_4399, 46).                 %4399
-define(ACCOUNT_TYPE_HM, 47).                   %海马
-define(ACCOUNT_TYPE_CW, 48).                   %益玩
-define(ACCOUNT_TYPE_51CM, 49).                 %51畅梦

-define(ACCOUNT_TYPE_UU, 50).                   %悠悠村
-define(ACCOUNT_TYPE_QTLD_ARD, 51).             %奇天乐地ARD
-define(ACCOUNT_TYPE_QTLD_IOS, 52).             %奇天乐地IOS
-define(ACCOUNT_TYPE_LD, 53).                   %乐逗
-define(ACCOUNT_TYPE_MMY, 54).                  %木蚂蚁
-define(ACCOUNT_TYPE_GG, 55).                   %谷果
-define(ACCOUNT_TYPE_XY, 56).                   %xy
-define(ACCOUNT_TYPE_BAIDU, 57).                %百度
-define(ACCOUNT_TYPE_YAOWAN, 58).               %要玩
-define(ACCOUNT_TYPE_IIAPPLE, 59).				%爱苹果
-define(ACCOUNT_TYPE_SHAZAM,  60).              % 海外 
-define(ACCOUNT_TYPE_HJR,     61).              %好接入
-define(ACCOUNT_TYPE_YY,      62).              % YY
-define(ACCOUNT_TYPE_DIANJOY, 63).              % 点乐
-define(ACCOUNT_TYPE_COOLPAD, 64).              % 酷派
-define(ACCOUNT_TYPE_YIJIE, 65).              % 易接
-define(ACCOUNT_TYPE_QUICK, 66).              % quick


