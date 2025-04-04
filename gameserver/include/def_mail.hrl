

%% =================================
%% dictionary key in role server




%% =================================



-define(MAIL_TYPE_SYS,			1).% 系统消息     
-define(MAIL_TYPE_REWARD,		2).% 系统奖励     
-define(MAIL_TYPE_ADD_FRIEND,	3).% 加好友申请      
-define(MAIL_TYPE_JOIN_UNION,	4).% 加联盟申请      
-define(MAIL_TYPE_PRIVATE,		5).% 私人邮件       
-define(MAIL_TYPE_UNION,		6).% 联盟邮件       


-define(MAIL_TEMPLATE_ADD_FRIEND_REQUEST, 1001).%% 好友申请
-define(MAIL_TEMPLATE_DEL_FRIEND, 		1002).%% 删除好友通知
-define(MAIL_BIND_WEIBO_SUCC,			1003).%% 绑定微博成功奖励
-define(MAIL_INVITE_GUY_FIRST_PAY_REWARD,1004).%% 邀请好友充值奖励
-define(MAIL_HULA_KILL, 1005).%% 虎牢关击杀奖励
-define(MAIL_HULA_LUCKY, 1006).%% 虎牢关幸运奖励
-define(MAIL_HULA_RANK_THREE, 1007).%% 虎牢关排名前3奖励
-define(MAIL_HULA_OFFLINE, 1008).%% 虎牢关离线参与奖励
-define(MAIL_HRON_FOUR_STAR, 1009).%% 华容道4星   
-define(MAIL_HRON_THREE_STAR, 1010).%% 华容道3星   
-define(MAIL_HRON_TWO_STAR, 1011).%% 华容道2星   
-define(MAIL_HRON_ONE_STAR, 1012).%% 华容道1星   
-define(MAIL_KING_FIRST,1013).%%皇权冠军
-define(MAIL_KING_SECOND,1014).%%皇权亚军
-define(MAIL_KING_OTHER,1015).%%皇权其他排名奖励
-define(MAIL_OTHER_REWARD, 0).%%其他奖励
-define(MAIL_HULA_RANK_FIRST10, 1017).%% 虎牢关排名前10奖励
-define(MAIL_HULA_RANK_JOIN, 1018).%% 虎牢关参与奖励
-define(MAIL_NANM_KILL, 1020).%% 战南蛮击杀奖励
-define(MAIL_NANM_LUCKY, 1021).%% 战南蛮幸运奖励
-define(MAIL_NANM_RANK_FIRST10, 1022).%% 战南蛮排名前10奖励
-define(MAIL_NANM_RANK_JOIN, 1023).%% 战南蛮参与奖励
-define(MAIL_NANM_OFFLINE, 1025).%% 战南蛮离线参与奖励
-define(MAIL_BET_EMPEROR_WIN, 1028). % 帝王争霸战下注成功奖励
-define(MAIL_BET_EMPEROR_LOSE, 1029). % 帝王争霸战下注失败返还
-define(MAIL_EMPEROR_WIN,1026). %帝王争霸战皇帝获得
-define(MAIL_JOIN_EMPEROR,1027). %帝王争霸战参与获得
-define(MAIL_FIRECRACKER_RETURN_GOLD, 1030). % 燃放爆竹返还元宝
-define(MAIL_FIRECRACKER_RANK_REWARD, 1031). % 燃放爆竹排名奖励
-define(MAIL_TREAHOUSE_RANK_REWARD, 1050). % 汉帝宝库排行榜获得
-define(MAIL_CROSS_SIGN, 1032).
-define(MAIL_CROSS_SUPPORT_FAIL, 1033).
-define(MAIL_CROSS_SUPPORT_SUCC, 1034).
-define(MAIL_CROSS_SKY_FIRST, 1035).
-define(MAIL_CROSS_SKY_SECOND, 1036).
-define(MAIL_CROSS_SKY_EIGHT, 1037).
-define(MAIL_CROSS_SKY_SIGN, 1038).
-define(MAIL_CROSS_GROUND_FIRST, 1039).
-define(MAIL_CROSS_GROUND_SECOND, 1040).
-define(MAIL_CROSS_GROUND_EIGHT, 1041).
-define(MAIL_CROSS_GROUND_SIGN, 1042).
-define(MAIL_CROSS_REWARD_ALL_SKY_FIRST, 1043).
-define(MAIL_CROSS_REWARD_ALL_SKY_SECOND, 1044).
-define(MAIL_CROSS_REWARD_ALL_GROUND_FIRST, 1045).
-define(MAIL_CROSS_REWARD_ALL_GROUND_SECOND, 1046).
-define(MAIL_CROSS_ENTER_SKY_LIST, 1047).
-define(MAIL_CROSS_ENTER_GROUND_LIST, 1048).
-define(MAIL_REBATE_REWARD, 1049). % 压岁钱未领取奖励
-define(MAIL_QUARTER_EMPEROR,1052). % 帝王战4强邮件奖励
-define(MAIL_SECOND_EMPEROR, 1051). % 帝王战第二名邮件奖励
-define(MAIL_LEVEL_RANK_REWARD, 1053). % 冲级排行奖励邮件
-define(MAIL_WEIXIN_SHARE_REWARD, 1054). % 微信分享奖励邮件
-define(MAIL_FAMILY_BE_KICK, 1057). %被踢出联盟
-define(MAIL_FAMILY_BE_AGREE, 1058). %加联盟申请被同意
-define(MAIL_FAMILY_BE_REFUSE, 1059). %加联盟申请被拒绝
-define(MAIL_RACE_GROUP_SIGN_REWARD, 1051). %华丽大赛参与奖
-define(MAIL_RACE_GROUP_FIRST_FOUR_REWARD, 1052). %华丽大赛小组前四奖
-define(MAIL_RACE_FIRST_EIGHT_REWARD, 1053). %华丽大赛八强奖
-define(MAIL_RACE_FIRST_FOUR_REWARD, 1054). %华丽大赛四强奖
-define(MAIL_RACE_SECOND_REWARD, 1055). %华丽大赛亚军奖
-define(MAIL_RACE_FIRST_REWARD, 1056). %华丽大赛冠军奖
-define(MAIL_3V3_REWARD, 1071).        %3v3排行榜奖励 
-define(MAIL_PVP_DAY_REWARD, 1072).     %竞技场每日奖励
-define(MAIL_NONE_TEMPLATE_MAIL, 0). %% 由配置文件配置的邮件
-define(MAIL_MELEE_RANK_WIN_REWARD, 1077).  %% 大乱斗的阵营胜利排名奖励
-define(MAIL_MELEE_RANK_LOSE_REWARD, 1078). %% 大乱斗的阵营失败排名奖励
-define(MAIL_MELEE_LONGEST_WIN_REWARD, 1079). %% 大乱斗的最长连胜奖励   
   
   