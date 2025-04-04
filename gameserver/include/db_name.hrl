-define(dbRole, dbRole).
-define(dbRoleExtra, dbRoleExtra).
-define(dbCounter, dbCounter).
-define(dbChapter, dbChapter).
-define(dbFighterList, dbFighterList).
-define(dbGer,	dbGer).
-define(dbBag,	dbBag).
-define(dbPVP, dbPVP).
-define(dbPlunder, dbPlunder).
-define(dbEtc, dbEtc).
-define(dbPush, dbPush).
-define(dbOfflineDeductGold,dbOfflineDeductGold).
-define(dbMail, dbMail).
-define(dbRoleName, dbRoleName).
-define(dbHist, dbHist).
-define(dbPay, dbPay).
-define(dbPayLog, dbPayLog).
-define(dbBestPassChapter,dbBestPassChapter).
-define(dbFriend, dbFriend).
-define(dbInvite, dbInvite).
-define(dbActivity, dbActivity).

-define(DB_ETC_KEY_HRON, 2).%华容道持久化key
-define(DB_ETC_KEY_HULA, 3).%虎牢关持久化key
-define(DB_ETC_KEY_PVP, 5).%排行榜持久化Key
-define(DB_ETC_KEY_HRON_TERM, 8).%华容道当前排行持久化key
-define(DB_ETC_KEY_NANM, 9).%战南蛮持久化key
-define(DB_ETC_KEY_NANM_OFFLINE_PLAY, 10).%战南蛮离线参与持久化key
-define(DB_ETC_KEY_NANM_BUFF,11).%战南蛮buff持久化key
-define(DB_ETC_KEY_TALK,12). %%世界聊天GM禁言列表
-define(DB_ETC_KEY_EIGHT_REPLAY,14). %%争霸8强战报列表
-define(DB_ETC_KEY_FIRE, 16).       %%爆竹持久化key
-define(DB_ETC_KEY_ACTIVITYRANK,17). %% 排行榜活动持久化key
-define(DB_ETC_KEY_FIRE_ROLEINFO, 18). %%爆竹玩家信息
-define(DB_ETC_KEY_REBATE_ROLEINFO, 19). %%返利玩家信息
-define(DB_ETC_KEY_LEVELRANK, 20). %% 等级排行榜活动信息
-define(DB_ETC_KEY_RACE, 21).       %%华丽大赛数据key
-define(DB_ETC_KEY_PUSH,22).			%%push相关的数据
-define(DB_ETC_KEY_TEAM_PK, 23).        %%3v3 Server
-define(DB_ETC_KEY_ALIEN, 24).          %%异星战场数据
-define(DB_ETC_KEY_PERSON_TALK, 25).    %%私聊信息
-define(DB_ETC_KEY_BATTLE_RANK, 26).    %%战役关卡排行
-define(DB_ETC_KEY_LOGOUT, 27).         %%登出数据记录
-define(DB_ETC_KEY_RANK_SERVER, 28).    %%rank_server数据
-define(DB_ETC_KEY_RULE, 29).          %%秩序战场数据
-define(DB_ETC_KEY_FESTIVAL, 30).      %%节日活动数据
-define(DB_ETC_KEY_MELEE, 31).          %%大乱斗活动数据

%% 合服需要处理的表，如果有新加的表，需要在这里加上,并在db_sql中加上对应的trans函数处理，如果对已有表结构进行改动，亦需要修改对应的trans函数
-define(all_role_id_tables, 
        [
		 gActivity,
         gBagItem,
         gBestPassChapter,
         gCard,
         gChapter,
         gDungeon,
         gEquip,
         gFighterList,
         gFriend,
         gGather,
         gGer,
         gGift,
         gGuide,
         gHron,
         gInvite,
         gInviteRoleList,
         gLimit,
         gOfflineDeductGold,
         gOfflinePayLog,
         gPay,
         gPush,
         gRole,
         gRoleExtra,
         gShopNum,
         gEncounter,
         gLieuInfo,
         gTalk,
         gOtherRecord,
         gTreasureHouse,
		 gTask]).