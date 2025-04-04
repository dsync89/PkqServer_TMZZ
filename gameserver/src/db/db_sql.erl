%% @author crimoon11
%% @doc @todo Add description to db_sql.


-module(db_sql).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_mail.hrl").
-compile(export_all).
-include("def_hist.hrl").
-include("def_task.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-define(GET_SQL_MAIL_NUM, 70).% the max num of mails get from sql once

%% 获取日志表的表名,日志表按月来分表
get_log_table_name(t_gold_pay_add) ->
    t_gold_pay_add;
get_log_table_name(TableName) when erlang:is_atom(TableName) ->
	{{Y, M, _}, _} = erlang:localtime(),
	lists:flatten(io_lib:format("~w_~w_~w", [TableName,Y,M])).

%% ====================================================================
%% 玩家数据 持久化
%% ====================================================================
sql_execute_with_log(Sql)	->
	case emysql:execute(?DB,Sql) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~s*****execute with err:~p,~s",[Sql,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~s*****execute with err:~p~n",[Sql,Exception])
	end.

%% 传入的Sql变量为多条sql语句的集合,最好加入事务的保护,保证整个数据操作的原子性
sql_execute_with_log2(Sql)   ->
    case emysql:execute(?DB,Sql) of
        [{ok_packet, _,_,_RetId,_,_,_},{ok_packet, _,_,RetId,_,_,_}]    ->
            {ok,RetId};
        {result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
            {ok,Rows};
        {error_packet, _, _,ErrCode,Reason2}    ->
            ?ERR("sql ****~s*****execute with err:~p,~s",[Sql,ErrCode,Reason2]),
            {error,{ErrCode,Reason2}};
        Exception ->
            ?ERR("sql ****~s*****execute with err:~p~n",[Sql,Exception])
    end.

sql_execute_sqls(Sql) ->
	Result = emysql:execute(?DB, Sql),
	lists:foldl(fun(E, CntAcc)->
						case E of
							{ok_packet, _,_,_RetId,_,_,_}	->
								CntAcc+1;
							{result_packet, _SeqNum,_FieldList,_Rows,_Extra} ->
								CntAcc+1;
							{error_packet, _, _,ErrCode,Reason2} ->
								?ERR("sql ****~s*****execute ~wth sqls with err:~p,~s",[Sql,CntAcc,ErrCode,Reason2]),
								CntAcc+1;
							Exception ->
								?ERR("sql ****~s*****execute ~wth sqls with err:~p~n",[Sql,CntAcc,Exception])
						end
					end,0, Result),
	ok.

sql_execute_with_log(Statement, Args)	->
	case emysql:execute(?DB,Statement,Args) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~p,~p*****execute with err:~p,~s",[Statement,Args,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~p,~p*****execute with err:~p~n",[Statement,Args,Exception])
	end.

get_all(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,List} ->
			List;
		_ ->
			[]
	end.

get_all(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			List;
		_ ->
			[]
	end.

get_row(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.

get_row(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.

get_fight_power_rank_list() ->
    case get_all("select roleID,fightPower from gRole where fightPower >= 10000") of
        [] ->
            [];
        List ->
            List
    end.

get_level_rank_list() ->
    case get_all("select roleID,level from gRole where level >= 10") of
        [] ->
            [];
        List ->
            List
    end.

%%删除所有机器人数据
delete_all_android()->
	sql_execute_with_log("delete from gRole where accid<10000").

search_all_roleName()->
	case get_all("select roleName from gRole;") of
		[]->
			[];
		List->
			[quote2(binary_to_list(RN))||RN<-lists:append(List)]
	end.

update_roleName(RoleID, NewName) ->
    Sql = io_lib:format("update gRole set roleName = ~s where roleID = ~w", [quote(NewName), RoleID]),
    sql_execute_with_log(Sql).

search_roleName(Name) ->
	Sql = io_lib:format("select roleID from gRole where roleName=~s;",[quote(Name)]),
	case get_row(Sql) of
		[RoleID] ->
			RoleID;
		_ ->
			?undefined
	end.
search_fuzzy_roleName(Name) ->
	Sql = io_lib:format("select roleID from gRole where roleName like '%~s%' and accid>10000 limit 30;",[quote2(Name)]),
	case get_all(Sql) of
		[]->
			?undefined;
		List->
			lists:append(List)
	end.

search_roleName2(Name) ->
	Sql = io_lib:format("select accid,roleID from gRole where roleName=~s;",[quote(Name)]),
	case get_row(Sql) of
		[AccountID, RoleID] ->
			[AccountID, RoleID];
		_ ->
			?undefined
	end.

get_all_roles()	->
	case get_row("select max(roleID) from gRole;") of
		[MaxId] when is_integer(MaxId) ->
			get_roles([], MaxId div 2000 + 2, MaxId div 2000 + 2);
		_ ->
			[]
	end.

get_roles(L, N, M) ->
	if N > 0 ->
			NextRL=get_roles(M - N),
			get_roles([NextRL|L], N - 1, M);
		true ->
			lists:flatten(L)
	end.

get_roles(M)	->
	RobotId = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID >= ~w and roleId < ~w;", [RobotId + 2000 * M, RobotId + 2000*(M + 1)]),
	case get_all(Sql) of
		[_|_]=List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_level_role_id_list(L, H) ->
    case data_setting:get(is_release) of
        false ->
            Sql = io_lib:format("select roleID from gRole where level >= ~w and level <= ~w", [L, H]),
            case get_all(Sql) of
                [_|_] = List ->
                    [E||[E]<-List];
                _ ->
                    []
            end;
        true ->
            Sql = io_lib:format("select roleID from gRole where level >= ~w and level <= ~w and roleID >= ~w", [L, H, tk_id:robot_roleID_max()]),
            case get_all(Sql) of
                [_|_] = List ->
                    [E||[E]<-List];
                _ ->
                    []
            end
    end.
	
get_level_roles(L,H) ->
	Sql = io_lib:format("select max(roleID) from gRole where level >= ~w and level < ~w ;", [L,H]),
	case get_row(Sql) of
		[MaxID] when is_integer(MaxID) ->
			get_level_roles([],MaxID div 2000 + 2, MaxID div 2000 + 2, L, H);
		_ ->
			[]
	end.

get_level_roles(L,N,M,LL,LH) ->
	if N > 0 ->
		   NextRL = get_level_roles(M-N,LL,LH),
		   get_level_roles([NextRL|L],N-1,M,LL,LH);
	   true ->
		   lists:flatten(L)
	end.

get_level_roles(M,LL,LH)->
	RobotID = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID >=~w and roleID < ~w and level >= ~w and level < ~w;", [RobotID + 2000 * M, RobotID + 2000 * (M+1), LL, LH]),
	case get_all(Sql) of
		[_|_] = List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_src_type_roles(SrcType) ->
    Sql = io_lib:format("select roleID from gRole where srcType = ~w;", [SrcType]),
    case get_all(Sql) of
        [_|_] = List ->
            [E||[E]<-List];
        _ ->
            []
    end.

get_level_and_vip_roles(LevelLow,LevelHigh,VipLow,VipHigh) ->
    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and level < ~w and vipLevel >= ~w and vipLevel < ~w;",
                           [tk_id:robot_roleID_max(),LevelLow,LevelHigh,VipLow,VipHigh]),
    case get_all(Sql) of
        [_|_] = List ->
            [E||[E]<-List];
        _ ->
            []
    end.

get_vip_roles(L,H) ->
	Sql = io_lib:format("select max(roleID) from gRole where vipLevel >= ~w and vipLevel < ~w ;", [L,H]),
	case get_row(Sql) of
		[MaxID] when is_integer(MaxID) ->
			get_vip_roles([],MaxID div 2000 + 2, MaxID div 2000 + 2, L, H);
		_ ->
			[]
	end.

get_vip_roles(L,N,M,LL,LH) ->
	if N > 0 ->
		   NextRL = get_vip_roles(M-N,LL,LH),
		   get_vip_roles([NextRL|L],N-1,M,LL,LH);
	   true ->
		   lists:flatten(L)
	end.

get_vip_roles(M,LL,LH)->
	RobotID = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID >=~w and roleID < ~w and vipLevel >= ~w and vipLevel < ~w;", [RobotID + 2000 * M, RobotID + 2000 * (M+1), LL, LH]),
	case get_all(Sql) of
		[_|_] = List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_roleIDList() ->
	RobotId = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID > ~w limit 2000;", [RobotId]),
	case get_all(Sql) of
		[_|_]=List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_roleIDList_noRobot() ->
	RobotId = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID >= ~w", [RobotId]),
	case get_all(Sql) of
		List when erlang:is_list(List) ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_gold(RoleID) ->
	Sql = io_lib:format("select gold,goldBonus from gRole where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[Gold, GoldBonus] ->
			{Gold, GoldBonus};
		_ ->
			{0,0}
	end.

get_offlineDeductGold(RoleID) ->
	Sql = io_lib:format("select deductGold from gOfflineDeductGold where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[DGold] ->
			DGold;
		_ ->
			0
	end.

set_offlineDeductGold(RoleID, 0) ->
	Sql = io_lib:format("delete from gOfflineDeductGold where roleID=~w;",[RoleID]),
	sql_execute_with_log(Sql);	
set_offlineDeductGold(RoleID, DeductGold)->
	Sql = io_lib:format("replace into gOfflineDeductGold values(~w,~w);",[RoleID, DeductGold]),
	sql_execute_with_log(Sql).

%% 离线充值记录
get_offlinePayLog(RoleID) ->
	Sql = io_lib:format("select payItemID,receipt,receiptMd5,SrcType from gOfflinePayLog where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] =List ->
			List;
		_ ->
			[]
	end.

%% APP充值失败的订单记录
set_failedPayLog(RoleID,Receipt) ->
	Sql = io_lib:format("insert into gFailedPayLog values(null,~w,'~s',~w,'~s');",[RoleID,Receipt,1,datetime(erlang:localtime())]),
	sql_execute_with_log(Sql).

clear_offlinePayLog(RoleID) ->
	Sql = io_lib:format("delete from gOfflinePayLog where roleID=~w;",[RoleID]),
	sql_execute_with_log(Sql).

%% 充值重复验证表
check_pay_receipt_duplicate(Md5)->
	Sql = io_lib:format("select count(*) from gPay where receiptMd5='~s';",[Md5]),
	case get_row(Sql) of
		[0] ->
			true;
		_ ->
			false
	end.

add_pay_receipt(Md5,RoleID,Receipt,SrcType, PayGold) ->
	Sql = io_lib:format("insert into gPay values('~s',~w,'~s',~w,'~s',~w);",[Md5,RoleID,Receipt,SrcType,datetime(erlang:localtime()), PayGold]),
	sql_execute_with_log(Sql).
	
get_role_accid(RoleID) ->
	Sql = io_lib:format("select accid from gRole where roleID = ~w;", [RoleID]),
	case get_row(Sql) of
		[X] ->
			X rem ?AccidBase;
		_ ->
			0
	end.
	
add_offlinePayLog(RoleID, PayItemID, Receipt, Md5, SrcType) ->
	Sql = io_lib:format("insert into gOfflinePayLog values(~w,~w,~s,'~s',~w);",[RoleID,PayItemID,quote(Receipt), Md5,SrcType]),
	sql_execute_with_log(Sql).

add_offlinePayAmountLog(RoleID, Amount, Receipt, Md5, SrcType) ->
    Sql = io_lib:format("insert into gOfflinePayAmountLog values(~w,~w,~s,'~s',~w);",[RoleID,Amount,quote(Receipt), Md5,SrcType]),
    sql_execute_with_log(Sql).

get_offlinePayAmountLog(RoleID) ->
    Sql = io_lib:format("select amount,receipt,receiptMd5,SrcType from gOfflinePayAmountLog where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_] =List ->
            List;
        _ ->
            []
    end.

clear_offlinePayAmountLog(RoleID) ->
    Sql = io_lib:format("delete from gOfflinePayAmountLog where roleID=~w;",[RoleID]),
    sql_execute_with_log(Sql).

get_role_lastLogoutTime(RoleID)->
	Sql = io_lib:format("select lastLogoutTime from gRole where roleID = ~w", [RoleID]),
	case get_row(Sql) of
		[LastLogoutTime] ->
			LastLogoutTime;
		_ ->
			util:now()
	end.

%% 日志表按月分开存储，只取本月的记录
get_role_lastLoginTime(RoleID)->
	TableName = get_log_table_name(logLogin),
	Sql = io_lib:format("select datetime from ~s where RoleID = ~w order by datetime desc limit 1", [TableName, RoleID]),
	case get_row(Sql) of
		[LoginTime] ->
			util:toUnixTime(LoginTime);
		_ ->
			util:now()
	end.
	
get_roleInfo(RoleID) ->
    Sql = io_lib:format("SELECT accid,roleName,isMale,level,exp,coin,reputation,gold,goldBonus,goldUsed,vipLevel,goldTotalPaid,title,fightPower,lastLogoutTime,familyID,lastJoinFamily,head,payExtReward,location,isFailed,timeVipDraw,lastPayTime,devid,srcType FROM gRole WHERE roleID= ~w", [RoleID]),
	case get_row(Sql) of
		[Vaccid,VroleName,VisMale,Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,VvipLevel,VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime,VFamilyID,VLastJoinFamily,Head,VPayExtReward,Location,IsFailed,TimeVipDraw,LastPayTime,DeviceID,SrcType]->
            {VPayExtReward2, LastPayTime2} = role_lib:update_pay_ext_reward(VPayExtReward, LastPayTime),
			#role{roleID=RoleID,description="",familyID=VFamilyID,lastJoinFamily=VLastJoinFamily,accid=Vaccid,roleName=VroleName,
                  isMale=int2bool(VisMale),level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,gold=Vgold,goldBonus=VgoldBonus,
                  goldUsed=VgoldUsed,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,fightPower=VfightPower,
                  lastLogoutTime=VlastLogoutTime,head=Head,payExtReward=VPayExtReward2,
                  location=Location,isFailed=int2bool(IsFailed),timeVipDraw=TimeVipDraw,lastPayTime=LastPayTime2,deviceID=DeviceID,srcType=SrcType};
		_ ->
			undefined
	end.

check_roleCreated(Accid) ->
    Sql = io_lib:format("select roleID from gRole where accid= ~w", [Accid]),
	case get_row(Sql) of
		[] ->
			false;
		[RoleID] ->
			{true,RoleID}
	end.
	
create_roleInfo(RoleInfo) ->
	#role{roleID=RoleID,accid=Vaccid,roleName=VroleName,isMale=VisMale,level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,gold=Vgold,goldBonus=VgoldBonus,
          goldUsed=VgoldUsed,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,fightPower=VfightPower,lastLogoutTime=VlastLogoutTime
         ,familyID=VFamilyID,lastJoinFamily=VLastJoinFamily,head=Head,payExtReward=VPayExtReward,location=Location,isFailed=IsFailed,
          timeVipDraw=TimeVipDraw,lastPayTime=LastPayTime,deviceID=DeviceID,srcType=SrcType}=RoleInfo,
	Sql = io_lib:format("insert into gRole values(~w,~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~w,~w,~w,~s,~w);",
						[RoleID,Vaccid,quote(VroleName),bool2int(VisMale),Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,
                         VvipLevel,VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime,VFamilyID,VLastJoinFamily,Head,VPayExtReward,
                         quote(Location),bool2int(IsFailed),TimeVipDraw,LastPayTime,quote(DeviceID),SrcType]),
	sql_execute_with_log(Sql).

update_roleInfo(RoleInfo) ->
	#role{roleID=RoleID,level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,gold=Vgold,goldBonus=VgoldBonus,goldUsed=VgoldUsed,
          vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,fightPower=VfightPower,lastLogoutTime=VlastLogoutTime,
          familyID=VFamilyID,lastJoinFamily=VLastJoinFamily,head=Head,payExtReward=VPayExtReward,location=Location,isFailed=IsFailed,
          timeVipDraw=TimeVipDraw,lastPayTime=LastPayTime,deviceID=DeviceID}=RoleInfo,
    Sql = io_lib:format("update gRole set level= ~w,exp= ~w,coin= ~w,reputation= ~w,gold= ~w,goldBonus= ~w,goldUsed= ~w,vipLevel= ~w,goldTotalPaid= ~w,title= ~w,fightPower= ~w,lastLogoutTime= ~w,FamilyID = ~w,lastJoinFamily = ~w,head=~w,payExtReward = ~w,location = ~s,isFailed = ~w,timeVipDraw=~w,lastPayTime=~w,devid=~s  where roleID= ~w",
                        [Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,VvipLevel,VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime,VFamilyID,VLastJoinFamily,Head,VPayExtReward,quote(Location),bool2int(IsFailed),TimeVipDraw,LastPayTime,quote(DeviceID),RoleID]),
    sql_execute_with_log(Sql).

update_role_family_id(RoleID, FamilyID) ->
    Sql = io_lib:format("update gRole set familyID = ~w where roleID = ~w", [FamilyID, RoleID]),
    sql_execute_with_log(Sql).

update_role_family_id(RoleID, FamilyID, LastJoinFamily) ->
    Sql = io_lib:format("update gRole set familyID = ~w, lastJoinFamily = ~w where roleID = ~w", [FamilyID, LastJoinFamily, RoleID]),
    sql_execute_with_log(Sql).

get_roleExtra(RoleID) -> 
    Sql = io_lib:format("select battleProgress,battleProgressHard,battleProgressFastHard,energy,energyBuyTimes,
                       challengeGodEnergy,challengeGodBuyTimes,lastChallengeGodDate,refreshLieuTimes,alreadyPayRefreshLieuTimes,dscvBuyTimes,meleeSignTimes,
                       lastMeleeSignDate,pvpBuyTimes,ruleBuyTimes,coinBuyTimes,coinBossHP,fireTimes,lastBuyTimesRefreshDate,lastEnergyTime,discoveryTimes,lastDscvTime,
                       dscvCount,pvpTimes,ruleTimes,weiboCount,nextWeiboCountRefreshSec,lastWeiXinShareSec,lastPvpTime,lastRuleTime,
                       encounterList,lastTitleRewardDate,lastDrawTitle,lastLoggedLoginDate,lastDrawLoginRewardDays,loginDays,lastDrawLevelUpLevel,
                        randomShopList,dailyDrawList from gRoleExtra where roleID= ~w", [RoleID]),
    case get_row(Sql) of
        [VbattleProgress,VbattleProgressHard,VbattleProgressFastHard,Venergy,VenergyBuyTimes,VChallengeGodEnergy, VChallengeGodBuyTimes,{date,VlastChallengeGodDate},VRefreshLieuTimes,
         VAlreadyPayRefreshLieuTimes,VdscvBuyTimes,VmeleeSignTimes,{date,VlastMeleeSignDate},VpvpBuyTimes,VruleBuyTimes,VcoinBuyTimes,VcoinBossHP,VfireTimes,{date,VlastBuyTimesRefreshDate},VlastEnergyTime,VdiscoveryTimes,
         VlastDscvTime,VdscvCount,VpvpTimes,VruleTimes,VweiboCount,VnextWeiboCountRefreshSec,VlastWeiXinShareSec,VlastPvpTime,VlastRuleTime,VencounterList0,
         {date,VlastTitleRewardDate},VlastDrawTitle,{date,VlastLoggedLoginDate},VlastDrawLoginRewardDays,VloginDays,VlastDrawLevelUpLevel,VrandomShopList0,VdailyDrawList0] ->
			VencounterList = to_term(VencounterList0),
			VrandomShopList=to_term(VrandomShopList0),
            VitemUseList = get_role_item_use_list(RoleID),
            VdailyDrawList = to_term(VdailyDrawList0);
		_Err ->
			VbattleProgress=role_battle:get_first_dungeonID(),
			VbattleProgressHard=role_battle:get_first_hard_dungeonID(),
			VbattleProgressFastHard = role_battle:get_first_fast_hard_dungeonID(),
			Venergy=role_lib:get_max_energy(0),
			VenergyBuyTimes=0,
			VChallengeGodEnergy=data_common:get(challengeGodTimes),
			VChallengeGodBuyTimes=0,
			VlastChallengeGodDate={1989,7,17},
			VRefreshLieuTimes=data_lieu_clo_setting:get(daily_free_refresh),
			VAlreadyPayRefreshLieuTimes=0,
			VdscvBuyTimes=0,
            VmeleeSignTimes=0,
            VlastMeleeSignDate={1989,7,17},
			VpvpBuyTimes=0,
			VruleBuyTimes=0,
			VcoinBuyTimes=0,
            VcoinBossHP=0,
			VfireTimes=0,
			VlastBuyTimesRefreshDate={1989,7,17},
			VlastEnergyTime=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VdiscoveryTimes=0,
			VlastDscvTime=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VdscvCount=0,
			VpvpTimes=data_common:get(max_pvp_times),
			VruleTimes=data_common:get(max_rule_times),
			VweiboCount=0,
			VnextWeiboCountRefreshSec=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VlastWeiXinShareSec=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VlastPvpTime=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VlastRuleTime=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VencounterList=[],
			VlastTitleRewardDate={2013,1,1},
			VlastDrawTitle=0,
			VlastLoggedLoginDate={2013,1,1},
			VlastDrawLoginRewardDays=0,
			VloginDays=0,
			VlastDrawLevelUpLevel=0,
			VrandomShopList=[],
            VitemUseList=[],
            {Year,Month,_} = erlang:date(),
            VdailyDrawList={{Year,Month},[]}
	end,
	RoleTimes = #roleTimes{energy=Venergy,
						   challengeGodEnergy=VChallengeGodEnergy,
						   challengeGodBuyTimes=VChallengeGodBuyTimes,
						   lastChallengeGodDate=VlastChallengeGodDate,
						   refreshLieuTimes=VRefreshLieuTimes,
						   alreadyPayRefreshLieuTimes=VAlreadyPayRefreshLieuTimes,
						   energyBuyTimes=VenergyBuyTimes,
						   dscvBuyTimes=VdscvBuyTimes,
                           meleeSignTimes=VmeleeSignTimes,
                           lastMeleeSignDate=VlastMeleeSignDate,
						   pvpBuyTimes=VpvpBuyTimes,
						   ruleBuyTimes=VruleBuyTimes,
						   coinBuyTimes=VcoinBuyTimes,
                           coinBossHP=VcoinBossHP,
						   fireTimes=VfireTimes,
						   lastBuyTimesRefreshDate=VlastBuyTimesRefreshDate,
						   lastEnergyTime=VlastEnergyTime,
						   discoveryTimes=VdiscoveryTimes,
						   lastDscvTime=VlastDscvTime,
						   dscvCount=VdscvCount,
						   pvpTimes=VpvpTimes,
						   ruleTimes=VruleTimes,
						   weiboCount=VweiboCount,
						   nextWeiboCountRefreshSec=VnextWeiboCountRefreshSec,
						   lastWeiXinShareSec=VlastWeiXinShareSec,
						   lastPvpTime=VlastPvpTime,
						   lastRuleTime=VlastRuleTime
						  },
	DailyInfo = #daily{lastTitleRewardDate     = VlastTitleRewardDate     
					  ,lastDrawTitle           = VlastDrawTitle           
					  ,lastLoggedLoginDate     = VlastLoggedLoginDate     
					  ,lastDrawLoginRewardDays = VlastDrawLoginRewardDays 
					  ,loginDays               = VloginDays               
					  ,lastDrawLevelUpLevel    = VlastDrawLevelUpLevel
                      ,dailyDrawList=VdailyDrawList
					  },	
	VbattleProgressHard2 = 
		if VbattleProgressHard =:= 0 ->
			   role_battle:get_first_hard_dungeonID();
		   true ->
			   VbattleProgressHard
		end,
	VbattleProgressFastHard2 = 
		if VbattleProgressHard =:= 0 ->
			   role_battle:get_first_fast_hard_dungeonID();
		   true ->
			   VbattleProgressFastHard
		end,
	{VbattleProgress,VbattleProgressHard2, VbattleProgressFastHard2,RoleTimes, VencounterList, DailyInfo, VrandomShopList, VitemUseList}.

set_roleExtra(RoleID, VbattleProgress, VbattleProgressHard, VbattleProgressFastHard,RoleTimes, VencounterList, DailyInfo, VrandomShopList, VitemUseList) ->	
	#roleTimes{energy=Venergy,
			   challengeGodEnergy=VChallengeGodEnergy,
			   challengeGodBuyTimes=VChallengeGodBuyTimes,
			   lastChallengeGodDate=VlastChallengeGodDate,
			   refreshLieuTimes=VRefreshLieuTimes,
			   alreadyPayRefreshLieuTimes=VAlreadyPayRefreshLieuTimes,
			   energyBuyTimes=VenergyBuyTimes,
			   dscvBuyTimes=VdscvBuyTimes,
               meleeSignTimes=VmeleeSignTimes,
               lastMeleeSignDate=VlastMeleeSignDate,
			   pvpBuyTimes=VpvpBuyTimes,
			   ruleBuyTimes=VruleBuyTimes,
			   coinBuyTimes=VcoinBuyTimes,
               coinBossHP=VcoinBossHP,
			   fireTimes=VfireTimes,
			   lastBuyTimesRefreshDate=VlastBuyTimesRefreshDate,
			   lastEnergyTime=VlastEnergyTime,
			   discoveryTimes=VdiscoveryTimes,
			   lastDscvTime=VlastDscvTime,
			   dscvCount=VdscvCount,
			   pvpTimes=VpvpTimes,
			   ruleTimes=VruleTimes,
			   weiboCount=VweiboCount,
			   nextWeiboCountRefreshSec=VnextWeiboCountRefreshSec,
			   lastWeiXinShareSec=VlastWeiXinShareSec,
				lastPvpTime=VlastPvpTime,
				lastRuleTime=VlastRuleTime
			  } = RoleTimes,
	#daily{lastTitleRewardDate     = VlastTitleRewardDate     
		   ,lastDrawTitle           = VlastDrawTitle           
		   ,lastLoggedLoginDate     = VlastLoggedLoginDate     
		   ,lastDrawLoginRewardDays = VlastDrawLoginRewardDays 
		   ,loginDays               = VloginDays               
		   ,lastDrawLevelUpLevel    = VlastDrawLevelUpLevel
           ,dailyDrawList=VdailyDrawList
		  } = DailyInfo,
	Sql=io_lib:format("replace into gRoleExtra values(~w,~w,~w,~w,~w,~w,~w,~w,'~s',~w,~w,~w,~w,'~s',~w,~w,~w,~w,~w,'~s',~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,'~s',~w,'~s',~w,~w,~w,~s,~s);",
					  [RoleID,
					   VbattleProgress,VbattleProgressHard,VbattleProgressFastHard,Venergy,VenergyBuyTimes,VChallengeGodEnergy, VChallengeGodBuyTimes,date(VlastChallengeGodDate),VRefreshLieuTimes,VAlreadyPayRefreshLieuTimes,
					   VdscvBuyTimes,VmeleeSignTimes,date(VlastMeleeSignDate),VpvpBuyTimes,VruleBuyTimes,VcoinBuyTimes,VcoinBossHP,VfireTimes,date(VlastBuyTimesRefreshDate),VlastEnergyTime,VdiscoveryTimes,VlastDscvTime,VdscvCount,VpvpTimes,VlastPvpTime,VruleTimes,VlastRuleTime,VweiboCount,VnextWeiboCountRefreshSec,
					   VlastWeiXinShareSec,to_bin(VencounterList),
					   date(VlastTitleRewardDate),VlastDrawTitle,date(VlastLoggedLoginDate),VlastDrawLoginRewardDays,VloginDays,VlastDrawLevelUpLevel,
					   to_bin(VrandomShopList),to_bin(VdailyDrawList)]
					  ),
	sql_execute_with_log(Sql),
    set_role_item_use_list(RoleID, VitemUseList).

get_role_item_use_list(RoleID) ->
    Sql = io_lib:format("select itemTypeID,useDate,useTimes from gItemUse where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [#item_use_info{itemTypeID=ItemTypeID,useDate=UseDate,useTimes=UseTimes}||[ItemTypeID,{date, UseDate},UseTimes]<-List];
        _ ->
            []
    end.

set_role_item_use_list(RoleID, ItemUseList) ->
    ArgList = [[RoleID,ItemTypeID,date(UseDate),UseTimes]||#item_use_info{itemTypeID=ItemTypeID,useDate=UseDate,useTimes=UseTimes}<-ItemUseList],
    DeleteSql = io_lib:format("delete from gItemUse where roleID=~w;", [RoleID]),
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql);
       true ->
           InsertSql = make_sql_batch("insert into gItemUse values", "(~w,~w,'~s',~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.
	
get_role_encounterList(RoleID)->
	Sql = io_lib:format("select * from gEncounter where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[] ->
			init_role_encounterList(RoleID),
			Sql = io_lib:format("select * from gEncounter where roleID = ~w",[RoleID]),
			case get_row(Sql) of
				[_|_] ->
					get_role_encounterList(RoleID);
				_ ->
					?ERR("create role encounterList fail with roleID :~w",[RoleID])
			end;
		[RoleID, MonsterRank, EncounterInfo] ->
			{MonsterRank, uncompress_decode(EncounterInfo)};
		X ->
			?ERR("get role encounterList err:~w",[X]),
			{0,[]}
	end.

init_role_encounterList(RoleID)->
	Sql = io_lib:format("insert into gEncounter values (~w,~w,~s)",[RoleID, 0, quote(compress_encode([]))]),
	sql_execute_with_log(Sql).

set_role_encounterList(RoleID, EncounterInfo)->
	{MonsterRank, EncounterList} = EncounterInfo,
	Sql = io_lib:format("replace into gEncounter values (~w,~w,~s)",[RoleID, MonsterRank, quote(compress_encode(EncounterList))]),
	sql_execute_with_log(Sql).


get_shopNumList(RoleID) ->
	Sql = io_lib:format("select shopID,sellID,buyNum from gShopNum where roleID=~w",[RoleID]),
	case get_all(Sql) of
		[_|_] = ShopNumList0 ->
			ShopNumList = [#p_shop_num{shopID=ShopID,sellID=SellID,buyNum=BuyNum}||[ShopID,SellID,BuyNum]<-ShopNumList0];
		_ ->
			ShopNumList=[]
	end,
	ShopNumList.

set_shopNumList(RoleID,ShopNumList) ->
	sql_execute_with_log(io_lib:format("delete from gShopNum where roleID=~w;",[RoleID])),
	ArgList = [[RoleID, ShopID,SellID,BuyNum] || #p_shop_num{shopID=ShopID,sellID=SellID,buyNum=BuyNum} <- ShopNumList],
	if ShopNumList == [] ->
		   {ok,0};
	   true ->
		   Sql = make_sql_batch("insert into gShopNum values", "(~w,~w,~w,~w)", ArgList),
		   sql_execute_with_log(Sql)
	end.



get_cardInfo(RoleID) ->
	Sql = io_lib:format("select openedCardList,cardList,drawCount from gCard where roleID=~w",[RoleID]),
	case get_row(Sql) of
		[OpenCardList0,CardList0,DrawCount] ->
			#cardInfo{cardList=to_term(CardList0),drawCount=DrawCount,openedCardList=to_term(OpenCardList0)};
		_ ->
			#cardInfo{cardList=[],drawCount=0,openedCardList=[]}
	end.

set_cardInfo(RoleID,CardInfo) ->
	Sql = io_lib:format("replace into gCard values(~w,~s,~s,~w);",
						[RoleID,
						 to_bin(CardInfo#cardInfo.openedCardList),
						 to_bin(CardInfo#cardInfo.cardList),
						 CardInfo#cardInfo.drawCount
						]),
	sql_execute_with_log(Sql).


get_treasure_house_info(RoleID) ->
	Sql = io_lib:format("select value_info, card_list, free_count, buy_count, free_times, mark,baseBoxGetProcess, isGetRankReward,lastExploreDate,activityID from gTreasureHouse where roleID = ~w", [RoleID]),
	case get_row(Sql) of
		[ValueInfo, CardList0, FreeCount, BuyCount, FreeTimes, Mark, BaseBoxGetProcess, IsGetRankReward,{date,LastExploreDate},ActivityID] ->
			FreeTimes2 = 
				case LastExploreDate == erlang:date() of
					true ->
						FreeTimes;
					_ ->
						data_treasure_box:get(treasure_house_free_times)
				end,
			FreeTimes3 = 
				case FreeCount =< 5 andalso erlang:date() == {2014,1,27} of
					true ->
					   data_treasure_box:get(treasure_house_free_times);
				   _ ->
					   FreeTimes2
				end,
			#treaHouseInfo{value_info=ValueInfo, card_list=to_term(CardList0), free_count=FreeCount,
						   buy_count=BuyCount, free_times=FreeTimes3, mark=Mark, baseBoxGetList=to_term(BaseBoxGetProcess),
						   isGetRankReward=IsGetRankReward,activityID=ActivityID};
		_ ->
			FreeTimes = data_treasure_box:get(treasure_house_free_times),
			#treaHouseInfo{value_info=1, card_list=[],free_count=0,buy_count=0, free_times=FreeTimes,
						   mark=0, baseBoxGetList=[],isGetRankReward=0,activityID=0}
	end.

set_treasure_house_info(RoleID, TreaHouseInfo)->
	Sql = io_lib:format("replace into gTreasureHouse values (~w,~w, ~s,~w,~w,~w,~w,~s,~w,'~s',~w);",
						[RoleID, TreaHouseInfo#treaHouseInfo.value_info,to_bin(TreaHouseInfo#treaHouseInfo.card_list),
						 TreaHouseInfo#treaHouseInfo.free_count, TreaHouseInfo#treaHouseInfo.buy_count,
						 TreaHouseInfo#treaHouseInfo.free_times, TreaHouseInfo#treaHouseInfo.mark,
						 to_bin(TreaHouseInfo#treaHouseInfo.baseBoxGetList),TreaHouseInfo#treaHouseInfo.isGetRankReward,
						 date(erlang:date()),TreaHouseInfo#treaHouseInfo.activityID]),
	sql_execute_with_log(Sql).

erase_all_treaHouse_info()->
	ok.
%% 	Sql=io_lib:format("delete from gTreasureHouse;", []),
%% 	sql_execute_with_log(Sql).

get_hronInfo(RoleID) ->
	Sql = io_lib:format("select * from gHron where roleID=~w", [RoleID]),
	case get_row(Sql) of
		[_, {date,Vdate},VcurDungeonNum,VmaxDungeonNum,VattackAdd,VhpAdd,DungeonID,VchallengeTimes,VCoinBuyTimes,VGoldBuyTimes] ->
			
			#hronInfo{
					  date          = Vdate          
					  ,curDungeonNum = VcurDungeonNum
                      ,maxDungeonNum = VmaxDungeonNum
					  ,attackAdd     = VattackAdd     
					  ,hpAdd         = VhpAdd         
					  ,dungeonID = DungeonID
					  ,challengeTimes = VchallengeTimes
                      ,coinBuyTimes = VCoinBuyTimes
                      ,goldBuyTimes = VGoldBuyTimes
					 };
		_ ->
			[]
	end.

set_hronInfo(RoleID, #hronInfo{}=HronInfo) ->
    #hronInfo{
              date          = Vdate          
              ,curDungeonNum = VcurDungeonNum
              ,maxDungeonNum = VmaxDungeonNum 
              ,attackAdd     = VattackAdd     
              ,hpAdd         = VhpAdd         
              ,dungeonID = VDungeonID 
              ,challengeTimes= VchallengeTimes
              ,coinBuyTimes = VCoinBuyTimes
              ,goldBuyTimes = VGoldBuyTimes
             } = HronInfo,
    Sql = io_lib:format("replace into gHron values(~w,'~s',~w,~w,~w,~w,~w,~w,~w,~w);",
                        [RoleID, date(Vdate),VcurDungeonNum,VmaxDungeonNum,VattackAdd,VhpAdd,VDungeonID,VchallengeTimes,VCoinBuyTimes,VGoldBuyTimes]),
    sql_execute_with_log(Sql);
set_hronInfo(_,_) ->
    ignore.

get_coinBattle(RoleID) ->
    Sql = io_lib:format("select * from gCoin where roleID=~w", [RoleID]),
    case get_row(Sql) of
        [_, {date,Date}, CoolDown, Times] ->
            case erlang:date() of
                Date ->
                    #coinBattle{date=Date, coolDown=CoolDown, times=Times};
                _ ->
                    #coinBattle{date=erlang:date(), coolDown=CoolDown, times=0}
            end;
        _ ->
            #coinBattle{date=erlang:date(), coolDown=0, times=0}
    end.

set_coinBattle(RoleID, #coinBattle{date=Date, coolDown=CoolDown, times=Times}) ->
    Sql = io_lib:format("replace into gCoin values(~w, '~s', ~w, ~w);", [RoleID, date(Date), CoolDown, Times]),
    sql_execute_with_log(Sql);
set_coinBattle(_, _) ->
    ignore.

get_monthCard(RoleID) ->
    Sql = io_lib:format("select * from gMonthCard where roleID=~w", [RoleID]),
    case get_row(Sql) of
        [_, EndTime, DrawTime, DayPayGold] ->
            #monthCard{endTime=EndTime, drawTime=DrawTime, dayPayGold=DayPayGold};
        _ ->
            #monthCard{endTime=0, drawTime=0, dayPayGold=0}
    end.

set_monthCard(RoleID, #monthCard{endTime=EndTime, drawTime=DrawTime, dayPayGold=DayPayGold}) ->
    Sql = io_lib:format("replace into gMonthCard values(~w, ~w, ~w, ~w);", [RoleID, EndTime, DrawTime, DayPayGold]),
    sql_execute_with_log(Sql).

get_limitInfo(RoleID) ->
	Sql = io_lib:format("select * from gLimit where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[_, VencounterNum,VisBindWeibo,VinviteRoleID,VinviteRoleName,VlastShareLevel,
		 VspiritGoldBoxCount,VspiritGoldBonusBoxCount,VspiritItemBoxCount, VequipGoldBoxCount,VequipGoldBonusBoxCount,VequipItemBoxCount]->
			#limitInfo{
					   encounterNum    = VencounterNum  
					   ,isBindWeibo     = int2bool(VisBindWeibo)   
					   ,inviteRoleID    = VinviteRoleID  
					   ,inviteRoleName  = VinviteRoleName
					   ,lastShareLevel  = VlastShareLevel
					   ,equipGoldBonusBoxCount=VequipGoldBonusBoxCount
					  ,equipGoldBoxCount=VequipGoldBoxCount
					  ,equipItemBoxCount=VequipItemBoxCount
					  ,spiritGoldBonusBoxCount=VspiritGoldBonusBoxCount
					  ,spiritGoldBoxCount=VspiritGoldBoxCount
					  ,spiritItemBoxCount=VspiritItemBoxCount
					  };
		_ ->
			undefined
	end.

set_limitInfo(RoleID, #limitInfo{}=LimitInfo) ->
	#limitInfo{
			   encounterNum    = VencounterNum  
			   ,isBindWeibo     = VisBindWeibo   
			   ,inviteRoleID    = VinviteRoleID  
			   ,inviteRoleName  = VinviteRoleName
			   ,lastShareLevel  = VlastShareLevel
			   ,equipGoldBonusBoxCount=VequipGoldBonusBoxCount
			   ,equipGoldBoxCount=VequipGoldBoxCount
			   ,equipItemBoxCount=VequipItemBoxCount
			   ,spiritGoldBonusBoxCount=VspiritGoldBonusBoxCount
			   ,spiritGoldBoxCount=VspiritGoldBoxCount
			   ,spiritItemBoxCount=VspiritItemBoxCount
			  } = LimitInfo,
	Sql = io_lib:format("replace into gLimit values (~w,~w,~w,~w,~s,~w,~w,~w,~w,~w,~w,~w);",
						[RoleID,VencounterNum,bool2int(VisBindWeibo),VinviteRoleID,quote(VinviteRoleName),VlastShareLevel,
						 VspiritGoldBoxCount,VspiritGoldBonusBoxCount,VspiritItemBoxCount, VequipGoldBoxCount,VequipGoldBonusBoxCount,VequipItemBoxCount]),
	sql_execute_with_log(Sql);
set_limitInfo(_,_)->
	ignore.

get_gatherInfo(RoleID) ->
	Sql = io_lib:format("select type,typeID from gGather where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = List ->
			lists:foldl(fun([1,E], [A,B,C]) ->
								[ [E|A], B,C];
						   ([2,E], [A,B,C]) ->
								[ A, [E|B],C];
                           ([3,E], [A,B,C]) ->
                                [ A, B,[E|C]]
						end, [[], [], []], List);
		_ ->
			[[],[],[]]
	end.

add_gatherInfo(RoleID, Type, TypeIDList) ->
	ArgList = [ [RoleID,Type,TypeID] || TypeID <- TypeIDList],
	if TypeIDList == [] ->
		   {ok,0};
	   true ->
	Sql = make_sql_batch("insert into gGather values","(~w,~w,~w)",ArgList),
	sql_execute_with_log(Sql)
	end.

get_bestPassChapterID(RoleID) ->
	Sql = io_lib:format("select chapterID from gBestPassChapter where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] =List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

add_bestPassChapterID(RoleID, ChapterID) ->
	Sql = io_lib:format("insert into gBestPassChapter values(~w,~w);",[RoleID,ChapterID]),
	sql_execute_with_log(Sql).

get_chapter(RoleID,ChapterID) ->
	Sql = io_lib:format("select bestRewarded,curDate from gChapter where roleID=~w and chapterID=~w;",[RoleID,ChapterID]),
	case get_row(Sql) of
		[BestRewarded0,{date,CurDate}]->
			BestRewarded = int2bool(BestRewarded0),
			Sql2 = io_lib:format("select dungeonID,restTimes,bestScore from gDungeon where roleID=~w and chapterID=~w;",[RoleID,ChapterID]),
			case get_all(Sql2) of
				[_|_]=List ->
					DungeonList = [#p_dungeon{bestScore=BestScore
											 ,dungeonID=DungeonID
											 ,restTimes=RestTimes} 
								  || [DungeonID,RestTimes,BestScore] <-List];
				_ ->
					DungeonList = []
			end,
			#chapter{
				  id=ChapterID
				  ,perfectRewarded=BestRewarded
				  ,dungeonList=DungeonList
				  ,curDate=CurDate
				  };
		_ ->
			?undefined
	end.

set_chapter(RoleID, ChapterList) ->
	ArgList = [[RoleID,ChapterID,bool2int(BestRewarded),date(CurDate)] ||
			   #chapter{
						id=ChapterID
						,perfectRewarded=BestRewarded
						,curDate=CurDate
					   } <- ChapterList],
	ArgList2 = [[RoleID,ChapterID,DungeonID,RestTimes,BestScore]||
				#chapter{id=ChapterID,dungeonList=DungeonList} <- ChapterList,
				#p_dungeon{bestScore=BestScore,dungeonID=DungeonID,restTimes=RestTimes} <- DungeonList],
	if ArgList==[] ->
		   ignore;
	   true ->
		   Sql = make_sql_batch("replace into gChapter values","(~w,~w,~w,'~s')",ArgList),
		   sql_execute_with_log(Sql)
	end,
	if ArgList2 == [] ->
		   {ok,0};
	   true ->
		   Sql2 = make_sql_batch("replace into gDungeon values", "(~w,~w,~w,~w,~w)", ArgList2),
		   sql_execute_with_log(Sql2)
	end.

get_equipList(RoleID) ->
	Sql = io_lib:format("select itemUID,itemTypeID,itemPos,itemLevel,itemRank,itemGerID,itemDecay,itemExp from gEquip where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_]=EquipList ->
			lists:foldl(
			  fun([ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,ItemDecay,ItemExp],{Acc,BagEquip}) ->
					  #data_item{itemType=ItemType} = data_item:get(ItemTypeID),
					  %% 如果是在身上的装备，则计算属性
					  Item = role_item:refresh_item(#item{itemUID=ItemUID
													 ,itemTypeID=ItemTypeID
													 ,addAttr=0
													 ,itemDecay=ItemDecay
													 ,itemLevel=ItemLevel
													 ,itemNum=1
													 ,itemPos=ItemPos
													 ,itemRank=ItemRank
													 ,itemType=ItemType
													 ,itemExp=ItemExp}),
					  if ItemGerID == 0 ->
							 {Acc, [Item|BagEquip]};
						 true ->
							 case lists:keytake(ItemGerID, 1, Acc) of
								 false ->
									 {[{ItemGerID,[Item]}| Acc], BagEquip};
								 {value, {_, L}, Acc2} ->
									 {[{ItemGerID, [Item|L]} | Acc2], BagEquip}							
							 end
					  end
			  end, {[],[]}, EquipList);
		_ ->
			{[],[]}
	end.

get_equipedList(RoleID)->
	Sql = io_lib:format("select itemUID,itemTypeID,itemPos,itemLevel,itemRank,itemGerID,itemDecay,itemExp from gEquip where roleID=~w AND itemPos > 0;",[RoleID]),
	case get_all(Sql) of
		[_|_]=EquipList ->
			EquipList;
		_ ->
			[]
	end.
	
set_equipList(RoleID, ListOfEquipList) ->
	DeleteSql = io_lib:format("delete from gEquip where roleID=~w",[RoleID]),
	ArgList = [ [ItemUID,RoleID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,item_lib:itemDecay(ItemDecay),ItemExp]
				|| {ItemGerID, ItemList} <- ListOfEquipList,
				   #item{itemUID=ItemUID
						 ,itemTypeID=ItemTypeID
						 ,itemDecay=ItemDecay
						 ,itemLevel=ItemLevel
						 ,itemPos=ItemPos
						 ,itemRank=ItemRank
						 ,itemExp = ItemExp} <- ItemList],
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql),
           {ok,0};
       true ->
           InsertSql = make_sql_batch("insert into gEquip values", "(~w,~w,~w,~w,~w,~w,~w,~w,~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ ";" ++ InsertSql)
    end.
	

get_bagItem(RoleID) ->
	Sql = io_lib:format("select itemUID,itemTypeID,itemNum from gBagItem where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_]=List ->
				[begin
					 #data_item{itemType=ItemType} = data_item:get(ItemTypeID),
					 #item{itemUID=ItemUID
													 ,itemTypeID=ItemTypeID
													 ,addAttr=0
													 ,itemDecay=0
													 ,itemLevel=1
													 ,itemNum=ItemNum
													 ,itemPos=0
													 ,itemRank=0
													 ,itemType=ItemType
													 ,itemExp=0}
				 end||[ItemUID,ItemTypeID,ItemNum]<-List];
		_ ->
			[]
	end.

set_bagItem(RoleID, BagItem) ->
    DeleteSql = io_lib:format("delete from gBagItem where roleID=~w;", [RoleID]),
    ArgList = [ [ItemUID,RoleID,ItemTypeID,ItemNum] || #item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemNum=ItemNum}<-BagItem],
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql),
           {ok,0};
       true ->
           InsertSql = make_sql_batch("insert into gBagItem values", "(~w,~w,~w,~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.

check_ger_select(RoleID) ->
	Sql = io_lib:format("select count(*) from gGer where roleID=~w;",[RoleID]),
	[Num] = get_row(Sql),
	if Num > 0 ->
		   true;
	   true ->
		   false
	end.
		
get_gerList(RoleID) ->
	Sql = io_lib:format("select * from gGer where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = List ->
			lists:foldl(fun([GerID,_,GerTypeID,GerLevel,GerExp,GerRank,GerPos], {PosListAcc, LPosListAcc, GerBagAcc}) ->
									   GerSimple = #gerSimple{gerExp=GerExp
															 ,gerID=GerID
															 ,gerLevel=GerLevel
															 ,gerPos=GerPos
															 ,gerQuality=GerRank
															 ,gerTypeID=GerTypeID},
								if GerPos == 0 ->
									   {PosListAcc, LPosListAcc, [GerSimple|GerBagAcc]};
									GerPos > ?guardGerPos ->
										{PosListAcc, [GerSimple#gerSimple{gerPos=GerPos-?guardGerPos}|LPosListAcc], GerBagAcc};
								   true ->
									   {[GerSimple|PosListAcc],LPosListAcc, GerBagAcc}
								end
						end, {[],[],[]}, List);									   
		_ ->
			{[], [],[]}
	end.

set_gerList(RoleID, PosList, GerList, GuardPosList) ->
	ArgList1 = lists:foldl(fun(Ger, Acc) ->
								   #ger{gerID=GerID,gerBase=GerBase} = Ger,
								   #gerBase{gerExp=GerExp
											,gerLevel=GerLevel
											,gerPos=GerPos
											,gerQuality=GerRank
											,gerTypeID=GerTypeID}=GerBase,
								   [[GerID,RoleID,GerTypeID,GerLevel,GerExp,GerRank,GerPos]|Acc]
						   end, [], PosList),
	ArgList2 = lists:foldl(fun(Ger, Acc) ->
								   #gerSimple{gerID=GerID
											  ,gerExp=GerExp
											  ,gerLevel=GerLevel
											  ,gerPos=GerPos
											  ,gerQuality=GerRank
											  ,gerTypeID=GerTypeID}=Ger,
								   [[GerID,RoleID,GerTypeID,GerLevel,GerExp,GerRank,GerPos]|Acc]
						   end, ArgList1, GerList),
	ArgList3 = lists:foldl(fun(Ger, Acc) ->
								   #gerSimple{gerID=GerID
                                              ,gerExp=GerExp
                                              ,gerLevel=GerLevel
                                              ,gerPos=GerPos
                                              ,gerQuality=GerRank
                                              ,gerTypeID=GerTypeID}=Ger,
								   [[GerID,RoleID,GerTypeID, GerLevel,GerExp,GerRank,GerPos + ?guardGerPos]|Acc]
						   end, ArgList2, GuardPosList),
    DeleteSql = io_lib:format("delete from gGer where roleID=~w;", [RoleID]),
    if ArgList3 == [] ->
           sql_execute_with_log(DeleteSql),
           {ok, 0};
       true ->
           InsertSql = make_sql_batch("insert into gGer values","(~w,~w,~w,~w,~w,~w,~w)",ArgList3),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.

get_role_guard_info(RoleID)->
	Sql = io_lib:format("select * from gGuardInfo where roleID = ~w;",[RoleID]),
	case get_row(Sql) of
		[RoleID, Count, Bin]->
            #role_guard{count=Count, infoList=uncompress_decode(Bin,[])};
		_ ->
			#role_guard{}
	end.

set_role_guard_info(RoleID,#role_guard{count=Count, infoList=Info})->
	Bin = compress_encode(Info),
	Sql = io_lib:format("replace into gGuardInfo values (~w,~w,~s);",[RoleID, Count, quote(Bin)]),
	{ok,_} = sql_execute_with_log(Sql).

get_fighterList(RoleID) ->
	Sql = io_lib:format("select fighterList from gFighterList where roleID=~w",[RoleID]),
	case get_row(Sql) of
		[FighterListBin] ->
			FighterList1 = to_term(FighterListBin);
		_ ->
			FighterList1 = []
	end,
	FighterList1.

get_fighterList_and_lieu_add_info(RoleID)->
	Sql = io_lib:format("select fighterList, lieuInfoList,lieuAtkAdd, lieuHpAdd from gFighterList where roleID = ~w;",[RoleID]),
	case get_row(Sql) of
		[FighterListBin, LieuInfoListBin, AtkAdd, HpAdd]->
			{to_term(FighterListBin), AtkAdd, HpAdd,to_term(LieuInfoListBin)};
		_ ->
			{[],0,0,[]}
	end.

set_fighterList(RoleID,FighterList,LieuInfoList, AtkAdd, HpAdd) ->
	Sql = io_lib:format("replace into gFighterList values (~w,~s,~s,~w,~w)",[RoleID,to_bin(FighterList), to_bin(LieuInfoList), AtkAdd, HpAdd]),
	sql_execute_with_log(Sql).

get_fighterList_and_lieu_add(RoleID)->
	Sql = io_lib:format("select fighterList, lieuAtkAdd, lieuHpAdd from gFighterList where roleID=~w;", [RoleID]),
	case get_row(Sql) of
		[FighterListBin, AtkAdd, HpAdd] ->
			{to_term(FighterListBin), {AtkAdd, HpAdd}};
		_ ->
			{[],{0,0}}
	end.

set_role_gag_list(RoleID, GagList)->
	Sql = io_lib:format("replace into gTalk values (~w, ~s);",[RoleID, to_bin(GagList)]),
	sql_execute_with_log(Sql).

get_role_gag_list(RoleID) ->
	Sql = io_lib:format("select gag_list from gTalk where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[GagListBin]->
			GagList = to_term(GagListBin);
		_ ->
			GagList = []
	end,
	GagList.

get_sign_emperor_info(RoleID) ->
	Sql = io_lib:format("select lastSignDate,signedDays,isEmperor, isGetBox from gOtherRecord where roleID = ~w;", [RoleID]),
	case get_row(Sql) of
		[{date,LastSignDate}, SignedDays, IsEmperor, IsGetBox] ->
			{LastSignDate, SignedDays, IsEmperor, IsGetBox};
		_ ->
			{{1989,7,17},0,0,0}
	end.

set_sign_emperor_info(RoleID, {LastSignedDate, SignedDays, IsEmperor, IsGetBox}) ->
	Sql = io_lib:format("replace into gOtherRecord values (~w, '~s', ~w, ~w, ~w);",[RoleID, date(LastSignedDate), SignedDays, IsEmperor, IsGetBox] ),
	sql_execute_with_log(Sql).

log_emperor_roleID(RoleID) ->
	Sql = io_lib:format("insert into log_emperor_history values (null, ~w);", [RoleID]),
	sql_execute_with_log(Sql).
	
get_activityInfo(RoleID) ->
	Sql = io_lib:format("select actID,value,list from gActivity where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = ActList ->
			ActList2 = 
			[#act{actID=ActID,list=to_term(List),value=to_term(Value,0)}
			|| [ActID,Value,List]<-ActList],
			 #dc{actList=ActList2,roleID=RoleID};
		_ ->
			#dc{actList=[],roleID=RoleID}
	end.

set_activityInfo(RoleID, DC) ->
    DeleteSql = io_lib:format("delete from gActivity where roleID=~w;",[RoleID]),
    ArgList = [[RoleID,ActID,to_bin(Value),to_bin(List)]
               || #act{actID=ActID,list=List,value=Value} <- DC#dc.actList],
    
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql),
           {ok,0};
       true ->
           InsertSql = make_sql_batch("insert into gActivity values","(~w,~w,~s,~s)",ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.


get_friendList(RoleID) ->
	Sql = io_lib:format("select type,friendID from gFriend where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_]=List->
			Pal = [E||[1,E]<-List],
			Foe = [E||[2,E]<-List],
			#d_friend{foe=Foe,pal=Pal,roleID=RoleID};
		_ ->
			#d_friend{foe=[],pal=[],roleID=RoleID}
	end.

add_friend(RoleID, 1=Type, FriendID) ->
	Sql = io_lib:format("insert into gFriend values(~w,~w,~w),(~w,~w,~w);",[RoleID,Type,FriendID,FriendID,Type,RoleID]),
	sql_execute_with_log(Sql);
add_friend(RoleID, 2=Type, FriendID) ->
	Sql = io_lib:format("insert into gFriend values(~w,~w,~w);",[RoleID,Type,FriendID]),
	sql_execute_with_log(Sql).

del_friend(RoleID, 1=Type, FriendID) ->
	Sql = io_lib:format("delete from gFriend where roleID=~w and type=~w and friendID=~w;",[RoleID,Type,FriendID]),
	sql_execute_with_log(Sql),
	Sql2 = io_lib:format("delete from gFriend where roleID=~w and type=~w and friendID=~w;",[FriendID,Type,RoleID]),
	sql_execute_with_log(Sql2);	
del_friend(RoleID, 2=Type, FriendID) ->
	Sql = io_lib:format("delete from gFriend where roleID=~w and type=~w and friendID=~w;",[RoleID,Type,FriendID]),
	sql_execute_with_log(Sql).
					 
get_pushInfo(RoleID) ->
	Sql = io_lib:format("select * from gPush where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[_,Token,IsPVPPushOpen,IsPushNightMute]->
			#d_push{isPVPPushOpen=int2bool(IsPVPPushOpen),
					isPushNightMute=int2bool(IsPushNightMute),
					roleID=RoleID,
					token=Token};
		_ ->
			?undefined
			end.

set_pushInfo(RoleID,PushInfo) ->
	#d_push{isPVPPushOpen=IsPVPPushOpen,
			isPushNightMute=IsPushNightMute,
			token=Token} = PushInfo,		
	Sql = io_lib:format("replace into gPush values (~w,~s,~w,~w);",
						[RoleID,quote(Token),bool2int(IsPVPPushOpen),bool2int(IsPushNightMute)]),
	sql_execute_with_log(Sql).
	
				   
	
get_inviteInfo(RoleID) ->
	RewardNumSql = io_lib:format("select rewardNum from gInvite where roleID=~w;",[RoleID]),
	case get_row(RewardNumSql) of
		[RewardNum] ->
			next;
		_ ->
			RewardNum=0
	end,
	InviteListSql = io_lib:format("select inviteRoleID from gInviteRoleList where roleID=~w;",[RoleID]),
	case get_all(InviteListSql) of
		[_|_]=List ->
			InviteList = [E||[E]<-List];
		_ ->
			InviteList = []
	end,
	#d_invite{inviteRoleIDList=InviteList,rewardNum=RewardNum,roleID=RoleID}.

set_inviteRewardNum(RoleID,RewardNum) ->
	Sql = io_lib:format("replace into gInvite values(~w,~w);",[RoleID,RewardNum]),
	sql_execute_with_log(Sql).

get_inviteRewardNum(InviterRoleID)->
	Sql = io_lib:format("select rewardNum from gInvite where roleID = ~w;", [InviterRoleID]),
	case get_row(Sql) of
		[Num] ->
			Num;
		_ ->
			0
	end.

add_inviteRoleID(RoleID, InviteRoleID) ->
	Sql = io_lib:format("insert into gInviteRoleList values (~w,~w);",[RoleID, InviteRoleID]),
	sql_execute_with_log(Sql).
	
get_guideState(RoleID)->
	Sql = io_lib:format("select guideState from gGuide where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[GuideState] ->
			GuideState;
		_ ->
			0
	end.

set_guideState(RoleID,GuideState) ->
	Sql = io_lib:format("replace into gGuide values(~w,~w);",[RoleID,GuideState]),
	sql_execute_with_log(Sql).

get_etc(Key) ->
	Sql = io_lib:format("select value from gETC where `key`=~w;", [Key]),
	case get_row(Sql) of
		[Bin] ->
			uncompress_decode(Bin,[]);
		_ ->
			[]
	end.

set_etc(Key, Info) ->
	Bin = compress_encode(Info),
	%?DEBUG("set etc,size=~w,key=~w",[byte_size(Bin), Key]),
	Sql = io_lib:format("replace into gETC values (~w,~s);",[Key, quote(Bin)]),
	{ok,_} = sql_execute_with_log(Sql).

del_etc(Key) ->
	Sql = io_lib:format("delete from gETC where `key` = ~w;",[Key]),
	sql_execute_with_log(Sql).
		
get_fightReplay(ReplayUID, ReplayType) ->
    Sql = io_lib:format("select replay from ~s where replayUID=~w;", [tk_id:get_replay_table_name(ReplayType), ReplayUID]),
    case get_row(Sql) of
        [Bin] ->
            uncompress_decode(Bin);
        _ ->
            []
    end.

set_fightReplay(ReplayUID, Info, RepType) ->
	Sql = io_lib:format("insert into ~s values (~w,~s,'~s');",[tk_id:get_replay_table_name(RepType), ReplayUID, quote(compress_encode(Info)), datetime(erlang:localtime())]),
	sql_execute_with_log(Sql).

set_fightReplayList(List, RepType) ->
	Time = datetime(erlang:localtime()),
	ArgList = [ [ReplayUID, quote(compress_encode(Info)), Time] || [ReplayUID, Info] <- List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [tk_id:get_replay_table_name(RepType)]),
                            "(~w,~s,'~s')",ArgList, 100).

del_fightReplay(ReplayUID, RepType) ->
    Sql = io_lib:format("delete from ~s where replayUID = ~w;",[tk_id:get_replay_table_name(RepType), ReplayUID]),
    sql_execute_with_log(Sql).

del_fightReplayList([], _RepType) ->
	{ok,0};
del_fightReplayList(List, RepType) ->
	Sql = io_lib:format("delete from ~s where replayUID in (~s);",[tk_id:get_replay_table_name(RepType), List]),
	sql_execute_with_log(Sql).

del_spec_type_replay(RepType) ->
    Sql = io_lib:format("truncate table ~s;",[tk_id:get_replay_table_name(RepType)]),
    sql_execute_with_log(Sql).
	
add_role_gift_drawed_type(RoleID, Type) ->
	Sql = io_lib:format("insert into gGift values (~w,~s)",[RoleID, quote(Type)]),
	sql_execute_with_log(Sql).

check_role_gift_drawed_type(RoleID, Type) ->
	Sql = io_lib:format("select count(*) from gGift where roleID=~w and `type`=~s;",[RoleID, quote(Type)]),
	case get_row(Sql) of
		[A] when is_integer(A) ->
			A >= 1;
		_ ->
			false
	end.

sql_find_mail(MailUID)->
	Sql = io_lib:format("select * from gMail where mailUID = ~w;",[MailUID]),
	case get_row(Sql) of
		[MailUID,RecvID,MailType,SenderID,_SenderName,_Content,_Time,MailTemplateID,_ParamList,MailReward,_IsRead]->
			[RecvID,MailTemplateID,db_sql:to_term(MailReward),MailType,SenderID];
		_ ->
			[0,0,0,0,0]
	end.

sql_find_spec_mailUID_list(RecvID, SenderID, MailType) ->
    Sql = io_lib:format("select mailUID from gMail where recvID = ~w and senderID = ~w and mailType = ~w", [RecvID, SenderID, MailType]),
    case get_all(Sql) of
        List when erlang:is_list(List) ->
            lists:flatten(List);
        _ ->
            []
    end.

get_unread_num(RoleID,Type)->
	case Type of
		0 ->
			Sql = io_lib:format("select count(*) from gMail where recvID = ~w and isRead = false;", [RoleID]);
		2 ->
			Sql = io_lib:format("select count(*) from gMail where recvID = ~w and mailType = 5 and isRead = false or  recvID = ~w and mailType = 3 and isRead = false;", [RoleID, RoleID]);
		_ ->
			Sql = io_lib:format("select count(*) from gMail where recvID = ~w and mailType <> 5 and mailType <> 3 and isRead = false;", [RoleID])
	end,
	case get_row(Sql) of
		[A] when is_integer(A) ->
			A;
		{error, {_, Reason2}} ->
			?ERR("sql ****~s*****execute with err:~s",[Sql,Reason2]),
			0
	end.

get_last_mails_by_type(RoleID, Type, ClientTopMailUID) ->
	case Type of
		2 ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType = 5 and mailUID > ~w or recvID = ~w and mailType = 3 and mailUID > ~w order by mailUID desc limit ~w;",[RoleID, ClientTopMailUID, RoleID, ClientTopMailUID, ?GET_SQL_MAIL_NUM]);
		_ ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType <> 5 and mailType <> 3 and mailUID > ~w order by mailUID desc limit ~w;",[RoleID, ClientTopMailUID, ?GET_SQL_MAIL_NUM])
	end,
	case get_all(Sql) of
		[_|_] = List ->
			A = 
				lists:foldr(fun([MailUID,_,MailType,SenderID,SenderName,Content,Time,MailTemplateID,ParamList,MailReward,IsRead],
							Acc) ->
								IsRead2 = db_sql:int2bool(IsRead),
								Mail = #mail{
									mailUID = MailUID,
									mailType = MailType,
									senderID = SenderID,
									senderName = SenderName,
									content = Content,
									time = Time,
									mailTemplateID = MailTemplateID,
									paramList = db_sql:to_term(ParamList),
									mailReward = db_sql:to_term(MailReward,[]),
									isRead = IsRead2},
									[Mail|Acc]
							end, [],List),
			#d_mail{mail = [A], roleID = RoleID};
		_ ->
			#d_mail{mail = [[]], roleID = RoleID}
	end.

sql_get_more_mail(RoleID, Type, StartMailUID)->
	case Type of
		2 ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType = 5 and mailUID < ~w or recvID = ~w and mailType = 3 and mailUID < ~w order by mailUID desc limit ~w;",[RoleID, StartMailUID, RoleID, StartMailUID, ?GET_SQL_MAIL_NUM]);
		_ ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType <> 5 and mailType <> 3 and mailUID < ~w order by mailUID desc limit ~w;",[RoleID, StartMailUID, ?GET_SQL_MAIL_NUM])
	end,
	case get_all(Sql) of
		[_|_] = List ->
			A = 
				lists:foldl(fun([MailUID,_,MailType,SenderID,SenderName,Content,Time,MailTemplateID,ParamList,MailReward,IsRead],
							Acc) ->
								IsRead2 = db_sql:int2bool(IsRead),
								Mail = #mail{
									mailUID = MailUID,
									mailType = MailType,
									senderID = SenderID,
									senderName = SenderName,
									content = Content,
									time = Time,
									mailTemplateID = MailTemplateID,
									paramList = db_sql:to_term(ParamList),
									mailReward = db_sql:to_term(MailReward,[]),
									isRead = IsRead2},
									[Mail|Acc]
							end, [],List),
			lists:reverse(A);
		_ ->
			[]
	end.

get_mailList(RoleID) ->
	Sql = io_lib:format("select * from gMail where recvID=~w order by mailUID;",[RoleID]),
	case get_all(Sql) of
		[_|_] = List ->
			{A,B,C, UA,UB,UC} = 
			lists:foldl(fun([MailUID,_,MailType,SenderID,SenderName,Content,Time,MailTemplateID,ParamList,MailReward,IsRead],
							{SysAcc, PrivAcc, CollAcc, Un1, Un2, Un3}) ->
								IsRead2 = int2bool(IsRead),
								Mail = #mail{
											mailUID=MailUID
											,mailType=MailType
											,senderID=SenderID
											,senderName=SenderName
											,content=Content
											,time=Time
											,mailTemplateID=MailTemplateID
											,paramList=to_term(ParamList)
											,mailReward=to_term(MailReward,[])
											,isRead=IsRead2
											 },
								if MailType == ?MAIL_TYPE_PRIVATE ->
									   if IsRead2 ->
									   {SysAcc, [Mail|PrivAcc], CollAcc, Un1, Un2, Un3};
										  true ->
									   {SysAcc, [Mail|PrivAcc], CollAcc, Un1, Un2+1, Un3}
									   end;
								   MailType == ?MAIL_TYPE_UNION ->
									   if IsRead2 ->
									   {SysAcc, PrivAcc, [Mail|CollAcc], Un1, Un2, Un3};
										  true ->
									   {SysAcc, PrivAcc, [Mail|CollAcc], Un1, Un2, Un3+1}
									   end;
								   true ->
									   if IsRead2 ->
									   {[Mail|SysAcc], PrivAcc, CollAcc, Un1, Un2, Un3};
										  true ->
									   {[Mail|SysAcc], PrivAcc, CollAcc, Un1+1, Un2, Un3}
									   end
								end
						end, {[], [], [], 0, 0, 0}, List), 
			#d_mail{mail=[A,B,C],roleID=RoleID,unreadNum=[UA,UB,UC]};
		_ ->
			#d_mail{mail=[[],[],[]],roleID=RoleID,unreadNum=[0,0,0]}
	end.
del_mail([]) ->
	{ok,0};
del_mail(MailUIDList) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("delete from gMail where mailUID in (~s);",[Arg]),
	sql_execute_with_log(Sql).

read_mail([]) ->
	{ok,0};
read_mail(MailUIDList) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("update gMail set isRead=1 where mailUID in (~s);",[Arg]),
	sql_execute_with_log(Sql).


add_mailList([]) ->
	{ok,0};
add_mailList(List) ->
	ArgList = 
		[begin
			 #mail{
				   mailUID=MailUID
				   ,mailType=MailType
				   ,senderID=SenderID
				   ,senderName=SenderName
				   ,content=Content
				   ,time=Time
				   ,mailTemplateID=MailTemplateID
				   ,paramList=ParamList
				   ,mailReward=MailReward
				   ,isRead=IsRead
				  }= Mail,
			 [MailUID,RoleID,MailType,SenderID,quote(SenderName),quote(Content),Time,MailTemplateID,to_bin(ParamList),to_bin(MailReward),bool2int(IsRead)]
		 end || {RoleID, Mail} <-List],
    make_sql_batch_by_piece("insert into gMail values", "(~w,~w,~w,~w,~s,~s,~w,~w,~s,~s,~w)", ArgList, 1000).


get_histList(RoleID,Type) ->
	Sql = io_lib:format("select histUID,histType,name,enemyID,time,arg,isRead,addRepu from gHist where roleID=~w and type=~w order by histUID;",[RoleID, Type]),
	case get_all(Sql) of
		[_|_] = List ->
			{A,B} = 
				lists:foldl(fun([VhistUID,VhistType,Vname,VenemyID,Vtime,Varg,VisRead,VaddRepu]
								,{Acc, UN})  ->
									IsRead = int2bool(VisRead),
									Hist = #hist{
												 histUID     = VhistUID    
												 ,histType   = VhistType  
												 ,name       = Vname      
												 ,enemyID    = VenemyID   
												 ,time       = Vtime      
												 ,arg        = Varg       
												 ,isRead     = IsRead
                                                 ,addRepu    = VaddRepu
												},
									if IsRead ->
										   {[Hist|Acc], UN};
									   true ->
										   {[Hist|Acc], UN+1}
									end					
							end, {[],0}, List),
			#d_hist{histList=A,unreadNum=B};
		_ ->
			#d_hist{histList=[],unreadNum=0}
	end.

del_hist([], _Type) ->
	{ok,0};
del_hist(MailUIDList, Type) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("delete from gHist where histUID in (~s);delete from ~s where replayUID in (~s);",
                        [Arg,tk_id:get_replay_table_name(to_rep_type(Type)),Arg]),
	sql_execute_sqls(Sql).

del_spec_type_hist(Type) ->
    case Type of
        ?TYPE_PVP ->
            Sql = io_lib:format("delete from gHist where (histType mod 32) div 16 = 1; truncate table ~s;",
                                [tk_id:get_replay_table_name(to_rep_type(Type))]);
        ?TYPE_RULE ->
            Sql = io_lib:format("delete from gHist where (histType mod 32) div 16 = 0; truncate table ~s;",
                                [tk_id:get_replay_table_name(to_rep_type(Type))])
    end,
    sql_execute_sqls(Sql).


to_rep_type(Type) ->
	case Type of
		?TYPE_PVP ->
			?REPLAY_TYPE_PVP;
		?TYPE_RULE ->
			?REPLAY_TYPE_RULE
	end.

read_hist([]) ->
	{ok,0};
read_hist(MailUIDList) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("update gHist set isRead=1 where histUID in (~s);",[Arg]),
	sql_execute_with_log(Sql).


add_histList([], _Type) ->
	{ok,0};
add_histList(List, Type) ->
	ArgList = 
		[begin
			 #hist{
									histUID     = VhistUID    
									,histType   = VhistType  
									,name       = Vname      
									,enemyID    = VenemyID   
									,time       = Vtime      
									,arg        = Varg       
									,isRead     = IsRead
                                    ,addRepu    = AddRepu
				  }= Hist,
			 [VhistUID,VhistType,quote(Vname),VenemyID,Vtime,Varg,bool2int(IsRead), RoleID, Type, AddRepu]
		 end || {RoleID, Hist, _FightInfo} <-List],
	%?ERR("list=~w",[List]),
	Sql = make_sql_batch("insert into gHist values", "(~w,~w,~s,~w,~w,~w,~w,'',~w,~w,~w)",ArgList),
	sql_execute_with_log(Sql),
	ReplayList = [ [HistUID,FightInfo] || {_RoleID, #hist{histUID=HistUID}, FightInfo} <-List ],
	set_fightReplayList(ReplayList, to_rep_type(Type)).


get_hist_fightInfo(HistUID, Type) ->
	get_fightReplay(HistUID, to_rep_type(Type)).
	
check_auto_mail(ConfigID) ->
    Sql = io_lib:format("select count(*) from gAutoMail where configID=~w;",[ConfigID]),
    case get_row(Sql) of
        [N] when N > 0 ->
            true;
        _ ->
            false
    end.

add_auto_mail(ConfigID) ->
    Sql = io_lib:format("insert into gAutoMail values (~w);",[ConfigID]),
    sql_execute_with_log(Sql).

check_account_ban(AccountID) ->
	Sql = io_lib:format("select count(*) from gBanAccount where accountID=~w;",[AccountID]),
	case get_row(Sql) of
		[N] when N > 0 ->
			true;
		_ ->
			false
	end.

add_account_ban(AccountID) ->
	Sql = io_lib:format("insert into gBanAccount values (~w);",[AccountID]),
	sql_execute_with_log(Sql).

del_account_ban(AccountIDList) ->
	Arg = string:join([integer_to_list(E)||E<-AccountIDList], ","),
	Sql = io_lib:format("delete from gBanAccount where accountID in (~s);",[Arg]),
	sql_execute_with_log(Sql).

%% 礼包数据---------------------------------------------------------------------------
get_reward_data(RoleID) ->
    Sql = io_lib:format("select onlineSecs, days, lastDays, getList from gReward where roleID=~w",[RoleID]),
    case get_row(Sql) of
        [OnlineSecs, Days, LastDays, GetListBin] ->
            #role_reward_info{onlineSecs = OnlineSecs, days = Days, lastDays = LastDays, getList = to_term(GetListBin)};
        _ ->
            #role_reward_info{onlineSecs = 0, days = 1,lastDays = calendar:date_to_gregorian_days(erlang:date()) - 1, getList = []}
    end.

set_reward_data(RoleID, #role_reward_info{onlineSecs = OnlineSecs, days = Days, lastDays = LastDays, getList = GetList}) ->
    Sql = io_lib:format("replace into gReward values (~w,~w,~w,~w,~s)",[RoleID,OnlineSecs,Days,LastDays,to_bin(GetList)]),
    sql_execute_with_log(Sql).

%% --------------------------------------------------------------------------------

get_team_pk_data(RoleID) ->
    Sql = io_lib:format("select teamPkData from gTeamPk where roleID=~w",[RoleID]),
    case get_row(Sql) of
        [Bin] ->
            TeamPkData = to_term(Bin);
        _ ->
            TeamPkData = ?undefined
    end,
    TeamPkData.

set_team_pk_data(_RoleID,?undefined) ->
    ok;
set_team_pk_data(RoleID,TeamPkData) ->
    Sql = io_lib:format("replace into gTeamPk values (~w,~s)",[RoleID,to_bin(TeamPkData)]),
    sql_execute_with_log(Sql).

get_road_data(RoleID) ->
    Sql = io_lib:format("select timestamp,nowID,extID,status,resetTimes from gRoad where roleID=~w",[RoleID]),
    case get_row(Sql) of
        [Timestamp, NowID, ExtID, Status, ResetTimes] ->
            #role_road{timestamp=Timestamp, nowID=NowID, extID=ExtID, status=Status, resetTimes=ResetTimes};
        _ ->
            ?undefined
    end.

set_road_data(_RoleID, ?undefined) ->
    ok;
set_road_data(RoleID, #role_road{timestamp=Timestamp, nowID=NowID, extID=ExtID, status=Status, resetTimes=ResetTimes}) ->
    Sql = io_lib:format("replace into gRoad values (~w, ~w, ~w, ~w, ~w, ~w)", [RoleID, Timestamp, NowID, ExtID, Status, ResetTimes]),
    sql_execute_with_log(Sql).

get_alien_data(RoleID) ->
    Sql = io_lib:format("select alienTimes,lastRecoverTime,resetTime from gAlien where roleID=~w",[RoleID]),
    case get_row(Sql) of
        [AlienTiems,LastRecoverTime,ResetTime] ->
            AlienData = #alien_info{times=AlienTiems, lastRecoverTime=LastRecoverTime,resetTime=ResetTime};
        _ ->
            AlienData = ?undefined
    end,
    AlienData.

set_alien_data(_RoleID,?undefined) ->
    ok;
set_alien_data(RoleID,#alien_info{times=AlienTiems, lastRecoverTime=LastRecoverTime, resetTime=ResetTime}) ->
    Sql = io_lib:format("replace into gAlien values (~w,~w,~w,~w)",[RoleID,AlienTiems,LastRecoverTime,ResetTime]),
    sql_execute_with_log(Sql).

get_task(RoleID)->
	TaskSql = io_lib:format("select * from gTask where roleID = ~w",[RoleID]),
	List = get_all(TaskSql),
	{RList1,L} = lists:foldl(fun([_,TaskID,Status,Num,ArgsBin],{Acc,IDS}=CC)->
					  case data_task:get(TaskID) of
						  ?undefined->
							  CC;
						  #data_task{trigger_id=TriggerID}->
					 		 {[#r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=to_term(ArgsBin),trigger_id=TriggerID}|Acc],[TaskID|IDS]}
					  end
			  end, {[],[]},List),
	ListAll = data_task:get_list(),
	AddList = ListAll -- L,
	lists:foldl(fun(ID,Acc)->
						#data_task{task_type=TaskType,trigger_id=TriggerID} = data_task:get(ID),
							case TaskType of
								?TASK_TYPE_MAIN->
									Acc;
								_->
									[#r_task{task_id=ID,status=?TASK_STATUS_WAS_ACCEPT,trigger_num=0,trigger_notes=[],trigger_id=TriggerID}|Acc]
							end
				end, RList1, AddList).

is_have_task_data(RoleID) ->
    Sql = io_lib:format("select count(*) from gTask where roleID = ~w;",[RoleID]),
    case get_row(Sql) of
        [0] ->
            false;
        _ ->
            true
    end.

set_task(RoleID,RoleTaskList)->
	DBRoleTaskList =
		lists:map(fun(#r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=TriggerArgs})->
							[RoleID,TaskID,Status,Num,to_bin(TriggerArgs)]
					end, RoleTaskList),
	DeleteTaskSql = io_lib:format("delete from gTask where roleID=~w;", [RoleID]),
    if DBRoleTaskList == [] ->
           sql_execute_with_log(DeleteTaskSql);
       true ->
           InsertSql1 = make_sql_batch("insert into gTask values","(~w,~w,~w,~w,~s)",DBRoleTaskList),
           sql_execute_with_log2(DeleteTaskSql ++ InsertSql1)
    end.

set_task2(RoleID,ChangeTaskList) ->
    case ChangeTaskList of
        [] ->
            next;
        _ ->
            DBRoleTaskList =
                lists:map(fun(#r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=TriggerArgs})->
                                  [RoleID,TaskID,Status,Num,to_bin(TriggerArgs)]
                          end, ChangeTaskList),
            InsertSql = make_sql_batch("replace into gTask values","(~w,~w,~w,~w,~s)",DBRoleTaskList),
            sql_execute_with_log(InsertSql)
    end.

get_task_by_id(RoleID, TaskID) ->
    TaskSql = io_lib:format("select * from gTask where roleID = ~w and taskID = ~w",[RoleID, TaskID]),
    case get_row(TaskSql) of
        [RoleID,TaskID,Status,Num,ArgsBin] ->
            case data_task:get(TaskID) of
                ?undefined->
                    ?undefined;
                #data_task{trigger_id=TriggerID}->
                    #r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=to_term(ArgsBin),trigger_id=TriggerID}
            end;
        [] ->
            #data_task{task_type=TaskType,trigger_id=TriggerID} = data_task:get(TaskID),
            case TaskType of
                ?TASK_TYPE_MAIN->
                    ?undefined;
                _ ->
                    #r_task{task_id=TaskID,status=?TASK_STATUS_WAS_ACCEPT,trigger_num=0,trigger_notes=[],trigger_id=TriggerID}
            end;
        ErrData ->
            ?ERR("RoleID:~w, TaskID:~w, ErrData:~w", [RoleID, TaskID, ErrData]),
            ?undefined
    end.

set_task_by_id(RoleID, #r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=TriggerArgs}) ->
    Sql = io_lib:format("replace into gTask values (~w,~w,~w,~w,~s)", [RoleID,TaskID,Status,Num,to_bin(TriggerArgs)]),
    sql_execute_with_log(Sql);

set_task_by_id(RoleID, Data) ->
    ?ERR("set_task_by_id error,RoleID:~w, Data:~w", [RoleID, Data]).
			

get_homestead(RoleID)->
	Sql = io_lib:format("select * from gHomestead where roleID = ~w", [RoleID]),
	case get_all(Sql) of
		[[_,RoleName,EneayTimes,MatingTimes,MatingCoolSecond,Add4Mating,GerID,GerTypeID,Quality,Level,RefreshMatingSecond,MachineListBin,LogListBin]]->
			HomeSteadInfo = #p_homestead{roleName=RoleName,energyTimes=EneayTimes,matingTimes=MatingTimes,
						 matingCoolSecond=MatingCoolSecond,add4mating=Add4Mating,gerID=GerID,gerTypeID=GerTypeID,quality=Quality,level=Level,refreshMatingSecond=RefreshMatingSecond},
			MachineList = to_term(MachineListBin),
			LogList = to_term(LogListBin),
			{HomeSteadInfo,MachineList,LogList};
		_->
			?undefined
	end.
set_homestead(RoleID,{HomeSteadInfo,MachieList,LogList})->
	#p_homestead{roleName=RoleName,energyTimes=AddEneayTimes,matingTimes=MatingTimes,
						 matingCoolSecond=MatingCoolSecond,add4mating=Add4Mating,gerID=GerID,gerTypeID=GerTypeID,quality=Quality,level=Level,refreshMatingSecond=RefreshMatingSecond} = HomeSteadInfo,
	MachineListBin = to_bin(MachieList),
	LogListBin = to_bin(LogList),
	Sql = io_lib:format("replace into gHomestead values(~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~s) ", [RoleID,quote(RoleName),AddEneayTimes,MatingTimes,MatingCoolSecond,Add4Mating,GerID,GerTypeID,Quality,Level,RefreshMatingSecond,MachineListBin,LogListBin]),
	sql_execute_with_log(Sql);
set_homestead(_RoleID,?undefined)->
	ignore.

get_enargy_friend(RoleID)->
	Sql = io_lib:format("select * from gFriendEnargy where roleID = ~w", [RoleID]),
	case get_all(Sql) of
		[[_,{date,Date},GiveTimes]]->
            ToFriendList = get_role_to_friend_list(RoleID),
            ToMeList = get_role_to_me_list(RoleID),
            AddFriendList = get_role_add_friend_list(RoleID),
            BeAddFriendList = get_role_be_add_friend_list(RoleID),
            Sql2 = io_lib:format("select * from gFriendFightTimes where roleID = ~w", [RoleID]),
            case get_all(Sql2) of
                [[_,{date,RefreshFightDate},FightTimes]]->
                    next;
                _ ->
                    RefreshFightDate = erlang:date(),
                    FightTimes = data_common:get(friend_fight_times)
            end,
            FightList = get_role_friend_fight_list(RoleID),
			#friend_enargy{roleID=RoleID,toFriendList=ToFriendList,toMeList=ToMeList,addFriendList=AddFriendList,
                           beAddFriendList=BeAddFriendList,date=Date,giveTimes=GiveTimes,
                           fightTimes=FightTimes,fightList=FightList,refreshFightDate=RefreshFightDate};
		_->
			#friend_enargy{roleID=RoleID,toFriendList=[],toMeList=[],addFriendList=[],beAddFriendList=[],date=erlang:date(),giveTimes=20,
                           fightTimes=data_common:get(friend_fight_times),fightList=[],refreshFightDate=erlang:date()}
	end.

batch_set_giveTimes(ArgList) ->
    make_sql_batch_by_piece("replace into gFriendEnargy values", "(~w,'~s',~w)", ArgList, 1000).

batch_set_friendFightTimes(ArgList) ->
    make_sql_batch_by_piece("replace into gFriendFightTimes values", "(~w,'~s',~w)", ArgList, 1000).

batch_set_toFriendList({RoleIDList, ArgList}) ->
    make_sql_batch_del_by_piece("delete from gToFriend where roleID in", RoleIDList, 1000),
    make_sql_batch_by_piece("insert into gToFriend values", "(~w,~w,~w,~w)", ArgList, 1000).

batch_set_toMeList({RoleIDList, ArgList}) ->
    make_sql_batch_del_by_piece("delete from gToMe where roleID in", RoleIDList, 1000),
    make_sql_batch_by_piece("insert into gToMe values", "(~w,~w,~w)", ArgList, 1000).

batch_set_addFriendList({RoleIDList, ArgList}) ->
    make_sql_batch_del_by_piece("delete from gAddFriend where roleID in", RoleIDList, 1000),
    make_sql_batch_by_piece("insert into gAddFriend values", "(~w,~w,~w)", ArgList, 1000).

batch_set_beAddFriendList({RoleIDList, ArgList}) ->
    make_sql_batch_del_by_piece("delete from gBeAddFriend where roleID in", RoleIDList, 1000),
    make_sql_batch_by_piece("insert into gBeAddFriend values", "(~w,~w,~w)", ArgList, 1000).

batch_set_friendFightList({RoleIDList, ArgList}) ->
    make_sql_batch_del_by_piece("delete from gFriendFight where roleID in", RoleIDList, 1000),
    make_sql_batch_by_piece("insert into gFriendFight values", "(~w,~w,~w)", ArgList, 1000).

get_role_to_friend_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp,isGive from gToFriend where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp,IsGive}||[FriendID,Timestamp,IsGive]<-List];
        _ ->
            []
    end.

get_role_friend_fight_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp from gFriendFight where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp}||[FriendID,Timestamp]<-List];
        _ ->
            []
    end.


get_role_to_me_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp from gToMe where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp}||[FriendID,Timestamp]<-List];
        _ ->
            []
    end.

get_role_add_friend_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp from gAddFriend where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp}||[FriendID,Timestamp]<-List];
        _ ->
            []
    end.

get_role_be_add_friend_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp from gBeAddFriend where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp}||[FriendID,Timestamp]<-List];
        _ ->
            []
    end.

get_shop_treasure(RoleID) ->
    Sql = io_lib:format("select nextRefreshTime, itemList from gShopTreasure where roleID = ~w", [RoleID]),
    case get_row(Sql) of
        [NextRefreshTime, ItemList] ->
            #shop_treasure{nextRefreshTime = NextRefreshTime, itemList = to_term(ItemList)};
        _ ->
            #shop_treasure{}
    end.

set_shop_treasure(RoleID, #shop_treasure{nextRefreshTime=NextRefreshTime, itemList=ItemList}) ->
    Sql = io_lib:format("replace into gShopTreasure values(~w,~w,~s)", [RoleID, NextRefreshTime, to_bin(ItemList)]),
    sql_execute_with_log(Sql).

search_familyName(Name) ->
    Sql = io_lib:format("select familyID from gFamily where familyName=~s;",[quote(Name)]),
    case get_row(Sql) of
        [FamilyID] ->
            FamilyID;
        _ ->
            ?undefined
    end.

update_family_rank(FamilyID, Rank) ->
    sql_execute_with_log(io_lib:format("update gFamily set rank = ~w where familyID = ~w", [Rank, FamilyID])).

del_family_info(FamilyID) ->
    sql_execute_with_log(io_lib:format("delete from gFamily where familyID = ~w", [FamilyID])).

get_family_info(FamilyID) ->
    Sql = io_lib:format("select * from gFamily where FamilyID = ~w", [FamilyID]),
    case get_row(Sql) of
        [FamilyID,FamilyName,FamilyLevel,CreateRoleID,CreateRoleName,OwnerRoleID,OwnerRoleName,CurMembers,ActivePoints,Notice,Rank,CreateTime,TalkBin] ->
            Members = get_family_member_info(FamilyID),
            #family_info{
                         family_id=FamilyID
                         ,family_name=FamilyName
                         ,level=FamilyLevel
                         ,create_role_id=CreateRoleID
                         ,create_role_name=CreateRoleName
                         ,owner_role_id=OwnerRoleID
                         ,owner_role_name=OwnerRoleName
                         ,cur_members=CurMembers
                         ,active_points=ActivePoints
                         ,notice=Notice
                         ,members=Members
                         ,rank=Rank
                         ,create_time=CreateTime
                         ,talk_data=uncompress_decode(TalkBin,[])};
        _ ->
            ?undefined
    end.

set_family_info(FamilyInfo) ->
    #family_info{
                 family_id=FamilyID
                 ,family_name=FamilyName
                 ,level=FamilyLevel
                 ,create_role_id=CreateRoleID
                 ,create_role_name=CreateRoleName
                 ,owner_role_id=OwnerRoleID
                 ,owner_role_name=OwnerRoleName
                 ,cur_members=CurMembers
                 ,active_points=ActivePoints
                 ,notice=Notice
                 ,members=Members
                 ,rank=Rank
                 ,create_time=CreateTime
                 ,talk_data=TalkData}=FamilyInfo,
    sql_execute_with_log(io_lib:format("replace into gFamily values(~w,~s,~w,~w,~s,~w,~s,~w,~w,~s,~w,~w,~s)",
                                       [FamilyID,quote(FamilyName),FamilyLevel,CreateRoleID,quote(CreateRoleName),OwnerRoleID,quote(OwnerRoleName),
                                        CurMembers,ActivePoints,quote(Notice),Rank,CreateTime,quote(compress_encode(TalkData))])),
    lists:foreach(fun(Member) -> set_family_member_info(Member) end, Members).

get_family_member_info(FamilyID) ->
    case get_all(io_lib:format("select * from gFamilyMember where familyID = ~w", [FamilyID])) of
        List when erlang:is_list(List) ->
            [#family_member_info{
                                 role_id=RoleID,
                                 role_name=RoleName,
                                 family_id=FamilyID,
                                 family_contribution=FamilyCon,
                                 left_family_contribution=LeftFamilyCon,
                                 use_gold_time=UseGoldTime,
                                 title=Title,
                                 is_male=int2bool(IsMale),
                                 online=false,
                                 role_level=RoleLevel,
                                 fight_power=FightPower,
                                 family_title=FamilyTitle,
                                 join_time=JoinTime
                                }
                                ||[RoleID,RoleName,_FamilyID,FamilyCon,LeftFamilyCon,UseGoldTime,Title,IsMale,RoleLevel,FightPower,FamilyTitle,JoinTime]<-List];
        _ ->
            []
    end.

set_family_member_info(FamilyMemberInfo) ->
    #family_member_info{
                        role_id=RoleID,
                        role_name=RoleName,
                        family_id=FamilyID,
                        family_contribution=FamilyCon,
                        left_family_contribution=LeftFamilyCon,
                        use_gold_time=UseGoldTime,
                        title=Title,
                        is_male=IsMale,
                        role_level=RoleLevel,
                        fight_power=FightPower,
                        family_title=FamilyTitle,
                        join_time=JoinTime
                       }=FamilyMemberInfo,
    Sql = io_lib:format("replace into gFamilyMember values(~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w)", [RoleID,quote(RoleName),FamilyID,FamilyCon,LeftFamilyCon,UseGoldTime,Title,bool2int(IsMale),RoleLevel,FightPower,FamilyTitle,JoinTime]),
    sql_execute_with_log(Sql).

update_family_member_name(RoleID, NewName) ->
    Sql = io_lib:format("update gFamilyMember set roleName = ~s where roleID = ~w", [quote(NewName), RoleID]),
    sql_execute_with_log(Sql).

del_family_member(FamilyID, RoleID) ->
    Sql = io_lib:format("delete from gFamilyMember where familyID = ~w and roleID = ~w", [FamilyID, RoleID]),
    sql_execute_with_log(Sql).

get_all_family_info() ->
    case get_all("select * from gFamily") of
        List when erlang:is_list(List) ->
            [
             #family_info{
                          family_id=FamilyID
                          ,family_name=FamilyName
                          ,level=FamilyLevel
                          ,create_role_id=CreateRoleID
                          ,create_role_name=CreateRoleName
                          ,owner_role_id=OwnerRoleID
                          ,owner_role_name=OwnerRoleName
                          ,cur_members=CurMembers
                          ,active_points=ActivePoints
                          ,notice=Notice
                          ,members=get_family_member_info(FamilyID)
                          ,rank=Rank
                          ,create_time=CreateTime
                          ,talk_data=[]}
                         ||[FamilyID,FamilyName,FamilyLevel,CreateRoleID,CreateRoleName,OwnerRoleID,OwnerRoleName,CurMembers,ActivePoints,Notice,Rank,CreateTime,_TalkBin]<-List];
        _ ->
            []
    end.

add_role_family_request(FamilyRequest) ->
    #family_request{
                    role_id=RoleID
                    ,role_name=RoleName
                    ,level=RoleLevel
                    ,fight_power=FightPower
                    ,timestamp=Timestamp
                    ,family_id=FamilyID} = FamilyRequest,
    Sql = io_lib:format("replace into gFamilyRequest values(~w,~s,~w,~w,~w,~w)", [RoleID,quote(RoleName),RoleLevel,FightPower,Timestamp,FamilyID]),
    sql_execute_with_log(Sql).

del_role_family_request(RoleID) ->
    Sql = io_lib:format("delete from gFamilyRequest where roleID = ~w", [RoleID]),
    sql_execute_with_log(Sql).

del_family_request(FamilyID) ->
    Sql = io_lib:format("delete from gFamilyRequest where FamilyID = ~w", [FamilyID]),
    sql_execute_with_log(Sql).

del_spec_family_request(RoleID, FamilyID) ->
    Sql = io_lib:format("delete from gFamilyRequest where roleID = ~w and familyID = ~w", [RoleID, FamilyID]),
    sql_execute_with_log(Sql).

get_all_family_request() ->
    case get_all("select * from gFamilyRequest") of
        List when erlang:is_list(List) ->
            [
             #family_request{
                             role_id=RoleID
                             ,role_name=RoleName
                             ,level=RoleLevel
                             ,fight_power=FightPower
                             ,timestamp=Timestamp
                             ,family_id=FamilyID}
                            ||[RoleID,RoleName,RoleLevel,FightPower,Timestamp,FamilyID]<-List];
        _ ->
            []
    end.

%% ====================================================================
%% 日志记录
%% ====================================================================
log_homestead_mating(List)->
	TableName = get_log_table_name(logHomestead),
	ArgList =[ [RoleID, datetime({Date, Time})]
			 ||{RoleID, Date, Time} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s')", ArgList,1000).

log_friend_enargy(List)->
	TableName = get_log_table_name(logFriendEnargy),
	ArgList =[ [RoleID,Type,DRoleID,Value, datetime({Date, Time})]
			 ||{RoleID, Date, Time,DRoleID,Type,Value} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s')", ArgList,1000).
log_dungen_fight(List)->
	TableName = get_log_table_name(logDungenFight),
	ArgList =[ [RoleID, datetime({Date, Time}), DungenID,Result,Type,T]
			 ||{RoleID, DungenID,Result,Date, Time,Type,T} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s',~w,~w,~w,~w)", ArgList,1000).
log_world_boss(List)->
	TableName = get_log_table_name(logWorldBoss),
	ArgList =[ [RoleID, datetime({Date, Time}),Type]
			 ||{RoleID, Date, Time, Type} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s',~w)", ArgList,1000).
log_race_sign(List)->
	TableName = get_log_table_name(logRaceSign),
	ArgList =[ [RoleID, datetime({Date, Time})]
			 ||{RoleID, Date, Time} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s')", ArgList,1000).
log_pvp_fight(List)->
	TableName = get_log_table_name(logPvpFight),
	ArgList =[ [RoleID, datetime({Date, Time}),Result,Rank]
			 ||{RoleID, Date, Time,Result,Rank} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s',~w,~w)", ArgList,1000).

log_create_role(List)->
	TableName = get_log_table_name(logCreateRole),
	ArgList =[ [Accid,RoleID, quote(DevID), IP, Result, datetime({Date, Time}),Sex]
			 ||{Accid, RoleID, DevID, IP, Sex, Result, Date, Time} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(~w,~w,~s,'~w',~w,'~s', ~w)", ArgList,1000).

log_login(List) ->
	TableName = get_log_table_name(logLogin),
	ArgList =[ [Accid,RoleID, quote(DevID), IP, datetime(DateTime),LastDuration]
			 ||{Accid, RoleID, DevID, IP, DateTime,LastDuration} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(~w,~w,~s,'~w', '~s','~w')", ArgList,1000).

log_logout(List) ->
    TableName = get_log_table_name(logLogout),
    ArgList =[ [Accid rem ?AccidBase ,RoleID, quote(DevID), IP, datetime(DateTime),LastDuration,SrcType]
               ||{Accid, RoleID, DevID, IP, DateTime,LastDuration,SrcType} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(~w,~w,~s,'~w', '~s','~w',~w)", ArgList,1000).


log_selectGer(RoleID, GerTypeID) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logSelectGer),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w);",
						[TableName, RoleID, datetime({Date,Time}), GerTypeID]),
	sql_execute_with_log(Str).

log_buyTimes(List) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyTimes),
	ArgList =[ [RoleID, datetime({Date,Time}), VipLevel, Seq, NewValue, Add, Type]
			 ||{RoleID, VipLevel, Seq, NewValue, Add,Type} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(~w,'~s',~w,~w,~w, ~w, ~w)", ArgList,1000).

log_buyEnergy(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyEnergy),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_buyDscv(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyDscv),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_buyPVP(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyPVP),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_buyCoin(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyCoin),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_gold_consume([]) ->
	{ok, 0};
log_gold_consume(List) ->
	TableName = get_log_table_name(t_gold_consume),
	ArgList =[ [RoleID, VipLevel, Gold, GoldBonus, CurGold, CurGoldBonus, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Gold, GoldBonus, CurGold, CurGoldBonus, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_tencent_consume([]) ->
    {ok, 0};
log_tencent_consume(List) ->
    TableName = get_log_table_name(t_tencent_consume),
    ArgList =[ [RoleID, datetime(DateTime), quote(Billno), Gold, GoldBonus, Type, ArgID, quote(Desc)]
             ||{RoleID, DateTime, Billno, Gold, GoldBonus, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s',~s,~w,~w,~w,~w,~s)", ArgList,1000).
	
log_coin_consume([]) ->
	{ok,0};
log_coin_consume(List) ->
	TableName = get_log_table_name(t_coin_consume),
	ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_repu_consume([]) ->
	{ok,0};
log_repu_consume(List) ->
	TableName = get_log_table_name(t_repu_consume),
	ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_score_consume([]) ->
	{ok,0};
log_score_consume(List) ->
	TableName = get_log_table_name(t_score_consume),
	ArgList =[ [RoleID, VipLevel, Score, CurScore, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Score, CurScore, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_coin_add([]) ->
	{ok,0};
log_coin_add(List) ->
	TableName = get_log_table_name(t_coin_add),
	ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_repu_add([]) ->
	{ok,0};
log_repu_add(List) ->
	TableName = get_log_table_name(t_repu_add),
	ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_gold_bonus_add([]) ->
	{ok,0};
log_gold_bonus_add(List) ->
	TableName = get_log_table_name(t_gold_bonus_add),
	ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_gold_pay_add([])->
	{ok,0};
log_gold_pay_add(List)->
	TableName = get_log_table_name(t_gold_pay_add),
	TableName2 = get_log_table_name(t_gold_bonus_add),
	ArgList = [ [RoleID, VipLevel, Gold, CurGold, datetime(Time), AppItemID, quote(Desc), Md5,Accid,quote(DeviceID),SrcType]
			 ||{RoleID, VipLevel, Gold, CurGold, _, _, _Date, Time, _, AppItemID, Desc, Md5,Accid,DeviceID,SrcType} <-List],
	ArgList2 = [ [RoleID, VipLevel, GoldBonus, CurGoldBonus, datetime(Time), Type, AppItemID, quote(Desc)]
			 ||{RoleID, VipLevel, _, _, GoldBonus, CurGoldBonus, _Date, Time, Type, AppItemID, Desc, _Md5,_Accid,_DeviceID,_SrcType} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName2]), "(null, ~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList2, 1000),
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null, ~w,~w,~w,~w,'~s',~w,~s,'~s',~w,~s,~w)",ArgList, 1000).

log_item_add([]) ->
	{ok,0};
log_item_add(List) ->
	TableName = get_log_table_name(t_item_add),
	ArgList =[ [RoleID, ItemUID, ItemTypeID, AddNum, CurItemNum, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, ItemList, _Date, Time, Type, ArgID, Desc} <-List, [ItemUID, ItemTypeID, AddNum, CurItemNum] <- ItemList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_item_consume([]) ->
	{ok,0};
log_item_consume(List) ->
	TableName = get_log_table_name(t_item_consume),
	ArgList =[ [RoleID, ItemUID, ItemTypeID, DecNum, CurItemNum, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, ItemList, _Date, Time, Type, ArgID, Desc} <-List, [ItemUID, ItemTypeID, DecNum, CurItemNum] <- ItemList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	

log_ger_add([]) ->
	{ok,0};
log_ger_add(List) ->
	TableName = get_log_table_name(t_ger_add),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerRank, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, GerList, _Date, Time, Type, ArgID, Desc} <-List, [GerUID, GerTypeID, GerLevel, GerRank] <- GerList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_ger_consume([]) ->
	{ok,0};
log_ger_consume(List) ->
	TableName = get_log_table_name(t_ger_consume),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerRank, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, GerList, _Date, Time, Type, ArgID, Desc} <-List, [GerUID, GerTypeID, GerLevel, GerRank] <- GerList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_ger_uplevel([]) ->
	{ok,0};
log_ger_uplevel(List) ->
	TableName = get_log_table_name(t_ger_uplevel),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, datetime(Time)]
			 ||{RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, _Date, Time} <- List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).

log_ger_uprank([]) ->
	{ok,0};
log_ger_uprank(List) ->
	TableName = get_log_table_name(t_ger_uprank),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, NewRank, FoodGerUID, datetime(Time)]
			 ||{RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, NewRank, FoodGerUID, _Date, Time} <- List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).

log_ger_downrank([]) ->
    {ok,0};
log_ger_downrank(List) ->
    TableName = get_log_table_name(t_ger_downrank),
    ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerRank, NewRank, datetime(Time)]
             ||{RoleID, GerUID, GerTypeID, GerLevel, GerRank, NewRank, Time} <- List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).

log_item_uprank([]) ->
	{ok, 0};
log_item_uprank(List) ->
	TableName = get_log_table_name(t_item_uprank),
	ArgList = [ [RoleID,ItemUID,ItemTypeID,CurLevel,NewLevel,CurRank,NewRank,FoodItemUID,datetime(Time)]
			  ||{RoleID,ItemUID,ItemTypeID,CurLevel,NewLevel,CurRank,NewRank,FoodItemUID,_Date,Time}<-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values ", [TableName]),"(null,~w,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList, 1000).

log_item_uplevel([]) ->
	{ok, 0};
log_item_uplevel(List) ->
	TableName = get_log_table_name(t_item_uplevel),
	ArgList = [ [RoleID,ItemUID,ItemTypeID,AddLevel,NewLevel,AddTimes,Coin,datetime(Time)]
			  ||{RoleID,ItemUID,ItemTypeID,AddLevel,NewLevel,AddTimes,Coin,_Date,Time}<-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]),"(null,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList, 1000).

log_user_online(NumList, Date, Time) ->
    Time2 = minute(Time),
    Date2 = date(Date),
    ArgList = [[Time2,Num,Date2,SrcType] || {SrcType, Num} <- NumList],
    make_sql_batch_by_piece("insert into user_online values", "(null,'~s',~w,'~s',~w)", ArgList, 1000).

log_guide(RoleID, GuideVal, DateTime) ->
    Sql = io_lib:format("replace into logGuide values(~w,~w,'~s')", [RoleID, GuideVal, datetime(DateTime)]),
    sql_execute_with_log(Sql).


log_suggest(RoleID, Accid, RoleName, Title, Content, DateTime) ->
    TableName = get_log_table_name(logSuggest),
    Sql = io_lib:format("insert into ~s values(~w,~w,~s,~s,~s,'~s')",
                        [TableName,Accid,RoleID,quote(RoleName),quote(Title),quote(Content), datetime(DateTime)]),
    sql_execute_with_log(Sql).

get_recent_suggest_datetime(RoleID) ->
    TableName = get_log_table_name(logSuggest),
    Sql = io_lib:format("select datetime from ~s where roleID = ~w order by datetime desc limit 1", [TableName, RoleID]),
    case get_row(Sql) of
        [LoginTime] ->
            util:toUnixTime(LoginTime);
        _ ->
            ?undefined
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%%指定匹配值分段批量删除
make_sql_batch_del_by_piece(Sql, List, PieceNum) ->
    make_sql_batch_del_by_piece(Sql, List, PieceNum, 0, "").


make_sql_batch_del_by_piece(Sql, [], _PieceNum, _AccNum, Acc) ->
    if Acc == "" ->
           ignore;
       true ->
           Sql2 = Sql ++ "(" ++ Acc,
           sql_execute_with_log(Sql2)
    end;
make_sql_batch_del_by_piece(Sql, List, PieceNum, PieceNum, Acc) ->
    Sql2 = Sql ++ "(" ++ Acc,
    sql_execute_with_log(Sql2),
    make_sql_batch_del_by_piece(Sql, List, PieceNum, 0, "");
make_sql_batch_del_by_piece(Sql, [E|List], PieceNum, AccNum, Acc) ->
    case AccNum of
        0 ->
            Acc2 = erlang:integer_to_list(E) ++ ");";
        _ ->
            Acc2 = erlang:integer_to_list(E) ++ "," ++ Acc
        end,
    make_sql_batch_del_by_piece(Sql, List, PieceNum, AccNum+1, Acc2).
%% 批量插入
make_sql_batch(Sql, Format, List) when List =/= []->
	Str = lists:foldl(fun(E,Acc) ->
							  ","++io_lib:format(Format,E)++Acc
					  end, ";", List),
	Sql++tl(Str).

%% 分段批量插入
make_sql_batch_by_piece(Sql, Format, List, PieceNum) ->
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "").

make_sql_batch_by_piece(Sql, _Format, [], _PieceNum, _AccNum, Acc) ->
	if Acc == "" ->
		   ignore;
	   true ->
		   Sql2 = Sql ++ tl(Acc),
		   sql_execute_with_log(Sql2)
	end;
make_sql_batch_by_piece(Sql, Format, List, PieceNum, PieceNum, Acc) ->
	Sql2 = Sql ++ tl(Acc),
	sql_execute_with_log(Sql2),
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "");
make_sql_batch_by_piece(Sql, Format, [E|List], PieceNum, AccNum, Acc) ->
	Acc2 = ","++io_lib:format(Format,E)++Acc,
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, AccNum+1, Acc2).
	
	
to_term(Bin)->
	to_term(Bin,[]).
to_term(Bin, Default) ->
	case catch binary_to_term(Bin) of
		{'EXIT',_} ->
			Default;
		Term ->
			Term
	end.

to_bin(Term) ->
	quote(term_to_binary(Term)).

compress_encode(Term) ->
	zlib:compress(term_to_binary(Term)).

uncompress_decode(Bin) ->
	uncompress_decode(Bin,[]).
uncompress_decode(Bin, Default) ->
	case catch binary_to_term(zlib:uncompress(Bin)) of
		{'EXIT',_} ->
			Default;
		Term ->
			Term
	end.

datetime({{A,B,C},{D,E,F}}) ->
	io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[A,B,C,D,E,F]);
datetime(Err) ->
	?ERR("datetime err:~p~n",[Err]).
date({A,B,C}) ->
	io_lib:format("~w-~.2.0w-~.2.0w",[A,B,C]);
date(Err) ->
	?ERR("date err:~p~n",[Err]).
time({A,B,C}) ->
	io_lib:format("~.2.0w:~.2.0w:~.2.0w",[A,B,C]).
minute({A,B,_C}) ->
	io_lib:format("~.2.0w:~.2.0w",[A,B]).
	

bool2int(true)->
	1;
bool2int(false)->
	0.

int2bool(1)->
	true;
int2bool(0)->
	false.

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];	%% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote2(String) when is_list(String) ->
    lists:reverse(quote(String, []));
quote2(Bin) when is_binary(Bin) ->
    list_to_binary(quote2(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

%% 合服脚本对应表的数据转换处理
trans(gActivity, DataList) ->
    NewDataList =
        lists:map(fun([A, B, C, D]) ->
                          [A, B, db_sql:quote(C), db_sql:quote(D)]
                  end, DataList),
    {"insert into gActivity values", "(~w,~w,~s,~s)", NewDataList};
trans(gBagItem, DataList) ->
    {"insert into gBagItem values", "(~w,~w,~w,~w)", DataList};
trans(gBestPassChapter, DataList) ->
    {"insert into gBestPassChapter values", "(~w,~w)", DataList};
trans(gCard, DataList) ->
    NewDataList =
        lists:map(fun([A, B, C, D]) ->
                          [A, db_sql:quote(B), db_sql:quote(C), D]
                  end, DataList),
    {"insert into gCard values", "(~w,~s,~s,~w)", NewDataList};
trans(gChapter, DataList) ->
    NewDataList =
        lists:map(fun([A, B, C, {date, D}]) ->
                          [A, B, C, db_sql:date(D)]
                  end, DataList),
    {"insert into gChapter values","(~w,~w,~w,'~s')", NewDataList};
trans(gDungeon, DataList) ->
    {"insert into gDungeon values", "(~w,~w,~w,~w,~w)", DataList};
trans(gEquip, DataList) ->
    {"insert into gEquip values", "(~w,~w,~w,~w,~w,~w,~w,~w,~w)", DataList};
trans(gFighterList, DataList) ->
    NewDataList =
        lists:map(fun([A, B, C, D, E]) ->
                          [A, db_sql:quote(B), db_sql:quote(C), D, E]
                  end, DataList),
    {"insert into gFighterList values", "(~w,~s,~s,~w,~w)", NewDataList};
trans(gFriend, DataList) ->
    {"insert into gFriend values", "(~w,~w,~w)", DataList};
trans(gGather, DataList) ->
    {"insert into gGather values", "(~w,~w,~w)", DataList};
trans(gGer, DataList) ->
    {"insert into gGer values", "(~w,~w,~w,~w,~w,~w,~w)", DataList};
trans(gGift, DataList) ->
    NewDataList =
        lists:map(fun([A, B]) ->
                          [A, db_sql:quote(B)]
                  end, DataList),
    {"insert into gGift values", "(~w,~s)", NewDataList};
trans(gGuide, DataList) ->
    {"insert into gGuide values", "(~w,~w)", DataList};
trans(gHron, DataList) ->
    NewDataList =
        lists:map(fun([V1, {date, V2}, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12]) ->
                          [V1, db_sql:date(V2), V3, V4, V5, V6, V7, db_sql:quote(V8), V9, V10, V11, V12]
                  end, DataList),
    {"insert into gHron values", "(~w,'~s',~w,~w,~w,~w,~w,~s,~w,~w,~w,~w)", NewDataList};
trans(gInvite, DataList) ->
    {"insert into gInvite values", "(~w,~w)", DataList};
trans(gInviteRoleList, DataList) ->
    {"insert into gInviteRoleList values", "(~w,~w)", DataList};
trans(gLimit, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3, V4, V5, V6, V7, V8]) ->
                          [V1, V2, V3, V4, db_sql:quote(V5), V6, V7, V8]
                          end, DataList),
    {"insert into gLimit values", "(~w,~w,~w,~w,~s,~w,~w,~w)", NewDataList};
trans(gOfflineDeductGold, DataList) ->
    {"insert into gOfflineDeductGold values", "(~w,~w)", DataList};
trans(gOfflinePayLog, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3, V4, V5]) ->
                          [V1, V2, db_sql:quote(V3), V4, V5]
                          end, DataList),
    {"insert into gOfflinePayLog values", "(~w,~w,~s,'~s',~w)", NewDataList};
trans(gPay, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3, V4, {datetime, V5}, V6]) ->
                          [V1, V2, V3, V4, db_sql:datetime(V5), V6]
                  end, DataList),
    {"insert into gPay values", "('~s',~w,'~s',~w,'~s',~w)", NewDataList};
trans(gPush, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3, V4]) ->
                          [V1, db_sql:quote(V2), V3, V4]
                  end, DataList),
    {"insert into gPush values", "(~w,~s,~w,~w)", NewDataList};
trans(gRole, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16,V17,V18]) ->
                          [V1, V2, db_sql:quote(V3), V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16,V17,quote(V18)]
                  end, DataList),
    {"insert into gRole values", "(~w,~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s)", NewDataList};
trans(gRoleExtra, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3, VV3,V4, V5, V6, V7, {date, V8}, V9, V10, V11, V12, V13, V14, V15, {date, V16}, V17, V18,
                       V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, {date, V29}, V30, {date, V31}, V32, V33, V34, V35]) ->
                          [V1, V2, V3, VV3,V4, V5, V6, V7, db_sql:date(V8), V9, V10, V11, V12, V13, V14, V15, db_sql:date(V16), V17, V18,
                           V19, V20, V21, V22, V23, V24, V25, V26, V27, db_sql:quote(V28), db_sql:date(V29), V30, db_sql:date(V31), V32, V33, V34, db_sql:quote(V35)] 
                  end, DataList),
    {"insert into gRoleExtra values", "(~w,~w,~w,~w,~w,~w,~w,~w,'~s',~w,~w,~w,~w,~w,~w,~w,'~s',~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,'~s',~w,'~s',~w,~w,~w,~s)", NewDataList};
trans(gShopNum, DataList) ->
    {"insert into gShopNum values", "(~w,~w,~w,~w)", DataList};
trans(gEncounter, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3]) ->
                          [V1, V2, db_sql:quote(V3)]
                  end, DataList),
    {"insert into gEncounter values", "(~w,~w,~s)", NewDataList};
trans(gLieuInfo, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2]) ->
                          [V1, db_sql:quote(V2)]
                          end, DataList),
    {"insert into gLieuInfo values", "(~w,~s)", NewDataList};
trans(gTalk, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2]) ->
                          [V1, db_sql:quote(V2)]
                  end, DataList),
    {"insert into gTalk values", "(~w, ~s)", NewDataList};
trans(gOtherRecord, DataList) ->
    NewDataList =
        lists:map(fun([V1, {date, V2}, V3, V4, V5]) ->
                          [V1, db_sql:date(V2), V3, V4, V5]
                  end, DataList),
    {"insert into gOtherRecord values", "(~w, '~s', ~w, ~w, ~w)", NewDataList};
trans(gTreasureHouse, DataList) ->
    NewDataList =
        lists:map(fun([V1, V2, V3, V4, V5, V6, V7, V8, V9, {date, V10}, V11]) ->
                          [V1, V2, db_sql:quote(V3), V4, V5, V6, V7, db_sql:quote(V8), V9, db_sql:date(V10), V11]
                  end, DataList),
    {"insert into gTreasureHouse values", "(~w,~w, ~s,~w,~w,~w,~w,~s,~w,'~s',~w)", NewDataList};
trans(gTask,DataList)->
	{"insert into gTask values", "(~w,~w,~w,~w,~s)", DataList}.
