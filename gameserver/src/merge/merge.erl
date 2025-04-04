%%%-------------------------------------------------------------------
%%% @author admin <admin360@gmail.com>
%%% @copyright (C) 2013, crimoon
%%% @doc
%%% 合服相关代码
%%% @end
%%%-------------------------------------------------------------------
-module(merge).

-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

%% API
-export([
         start/0,
         spawn_start/0
        ]).

-define(APP, [crypto,emysql]).
-define(LOG(Format, Args), global:send(merge_log_server, {log, ?MODULE, ?LINE, erlang:localtime(), Format, Args})).

execute(PoolID, Sql) ->
    case catch emysql:execute(PoolID, Sql) of
        {ok_packet, A,B,C,D,E,F} ->
            {ok_packet, A,B,C,D,E,F};
        {result_packet, A, B, C, D} ->
            {result_packet, A, B, C, D};
        {'EXIT',{connection_down,{and_conn_reset_failed,{cannot_reopen_in_reset,{failed_to_connect_to_database,econnrefused}}}}} ->
            re_add_pool(),
            execute(PoolID, Sql);
        Exception ->
            ?LOG("~w~n", [Exception]),
            timer:sleep(10000),
            execute(PoolID, Sql)
    end.



%% 如果mysql服务挂掉，需要先remove_pool，再add_pool,在这之前可以加一个重启mysql服务的命令或者手动重启
re_add_pool() ->
    case os:type() of
        {win32,nt} ->
            to_do_os_cmd;
        {unix, linux} ->
            to_do_os_cmd
    end,
    ServerIDList = do_merge:get(server_id_list),
    MasterServerID = data_setting:get(server_id),
    AllServerIDList = [MasterServerID|ServerIDList],
    ok = remove_pool(MasterServerID, AllServerIDList),
    re_add_pool(MasterServerID, AllServerIDList).

re_add_pool(_MasterServerID, []) ->
    ok;
re_add_pool(MasterServerID, AllServerIDList) ->
    case AllServerIDList of
        [MasterServerID|LeftServerIDList] ->
            PoolID = get_master_pool_id(),
            {IP,Port,Name,Pass,DBName,Num} = data_setting:get(database);
        [SlaveServerID|LeftServerIDList] ->
            PoolID = get_slave_pool_id(SlaveServerID),
            {IP,Port,Name,Pass,DBName,Num} = do_merge:get({database, SlaveServerID})
    end,
    case catch emysql:add_pool(PoolID, Num,Name,Pass, IP, Port,DBName, utf8) of
        ok ->
            re_add_pool(MasterServerID, LeftServerIDList);
        {'EXIT',pool_already_exists} ->
            re_add_pool(MasterServerID, LeftServerIDList);
        {failed_to_connect_to_database,econnrefused} ->
            re_add_pool();
        Err ->
            ?LOG("~w~n", [Err]),
            timer:sleep(10000),
            re_add_pool(MasterServerID, AllServerIDList)
    end.

%% remove_pool的操作
remove_pool(_MasterServerID, []) ->
    ok;
remove_pool(MasterServerID, AllServerIDList) ->
    case AllServerIDList of
        [MasterServerID|LeftServerIDList] ->
            PoolID = get_master_pool_id();
        [SlaveServerID|LeftServerIDList] ->
            PoolID = get_slave_pool_id(SlaveServerID)
    end,
    case catch emysql:remove_pool(PoolID) of
        ok ->
            remove_pool(MasterServerID, LeftServerIDList);
        {'EXIT',pool_not_found} ->
            remove_pool(MasterServerID, LeftServerIDList);
        Err ->
            ?LOG("~w~n", [Err]),
            timer:sleep(1000),
            remove_pool(MasterServerID, AllServerIDList)
    end.


%% 分段批量插入
make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum) ->
    make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, 0, "").

make_sql_batch_by_piece(PoolID, Sql, _Format, [], _PieceNum, _AccNum, Acc) ->
    if Acc == "" ->
           ignore;
       true ->
           Sql2 = Sql ++ tl(Acc),
           {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2)
    end;
make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, PieceNum, Acc) ->
    Sql2 = Sql ++ tl(Acc),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2),
    make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, 0, "");
make_sql_batch_by_piece(PoolID, Sql, Format, [E|List], PieceNum, AccNum, Acc) ->
    Acc2 = ","++io_lib:format(Format,E)++Acc,
    make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, AccNum+1, Acc2).


%%指定匹配值分段批量删除
make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum) ->
    make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, 0, "").


make_sql_batch_del_by_piece(PoolID, Sql, [], _PieceNum, _AccNum, Acc) ->
    if Acc == "" ->
           ignore;
       true ->
           Sql2 = Sql ++ "(" ++ Acc,
           {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2)
    end;
make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, PieceNum, Acc) ->
    Sql2 = Sql ++ "(" ++ Acc,
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2),
    make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, 0, "");
make_sql_batch_del_by_piece(PoolID, Sql, [E|List], PieceNum, AccNum, Acc) ->
    case AccNum of
        0 ->
            Acc2 = erlang:integer_to_list(E) ++ ");";
        _ ->
            Acc2 = erlang:integer_to_list(E) ++ "," ++ Acc
        end,
    make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, AccNum+1, Acc2).

%%%===================================================================
%%% API
%%%===================================================================
spawn_start() ->
    erlang:spawn(fun() -> merge:start() end).

start() ->
    erlang:register(?MODULE, self()),
    start_init(),
    ?LOG("~ts~n", ["数据库连接成功"]),
    do_start_merge(),
    ?LOG("~ts~n", ["合服完毕，恭喜！"]),
    tk_misc:stop_applications(?APP).

get_master_pool_id() ->
    master_pool_id.

get_slave_pool_id(ServerID) when erlang:is_integer(ServerID) ->
    erlang:list_to_atom(lists:append("slave_pool_id_", erlang:integer_to_list(ServerID))).

start_init() ->
    %% 启动emysql
    tk_misc:start_applications(?APP),
    tk_config:reload_config("config/do_merge.config",do_merge,key_value,original),
    %% 启动日志服务器
    do_start_log_server(),
    ?LOG("~ts~n", ["主节点：读取合服配置文件成功，日志服务器启动成功，准备连接mysql"]),
    {IP,Port,Name,Pass,DBName,Num} = data_setting:get(database),
    ok = emysql:add_pool(get_master_pool_id(), Num,Name,Pass, IP, Port,DBName, utf8),
    ServerIDList = do_merge:get(server_id_list),
    ?LOG("~ts:~w~n", ["主节点mysql连接成功，准备连接从服务器的mysql，本次参与合服的区包括", ServerIDList]),
    lists:foreach(
      fun(ServerID) ->
              {SlaveIP,SlavePort,SlaveName,SlavePass,SlaveDBName,SlaveNum} = do_merge:get({database, ServerID}),
              ok = emysql:add_pool(get_slave_pool_id(ServerID), SlaveNum,SlaveName,SlavePass, SlaveIP, SlavePort,SlaveDBName, utf8)
      end, ServerIDList).

%% 开始合并数据
do_start_merge() ->
    ServerIDList = do_merge:get(server_id_list),
    MasterServerID = data_setting:get(server_id),
    lists:foreach(fun(ServerID) ->
                          case ServerID =< MasterServerID of
                              true ->
                                  ?LOG("~ts~n", ["配置错误，主服务器的ID不是最小的,请检查配置"]),
                                  erlang:throw(config_error);
                              false ->
                                  next
                          end
                  end, ServerIDList),
    ?LOG("~ts~n", ["先删除所有的邮件、战报、汉帝宝库数据"]),
    do_delete_tables(ServerIDList),
    ?LOG("~ts~n", ["删除死号数据，包括机器人的数据"]),
    do_delete_dead_player(ServerIDList),
    ?LOG("~ts~n", ["准备处理角色重名"]),
    do_process_role_name(ServerIDList),
    ?LOG("~ts~n", ["准备处理重复的gpay主键"]),
    do_process_gpay(ServerIDList),
    ?LOG("~ts~n", ["账号处理完毕，准备 合并数据"]),
    %% 开始遍历各种数据并插入到最终数据表中
    do_merge_data(ServerIDList),
    
    ?LOG("~ts~n", ["数据合并完毕，开始更新活动数据"]),
    
    do_process_activity(),

    ?LOG("~ts~n", ["合服完毕，恭喜！"]),
    ok.


do_process_activity() ->
    %% 先处理争霸
    erlang:put(?is_doing_merge, true),
    ets:new(roleName, [public, set, named_table]),
    ets:new(?ETS_ID, [{keypos, 1}, set, public, named_table]),
    init_id(gerID, "gerID","gGer",?GER_ID_BASE),
    ItemID1 = init_id2("itemUID", "gBagItem", ?ITEM_ID_BASE),
    ItemID2 = init_id2("itemUID", "gEquip", ?ITEM_ID_BASE),
    ItemID3 = erlang:max(ItemID1,ItemID2),
    ets:insert(?ETS_ID, {itemUID,ItemID3}),
    MasterPoolID = get_master_pool_id(),
    Config = data_title:get(title_condition),
    {result_packet, _, _, RoleList,_} = execute(MasterPoolID, "select * from gRole"),
    RoleList2 =
        lists:map(fun([_V1, _V2, V3, _V4, _V5, _V6, _V7, _V8, _V9, _V10, _V11, _V12, _V13, _V14, _V15, _V16] = Role) ->
                          ets:insert(roleName, {V3, 1}),
                          erlang:list_to_tuple(Role)
                  end, RoleList),
    RoleList3 = lists:keysort(15, RoleList2),
    %%是否需要加机器人
    RoleNum = erlang:length(RoleList3),
    case RoleNum < 9999 of
        true ->
            ?LOG("RoleNum:~w, 需要添加机器人~n", [RoleNum]),
            {ok, RobotList} = gen_robot_data(RoleNum, MasterPoolID),
            RobotList2 =
                lists:map(fun(Robot) ->
                                  erlang:list_to_tuple(Robot)
                          end, RobotList),
            RoleList4 = lists:append(RobotList2, RoleList3);
        false ->
            RoleList4 = RoleList3
    end,
    {_, RoleList5, RankList} =
        lists:foldr(fun(Role, {Rank, AccRoleList, AccRankList}) ->
                            RoleLevel = erlang:element(5, Role),
                            NewTitle = pvp_server:cacl_title(RoleLevel, Rank, Config),
                            NewRole = erlang:setelement(14, Role, NewTitle),
                            {Rank + 1, [NewRole|AccRoleList], [{Rank, erlang:element(1, NewRole), NewTitle}|AccRankList]}
                    end, {1, [], []}, RoleList4),
    ?LOG("先删除全部gRole数据，再批量插入~n", []),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, "delete from gRole"),
    
    RoleList6 =
        lists:map(fun(Role) ->
                          erlang:tuple_to_list(Role)
                  end, RoleList5),
    {InsertSql, Format, NewDataList} = db_sql:trans(gRole, RoleList6),
    ?LOG("插入全部的gRole数据~n", []),
    make_sql_batch_by_piece(MasterPoolID, InsertSql, Format, NewDataList, 1000),
    ?LOG("gRole数据写入成功~n", []),
    RankList2 = lists:sublist(lists:reverse(RankList), 9999),
    Bin = db_sql:compress_encode(RankList2),
    ?LOG("更新pvp排行榜数据~n", []),
    PvpSql = io_lib:format("replace into gETC values (~w,~s);",[?DB_ETC_KEY_PVP, db_sql:quote(Bin)]),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, PvpSql),
    %% 争霸的数据处理完毕
    ?LOG("虎牢关、战南蛮重置，清空华容道的所有排名信息，所有玩家华容道的剩余挑战次数重置为3次,清除夺宝和帝王争霸战数据~n", []),
    %% 清空华容道的所有排名信息，所有玩家华容道的剩余挑战次数重置为3次,清除夺宝和帝王争霸战数据
    
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;", [?DB_ETC_KEY_NANM_OFFLINE_PLAY])),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;", [?DB_ETC_KEY_NANM_BUFF])),
    
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;", [?DB_ETC_KEY_HRON])),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;", [?DB_ETC_KEY_HRON_TERM])),
    
    
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;", [?DB_ETC_KEY_FIRE])),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;", [?DB_ETC_KEY_FIRE_ROLEINFO])),
    
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, "delete from gHron;"),
    %% 删除活动排行榜数据
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;", [?DB_ETC_KEY_ACTIVITYRANK])),
    %% 保留积天返利的活动数据
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, "delete from gActivity where actID <> 10001;"),
    ?LOG("当日的所有类型体力购买次数，银两购买次数，重置为每日可买最大次数~n", []),
    %% 当日的所有类型体力购买次数，银两购买次数，重置为每日可买最大次数
    ResetSql = io_lib:format("update gRoleExtra set energyBuyTimes = 0, dscvBuyTimes = 0, pvpBuyTimes = 0, plunderBuyTimes = 0, coinBuyTimes = 0, lastBuyTimesRefreshDate = '~s';", [db_sql:date(erlang:date())]),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, ResetSql),
    %% 合并新年红包的活动数据，数据结构特殊，这里单独处理
    merge_etc_data(),
    ok.

merge_etc_data() ->
    MasterPoolID = get_master_pool_id(),
    ServerIDList = do_merge:get(server_id_list),
    SlavePoolIDList = lists:map(fun(ServerID) ->
                                        get_slave_pool_id(ServerID)
                                end, ServerIDList),
    merge_rebate(MasterPoolID, SlavePoolIDList).

merge_rebate(MasterPoolID, SlavePoolIDList) ->
    SelectSql = io_lib:format("select value from gETC where `key`=~w;", [?DB_ETC_KEY_REBATE_ROLEINFO]),
    RebateDataList =
        lists:foldr(fun(PoolID, Acc) ->
                            {result_packet, _, _, Data,_} = execute(PoolID, SelectSql),
                            case Data of
                                [[Bin]] ->
                                    Term = binary_to_term(zlib:uncompress(Bin)),
                                    case erlang:is_list(Term) of
                                        true ->
                                            Term ++ Acc;
                                        false ->
                                            Acc
                                    end;
                                _ ->
                                    Acc
                            end
                    end, [], [MasterPoolID|SlavePoolIDList]),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID,
                                       io_lib:format("replace into gETC values (~w,~s);",[?DB_ETC_KEY_REBATE_ROLEINFO, db_sql:quote(zlib:compress(term_to_binary(RebateDataList)))])).    

init_id(IDName, Key,Table,Base) ->
    ID = init_id2(Key,Table,Base),
    ets:insert(?ETS_ID, {IDName, ID}).

init_id2(Key,Table,Base) ->
    Sql = io_lib:format("select max(~s) from ~s;",[Key,Table]),
    {result_packet, _,_,Rows,_} = execute(get_master_pool_id(), Sql),
    case Rows of
        [[Max]] when is_integer(Max) ->
            next;
        _ ->
            Max=0
    end,
    erlang:max(Max, Base).

gen_robot_data(RoleNum, MasterPoolID) when RoleNum < 9999 ->
    TermList = gen_config_term_list(RoleNum),
    tk_config:load_data_name(),
    AllGer = gen_account:all_ger(),
    AllItem = gen_account:all_item(),
    {RobotList, GerList} = 
        lists:foldl(fun(E, Acc) ->  
                            gen_account:gen_account(E, Acc, AllGer, AllItem)  
                    end, {[],[]}, TermList),
    RobotList2 = lists:map(fun({_, #role{roleID=RoleID,accid=Vaccid,roleName=VroleName,isMale=VisMale,level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,
                                         gold=Vgold,goldBonus=VgoldBonus,goldUsed=VgoldUsed,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,
                                         fightPower=VfightPower,lastLogoutTime=VlastLogoutTime}}) ->
                                   [RoleID,Vaccid,VroleName,db_sql:bool2int(VisMale),Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,VvipLevel,
                                    VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime]
                           end, RobotList),
    %% 写gFighterList
    ?LOG("写gFighterList~n", []),
    FighterDataList = lists:map(fun({RoleID, FighterList}) ->
                                        [RoleID,erlang:term_to_binary(FighterList), erlang:term_to_binary([]), 0, 0]
                                end, GerList),
    {InsertSql1, Format1, NewFighterDataList} = db_sql:trans(gFighterList, FighterDataList),
    make_sql_batch_by_piece(MasterPoolID, InsertSql1, Format1, NewFighterDataList, 1000),
    ?LOG("写gGer~n", []),
    %% 写gGer
    GerDataList =
        lists:foldl(
          fun({RoleID, RoleGerList}, Acc1) ->
                  lists:foldl(
                    fun(Ger, Acc2) ->
                            #ger{gerID=GerID,gerBase=GerBase} = Ger,
                            #gerBase{gerExp=GerExp
                                     ,gerLevel=GerLevel
                                     ,gerPos=GerPos
                                     ,gerQuality=GerRank
                                     ,gerTypeID=GerTypeID}=GerBase,
                            [[GerID,RoleID,GerTypeID,GerLevel,GerExp,GerRank,GerPos]|Acc2]
                    end, Acc1, RoleGerList)
          end, [], GerList),
    {InsertSql2, Format2, NewGerDataList} = db_sql:trans(gGer, GerDataList),
    make_sql_batch_by_piece(MasterPoolID, InsertSql2, Format2, NewGerDataList, 1000),
    ?LOG("生成机器人成功~n", []),
    {ok, RobotList2}.

gen_config_term_list(RoleNum) ->
    StartRank = RoleNum + 1,
    {ok, TermList} = file:consult(filename:join([tk_config:root_dir(),"config/data_gen_account.config"])),
    lists:foldr(fun({{Rank1, Rank2},{GerStar,GerLevel,EquipStar,EquipLevel,GerNum}}, Acc) ->
                        if
                            StartRank =< Rank1 ->
                                [{{Rank1, Rank2},{GerStar,GerLevel,EquipStar,EquipLevel,GerNum}}|Acc];
                            StartRank =< Rank2 ->
                                [{{StartRank, Rank2},{GerStar,GerLevel,EquipStar,EquipLevel,GerNum}}|Acc];
                            true ->
                                Acc
                        end
                end, [], TermList).

do_merge_data(ServerIDList) ->
    MasterPoolID = get_master_pool_id(),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, "ALTER TABLE  `gRole` CHANGE  `roleName`  `roleName` VARCHAR( 20 ) BINARY CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT  '主公名字'"),
    lists:foreach(fun(ServerID) ->
                          SlavePoolID = get_slave_pool_id(ServerID),
                          lists:foreach(fun(Table) ->
                                                SelectSql = io_lib:format("select * from ~w", [Table]),
                                                ?LOG("ServerID:~w, Table:~w, ~ts~n", [ServerID, Table, SelectSql]),
                                                {result_packet, _, _, DataList,_} = execute(SlavePoolID, SelectSql),
                                                {InsertSql, Format, NewDataList} = db_sql:trans(Table, DataList),
                                                ?LOG("ServerID:~w, Table:~w, ~ts~n", [ServerID, Table, InsertSql]),
                                                make_sql_batch_by_piece(MasterPoolID, InsertSql, Format, NewDataList, 1000)
                                        end, ?all_role_id_tables)
                    end, ServerIDList).


do_delete_tables(ServerIDList) ->
    MasterServerID = data_setting:get(server_id),
    lists:foreach(fun(ServerID) ->
                          PoolID =
                              case ServerID =:= MasterServerID of
                                  true ->
                                      get_master_pool_id();
                                  false ->
                                      get_slave_pool_id(ServerID)
                              end,
                          ?LOG("~ts,ServerID:~w,PoolID:~w~n", ["开始删除不需要的数据", ServerID, PoolID]),
                          {ok_packet, _,_,_,_,_,_} = execute(PoolID, "truncate table gMail;"),
                          {ok_packet, _,_,_,_,_,_} = execute(PoolID, "truncate table gReplay;"),
                          {ok_packet, _,_,_,_,_,_} = execute(PoolID, "truncate table gHist;"),
                          {ok_packet, _,_,_,_,_,_} = execute(PoolID, "truncate table gTreasureHouse;")
                  end, [MasterServerID|ServerIDList]).

do_delete_dead_player(ServerIDList) ->
    MasterServerID = data_setting:get(server_id),
    lists:foreach(fun(ServerID) ->
                          PoolID =
                              case ServerID =:= MasterServerID of
                                  true ->
                                      get_master_pool_id();
                                  false ->
                                      get_slave_pool_id(ServerID)
                              end,
                          do_delete_dead_player2(PoolID, ServerID)
                  end, [MasterServerID|ServerIDList]),
    ok.

do_delete_dead_player2(PoolID, ServerID) ->
    case do_merge:get(is_delete_dead_player) of
        true ->
            NowTimestamp = util:now(),
            OfflineDaysLimit = do_merge:get(offline_days),
            LimitTimestamp = NowTimestamp - OfflineDaysLimit * 24 * 60 * 60,
            LevelLimit = do_merge:get(level),
            PayGoldLimit = do_merge:get(pay_gold),
            SelectSql = io_lib:format("select roleID from gRole where goldTotalPaid <= ~w and level <= ~w and lastLogoutTime <= ~w;", [PayGoldLimit, LevelLimit, LimitTimestamp]),
            {result_packet, _, _, RoleIDList,_} = execute(PoolID, SelectSql);
        false ->
            RoleIDList = []
    end,
    ?LOG("ServerID:~w, 筛选出~w个死号玩家~n", [ServerID, erlang:length(RoleIDList)]),
    RobotIDList = lists:seq((ServerID + 1) * 1000000 + 1, (ServerID + 1) * 1000000 + 9999),
    DelRoleIDList = lists:append(RobotIDList, lists:flatten(RoleIDList)),
    lists:foreach(
      fun(Table) ->
              ?LOG("ServerID:~w, 删除(~w)表~n", [ServerID, Table]),
              DeleteSql = io_lib:format("delete from ~w where roleID in", [Table]),
              make_sql_batch_del_by_piece(PoolID, DeleteSql, DelRoleIDList, 5000),
              case Table of
                  gFriend ->
                      DeleteSql2 = "delete from gFriend where friendID in",
                      make_sql_batch_del_by_piece(PoolID, DeleteSql2, DelRoleIDList, 5000);
                  gInviteRoleList ->
                      DeleteSql2 = "delete from gInviteRoleList where inviteRoleID in",
                      make_sql_batch_del_by_piece(PoolID, DeleteSql2, DelRoleIDList, 5000);
                  _ ->
                      next
              end
      end, ?all_role_id_tables).

%% 处理重复
do_process_gpay(SlaveServerIDList) ->
        %% 先跑一次循环，检查哪些gPay主键是重复的
    MasterServerID = data_setting:get(server_id),
    Md5List =
        lists:foldr(
          fun(ServerID, AccMd5List) ->
                  PoolID =
                      case ServerID =:= MasterServerID of
                          true ->
                              get_master_pool_id();
                          false ->
                              get_slave_pool_id(ServerID)
                      end,
                  {ok_packet, _,_,_,_,_,_} = execute(PoolID, "alter table gPay change receiptMd5 receiptMd5 varchar(80) not null;"),
                  {result_packet, _, _, Rows,_} = execute(PoolID, "select * from gPay"),
                  lists:foldr(
                    fun([Md5, _, _, _, _], AccMd5List2) ->
                            case lists:keyfind(Md5, 1, AccMd5List2) of
                                false ->
                                    [{Md5, [ServerID]}|AccMd5List2];
                                {Md5, ServerIDList} ->
                                    lists:keyreplace(Md5, 1, AccMd5List2, {Md5, [ServerID|ServerIDList]})
                            end  
                    end, AccMd5List, Rows)
          end, [], [MasterServerID|SlaveServerIDList]),
    DuplicateList =
        lists:foldr(fun({_, ServerIDList} = R, Acc) ->
                            case erlang:length(ServerIDList) > 1 of
                                true ->
                                    [R | Acc];
                                false ->
                                    Acc
                            end
                    end, [], Md5List),
    ?LOG("有重复情况的gpay一共有~w个~n", [erlang:length(DuplicateList)]),
    ?LOG("~w~n", [DuplicateList]),
    Fix = do_merge:get(rename_fix),
    lists:foreach(
      fun({Md5, ServerIDList}) ->
              lists:foreach(
                fun(ServerID) ->
                        NewMd5 = erlang:list_to_binary(erlang:binary_to_list(Md5) ++ Fix ++ erlang:integer_to_list(ServerID)),
                        change_gpay_md5(NewMd5, ServerID, Md5)
                end, ServerIDList)
      end, DuplicateList),
    ok.

%% 修改gpay主键
change_gpay_md5(NewMd5, ServerID, Md5) ->
    MasterServerID = data_setting:get(server_id),
    PoolID =
        case ServerID =:= MasterServerID of
            true ->
                get_master_pool_id();
            false ->
                get_slave_pool_id(ServerID)
        end,
    Sql = io_lib:format("update gPay set receiptMd5= '~s' where receiptMd5 = '~s';",[NewMd5, Md5]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql),
    ok.


%% 处理角色重名的情况，并根据规则进行重命名
%% 数据库中记录 role_id_list  [{server_id, role_id}, ......]
do_process_role_name(ServerIDList) ->
    %% 先跑一次循环，检查哪些角色名是重复的
    MasterServerID = data_setting:get(server_id),
    NameList =
        lists:foldr(
          fun(ServerID, AccNameList) ->
                  PoolID =
                      case ServerID =:= MasterServerID of
                          true ->
                              get_master_pool_id();
                          false ->
                              get_slave_pool_id(ServerID)
                      end,
                  {result_packet, _, _, Rows,_} = execute(PoolID, "select * from gRole"),
                  lists:foldr(
                    fun([VroleID, _Vaccid, VroleName, _VisMale, _Vlevel, _Vexp, _Vcoin, _Vreputation, _Vgold, _VgoldBonus, _VgoldUsed, _VvipLevel, _VgoldTotalPaid, _Vtitle, _VfightPower, _VlastLogoutTime], AccNameList2) ->
                            VroleNameT = del_tail_space(VroleName),
                            case VroleNameT =/= VroleName of
                                true ->
                                    ?LOG("出现末尾带空格的名字, VroleName:~w, VroleNameT:~w, VroleID:~w, ServerID:~w", [VroleName, VroleNameT, VroleID, ServerID]);
                                false ->
                                    next
                            end,
                            case lists:keyfind(VroleNameT, 1, AccNameList2) of
                                false ->
                                    [{VroleNameT, [{ServerID, VroleID}]}|AccNameList2];
                                {VroleNameT, RoleIDList} ->
                                    lists:keyreplace(VroleNameT, 1, AccNameList2, {VroleNameT, [{ServerID, VroleID}|RoleIDList]})
                            end  
                    end, AccNameList, Rows)
          end, [], [MasterServerID|ServerIDList]),
    DuplicateList =
        lists:foldr(fun({_, RoleIDList} = R, Acc) ->
                            case erlang:length(RoleIDList) > 1 of
                                true ->
                                    [R | Acc];
                                false ->
                                    Acc
                            end
                    end, [], NameList),
    ?LOG("有重复情况的名字一共有~w个~n", [erlang:length(DuplicateList)]),
    ?LOG("~w~n", [DuplicateList]),
    RenameFix = do_merge:get(rename_fix),
    lists:foreach(
      fun({RoleName, RoleIDList}) ->
              lists:foreach(
                fun({ServerID, RoleID}) ->
                        NewRoleName = erlang:list_to_binary(erlang:binary_to_list(RoleName) ++ RenameFix ++ erlang:integer_to_list(ServerID)),
                        rename_role_name(RoleID, NewRoleName, ServerID, RoleName)
                end, RoleIDList)
      end, DuplicateList),
    ok.

del_tail_space(VroleName) when erlang:is_binary(VroleName) ->
    VroleNameList = erlang:binary_to_list(VroleName),
    NewVroleNameList = del_tail_space2(lists:reverse(VroleNameList), false),
    erlang:list_to_binary(NewVroleNameList).

del_tail_space2([], _) ->
    [];
del_tail_space2(List, true) ->
    lists:reverse(List);
del_tail_space2([H|List], false) ->
    case H of
        32 ->
            del_tail_space2(List, false);
        _ ->
            del_tail_space2([H|List], true)
    end.
    

%% 角色改名，更新gRole，gLimit
rename_role_name(RoleID, NewRoleName, ServerID, OldRoleName) ->
    MasterServerID = data_setting:get(server_id),
    PoolID =
        case ServerID =:= MasterServerID of
            true ->
                get_master_pool_id();
            false ->
                get_slave_pool_id(ServerID)
        end,
    GRoleSql = io_lib:format("update gRole set roleName= ~s where roleID = ~w;",[db_sql:quote(NewRoleName), RoleID]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, GRoleSql),
    GLimitSql = io_lib:format("update gLimit set inviteRoleName= ~s where inviteRoleName = ~s;",[db_sql:quote(NewRoleName), db_sql:quote(OldRoleName)]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, GLimitSql),
    ok.


do_start_log_server() ->
    case global:whereis_name(merge_log_server) of
        undefined ->
            PID = erlang:self(),
            erlang:spawn(fun() -> merge_log:start_log_server(merge_log_server, PID) end),
            erlang:spawn(fun() -> merge_log:start_log_server(behavior_serverlog, PID) end),
            %% 强制等待日志服务器启动完成
            receive 
                log_server_started ->
                    ok
            end;
        _PID ->
            ignore
    end.













