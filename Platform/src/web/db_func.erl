%% @author
%% @doc @todo Add description to db_func.
-include("common.hrl").

-module(db_func).
-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================

%% 获取日志表的表名,日志表按月来分表
get_log_table_name(TableName) when erlang:is_atom(TableName) ->
	{{Y, M, _}, _} = erlang:localtime(),
	lists:flatten(io_lib:format("~w_~w_~w", [TableName,Y,M])).

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

check_reward(Accid) ->
    case get_row(io_lib:format("select isDraw from reward where accountID = ~w and typeVal = 0", [Accid])) of
        [0] ->
            sql_execute_with_log(io_lib:format("update reward set isDraw = 1 where accountID = ~w and typeVal = 0", [Accid])),
            true;
        _ ->
            false
    end.

check_gold(Accid) ->
    case get_row(io_lib:format("select typeVal, isDraw from reward where accountID = ~w and typeVal > 0", [Accid])) of
        [Gold, 0] ->
            sql_execute_with_log(io_lib:format("update reward set isDraw = 1 where accountID = ~w and typeVal = ~w", [Accid, Gold])),
            {true, Gold};
        _ ->
            false
    end.

insert_login_log(AccID, ServerID) ->
    case ets:lookup(?ETS_SER_INFO,AccID) of
        [{AccID,ServerList,_}] ->
            if length(ServerList) =:= 4 ->
                   [S1,S2,S3,S4] = ServerList,
                   case ServerID of
                       S1 ->
                           ets:insert(?ETS_SER_INFO,{AccID,ServerList,util:now()});
                       S2 ->
                           ServerList2 = [ServerID,S1,S3,S4],
                           ets:insert(?ETS_SER_INFO,{AccID,ServerList2,util:now()}),
                           insert_login_log2(AccID, ServerList2);
                       S3 ->
                           ServerList2 = [ServerID,S1,S2,S4],
                           ets:insert(?ETS_SER_INFO,{AccID,ServerList2,util:now()}),
                           insert_login_log2(AccID, ServerList2);
                       S4 ->
                           ServerList2 = [ServerID,S1,S2,S3],
                           ets:insert(?ETS_SER_INFO,{AccID,ServerList2,util:now()}),
                           insert_login_log2(AccID, ServerList2);
                       _  ->
                           ServerList2 = [ServerID,S1,S2,S3],
                           ets:insert(?ETS_SER_INFO,{AccID,ServerList2,util:now()}),
                           insert_login_log2(AccID, ServerList2)
                   end;	
               true ->
                   ServerList2 = [ServerID | [ E || E<-ServerList,E=/=ServerID]],
                   ets:insert(?ETS_SER_INFO,{AccID,ServerList2,util:now()}),
                   case ServerList2 =/= ServerList of
                       true ->
                           insert_login_log2(AccID, ServerList2);
                       false ->
                           next
                   end
            end;
        [] ->
            ServerList2 = [ServerID],
            ets:insert(?ETS_SER_INFO,{AccID,ServerList2,util:now()}),
            insert_login_log2(AccID, ServerList2)
    end.

insert_login_log2(AccID, ServerList) ->
	case ServerList of
		[ServerID1, ServerID2, ServerID3, ServerID4] ->
			next;
		[ServerID1, ServerID2, ServerID3] ->
			ServerID4 = 0;
		[ServerID1, ServerID2] ->
			ServerID3 = 0,
			ServerID4 = 0;
		[ServerID1] ->
			ServerID2 = 0,
			ServerID3 = 0,
			ServerID4 = 0
	end,
	Sql = io_lib:format("replace into login_history values(~w,~w,~w,~w,~w);", [AccID, ServerID1, ServerID2, ServerID3, ServerID4]),
	sql_execute_with_log(Sql).

get_login_hist(AccID) ->
	case ets:lookup(?ETS_SER_INFO,AccID) of
        [{AccID,ServerList, _}]->
            ets:insert(?ETS_SER_INFO,{AccID,ServerList,util:now()}),
            ServerList;
		[] ->
			Sql = io_lib:format("select serverID1, serverID2, serverID3, serverID4 from login_history where accountID=~w", [AccID]),
			case sql_execute_with_log(Sql) of
				{ok,[R]} when erlang:is_list(R) ->
					R2 = lists:filter(fun(E) -> E =/= 0 end, R),
					ets:insert(?ETS_SER_INFO,{AccID,R2,util:now()}),
					R2;
				_ ->
                    ets:insert(?ETS_SER_INFO,{AccID,[],util:now()}),
					[]
			end
	end.

get_account_info(AccountName) ->
	case get_account_info_ets(AccountName) of
		[Accid,Password] ->
			[Accid,Password];
		_ ->
			Sql = io_lib:format("select accountID, password from account where accountName='~s' and type=1",[AccountName]),
			case sql_execute_with_log(Sql) of
				{ok,[[Accid2,Password2]]} ->
					ets:insert(?ETS_ACC_INFO,{AccountName,Accid2,Password2}),
					[Accid2,Password2];
				_Err ->
					% ?ERR("get accouont info:~p~n",[Err]),
					[]
			end
	end.

batch_add_loglogin([]) ->
	ok;
batch_add_loglogin(List) ->
	TableName = get_log_table_name(logLogin),
	DateTime = datetime(erlang:localtime()),
	ArgList = [[Accid, DateTime, SrcType] || {Accid, SrcType} <- List],
	Sql = make_sql_batch(io_lib:format("insert into ~s values", [TableName]),"(~w,'~s',~w)",ArgList),
	sql_execute_with_log(Sql).

batch_add_loglogout([]) ->
    ok;
batch_add_loglogout(List) ->
    TableName = get_log_table_name(logLogout),
    ArgList =[ [Accid rem ?AccidBase ,RoleID, quote(DevID), IP, datetime(DateTime),LastDuration,SrcType]
               ||{Accid, RoleID, DevID, IP, DateTime,LastDuration,SrcType} <-List],
    Sql = make_sql_batch(io_lib:format("insert into ~s values", [TableName]),"(~w,~w,~s,'~w', '~s','~w',~w)",ArgList),
    sql_execute_with_log(Sql).

add_loglogin(Accid, SrcType) ->
	TableName = get_log_table_name(logLogin),
    Sql = io_lib:format("insert into ~s values(~w,'~s',~w)",[TableName, Accid, datetime(erlang:localtime()), SrcType]),
    sql_execute_with_log(Sql).

create_account(SrcType,AccountName, Password, PhoneNumber, DevID, IP) ->
	Sql = io_lib:format("insert into account values(null,~w,'~s','~s','~s','~s',~s,~s)",[SrcType,AccountName, Password, PhoneNumber,datetime(erlang:localtime()), quote(DevID), quote(IP)]),
	sql_execute_with_log(Sql).

bind_account(Accid, NewAccount, NewPassword, PhoneNum) ->
	Sql = io_lib:format("update account set accountName='~s', password='~s',phoneNumber='~s' where accountID=~w;",[NewAccount,NewPassword,PhoneNum,Accid]),
	sql_execute_with_log(Sql).

change_account_password(Accid, NewPassword) ->
    Sql = io_lib:format("update account set password='~s' where accountID=~w;",[NewPassword,Accid]),
    sql_execute_with_log(Sql).

%%baidu相关接口
get_bd_accid2(Uin) ->
    Uin2="bd_"++Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s'",[Uin2]),
    get_row(Sql).

get_bd_accid(Uin, DevID, IP) ->
    Uin2="bd_"++Uin,
    case get_account_info_ets(Uin2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_bd_accid2(Uin) of
                [] ->
                    {ok,_} = create_bd_account(Uin, DevID, IP),
                    [Accid2] = get_bd_accid2(Uin),
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    [Accid2];                               
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    Result
            end
    end.
    

create_bd_account(Uin, DevID, IP) ->
    Uin2="bd_"++Uin,
    Sql = io_lib:format("insert into account values(null,~w,'~s','',null,'~s',~s,~s)", [?ACCOUNT_TYPE_BAIDU, Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%%要玩相关接口
get_yw_accid2(Uin) ->
    Uin2="yw_"++Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s'",[Uin2]),
    get_row(Sql).

get_yw_accid(Uin, DevID, IP) ->
    Uin2="yw_"++Uin,
    case get_account_info_ets(Uin2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_yw_accid2(Uin) of
                [] ->
                    {ok,_} = create_yw_account(Uin, DevID, IP),
                    [Accid2] = get_yw_accid2(Uin),
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    [Accid2];                               
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    Result
            end
    end.
    

create_yw_account(Uin, DevID, IP) ->
    Uin2="yw_"++Uin,
    Sql = io_lib:format("insert into account values(null,~w,'~s','',null,'~s',~s,~s)", [?ACCOUNT_TYPE_YAOWAN, Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).


%% 点乐相关接口
get_dianjoy_accid2(Uin) ->
    Uin2 = "dianjoy_" ++ Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s'", [Uin2]),
    get_row(Sql).

get_dianjoy_accid(Uin, DevID, IP) ->
    Uin2 = "dianjoy_" ++ Uin,
    case get_account_info_ets(Uin2) of
        [Accid, _Password] ->
            [Accid];
        _ ->
            case get_dianjoy_accid2(Uin) of
                [] ->
                    {ok, _} = create_dianjoy_account(Uin, DevID, IP),
                    [Accid2] = get_dianjoy_accid2(Uin),
                    ets:insert(?ETS_ACC_INFO, {Uin2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {Uin2, Accid2, ""}),
                    Result
            end
    end.

create_dianjoy_account(Uin, DevID, IP) ->
    Uin2 = "dianjoy_" ++ Uin,
    Sql = io_lib:format("insert into account values(null,63,'~s','',null,'~s',~s,~s)", [Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% 酷派相关接口  
get_coolpad_accid2(Uin) ->
    Uin2 = "coolpad_" ++ Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s'", [Uin2]),
    get_row(Sql).

get_coolpad_accid(Uin, DevID, IP) ->
    Uin2 = "coolpad_" ++ Uin,
    case get_account_info_ets(Uin2) of
        [Accid, _Password] ->
            [Accid];
        _ ->
            case get_coolpad_accid2(Uin) of
                [] ->
                    {ok, _} = create_coolpad_account(Uin, DevID, IP),
                    [Accid2] = get_coolpad_accid2(Uin),
                    ets:insert(?ETS_ACC_INFO, {Uin2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {Uin2, Accid2, ""}),
                    Result
            end
    end.

create_coolpad_account(Uin, DevID, IP) ->
    Uin2 = "coolpad_" ++ Uin,
    Sql = io_lib:format("insert into account values(null,64,'~s','',null,'~s',~s,~s)", [Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%%91相关接口
get_91_accid2(Uin) ->
    Uin2="91_"++Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s'",[Uin2]),
    get_row(Sql).

get_91_accid(Uin, DevID, IP) ->
    Uin2="91_"++Uin,
    case get_account_info_ets(Uin2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_91_accid2(Uin) of
                [] ->
                    {ok,_} = create_91_account(Uin, DevID, IP),
                    [Accid2] = get_91_accid2(Uin),
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    [Accid2];                               
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    Result
            end
    end.
    

create_91_account(Uin, DevID, IP) ->
    Uin2="91_"++Uin,
    Sql = io_lib:format("insert into account values(null,2,'~s','',null,'~s',~s,~s)", [Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).


%----91
get_91_accid2_sgz15_ios(Uin) ->
    Uin2="91_"++Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s'",[Uin2]),
    get_row(Sql).

get_91_accid_sgz15_ios(Uin,DevID, IP) ->
    Uin2="91_"++Uin,
    case get_account_info_ets(Uin2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_91_accid2_sgz15_ios(Uin) of
                [] ->
                    {ok,_} = create_91_account_sgz15_ios(Uin,DevID, IP),
                    [Accid2] = get_91_accid2_sgz15_ios(Uin),
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    [Accid2];                               
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    Result
            end
    end.
    

create_91_account_sgz15_ios(Uin,DevID,IP) ->
    Uin2="91_"++Uin,
    Sql = io_lib:format("insert into account values(null,18,'~s','',null,'~s',~s,~s)", [Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

get_91_accid2_sgz15_ard(Uin) ->
    Uin2="91_"++Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s'",[Uin2]),
    get_row(Sql).

get_91_accid_sgz15_ard(Uin, DevID, IP) ->
    Uin2="91_"++Uin,
    case get_account_info_ets(Uin2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_91_accid2_sgz15_ard(Uin) of
                [] ->
                    {ok,_} = create_91_account_sgz15_ard(Uin,DevID, IP),
                    [Accid2] = get_91_accid2_sgz15_ard(Uin),
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    [Accid2];                               
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    Result
            end
    end.
    

create_91_account_sgz15_ard(Uin,DevID, IP) ->
    Uin2="91_"++Uin,
    Sql = io_lib:format("insert into account values(null,19,'~s','',null,'~s',~s,~s)", [Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%%uc相关接口
get_uc_accid(Ucid, DevID,IP) ->
    Ucid2="uc_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_uc_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_uc_account(Ucid, DevID, IP),
                    [Accid2] = get_uc_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_uc_accid2(Ucid) ->
    Ucid2 = "uc_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=3",[Ucid2]),
    get_row(Sql). 

create_uc_account(Ucid, DevID,IP) ->
    Ucid2 = "uc_"++Ucid,
    Sql = io_lib:format("insert into account values(null,3,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%%------------END

%%dl相关接口
get_dl_accid2(Uin) ->
    Uin2="dl_"++Uin,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=4",[Uin2]),
    get_row(Sql).

get_dl_accid(Uin, DevID,IP) ->
    Uin2="dl_"++Uin,
    case get_account_info_ets(Uin2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_dl_accid2(Uin) of
                [] ->
                    {ok,_} = create_dl_account(Uin,DevID,IP),
                    [Accid2] = get_dl_accid2(Uin),
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    [Accid2];       
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Uin2,Accid2,""}),
                    Result
            end
    end.

create_dl_account(Uin, DevID,IP) ->
    Uin2="dl_"++Uin,
    Sql = io_lib:format("insert into account values(null,4,'~s','',null,'~s',~s,~s)", [Uin2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%%------------END

%%zz相关接口
get_zz_accid(Ucid, DevID, IP) ->
    Ucid2="zz_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_zz_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_zz_account(Ucid, DevID, IP),
                    [Accid2] = get_zz_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_zz_accid2(Ucid) ->
    Ucid2 = "zz_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=5",[Ucid2]),
    get_row(Sql). 

create_zz_account(Ucid, DevID,IP) ->
    Ucid2 = "zz_"++Ucid,
    Sql = io_lib:format("insert into account values(null,5,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%%------------END

%%360相关接口
get_360_accid(Ucid, DevID,IP) ->
    Ucid2="360_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_360_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_360_account(Ucid, DevID,IP),
                    [Accid2] = get_360_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_360_accid2(Ucid) ->
    Ucid2 = "360_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=6",[Ucid2]),
    get_row(Sql). 

create_360_account(Ucid, DevID, IP) ->
    Ucid2 = "360_"++Ucid,
    Sql = io_lib:format("insert into account values(null,6,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%%------------END


%% 豌豆荚登陆接口
get_wdj_accid(Ucid, DevID, IP) ->
    Ucid2="wdj_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_wdj_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_wdj_account(Ucid, DevID, IP),
                    [Accid2] = get_wdj_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_wdj_accid2(Ucid) ->
    Ucid2 = "wdj_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=7",[Ucid2]),
    get_row(Sql). 

create_wdj_account(Ucid, DevID,IP) ->
    Ucid2 = "wdj_"++Ucid,
    Sql = io_lib:format("insert into account values(null,7,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% END

%% 多酷登陆接口
get_dk_accid(Ucid, DevID,IP) ->
    Ucid2="dk_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_dk_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_dk_account(Ucid, DevID,IP),
                    [Accid2] = get_dk_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_dk_accid2(Ucid) ->
    Ucid2 = "dk_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=8",[Ucid2]),
    get_row(Sql). 

create_dk_account(Ucid, DevID,IP) ->
    Ucid2 = "dk_"++Ucid,
    Sql = io_lib:format("insert into account values(null,8,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% END

    
%% 小米登陆接口
get_mi_accid(Ucid, DevID,IP) ->
    Ucid2="mi_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_mi_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_mi_account(Ucid, DevID,IP),
                    [Accid2] = get_mi_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_mi_accid2(Ucid) ->
    Ucid2 = "mi_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=9",[Ucid2]),
    get_row(Sql). 

create_mi_account(Ucid, DevID,IP) ->
    Ucid2 = "mi_"++Ucid,
    Sql = io_lib:format("insert into account values(null,9,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% END

%% 安智登陆接口 
get_az_accid(Ucid, DevID,IP) ->
    Ucid2="az_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_az_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_az_account(Ucid, DevID,IP),
                    [Accid2] = get_az_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_az_accid2(Ucid) ->
    Ucid2 = "az_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=10",[Ucid2]),
    get_row(Sql). 

create_az_account(Ucid, DevID,IP) ->
    Ucid2 = "az_"++Ucid,
    Sql = io_lib:format("insert into account values(null,10,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% END

%% pp助手登录接口

get_pp_accid(PPID, DevID,IP)->
    PPID2 = "pp_" ++ PPID,
    case get_account_info_ets(PPID2) of
        [AccID, _Password] ->
            [AccID];
        _ ->
            case get_pp_accid2(PPID2) of
                [] ->
                    {ok, _} = create_pp_account(PPID2,DevID,IP),
                    [AccID2] = get_pp_accid2(PPID2),
                    ets:insert(?ETS_ACC_INFO, {PPID2, AccID2, ""}),
                    [AccID2];
                Result ->
                    [AccID2] = Result,
                    ets:insert(?ETS_ACC_INFO, {PPID2, AccID2,""}),
                    Result
            end
    end.

get_pp_accid2(PPID)->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type = 11;",[PPID]),
    get_row(Sql).

create_pp_account(PPID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 11, '~s' , '',  null, '~s',~s,~s);", [PPID, datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% END

%% 快用登录接口
get_ky_accid(KYID, DevID, IP) ->
    KYID2 = "ky_" ++ KYID,
    case get_account_info_ets(KYID2) of
        [AccID, _Passwd] ->
            [AccID];
        _ ->
            case get_ky_accid2(KYID2) of
                [] ->
                    {ok, _} = create_ky_account(KYID2, DevID, IP),
                    [AccID2] = get_ky_accid2(KYID2),
                    ets:insert(?ETS_ACC_INFO, {KYID2, AccID2, ""}),
                    [AccID2];
                Result ->
                    [AccID2] = Result,
                    ets:insert(?ETS_ACC_INFO, {KYID2, AccID2, ""}),
                    Result
            end
    end.

get_ky_accid2(KYID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 12;", [KYID]),
    get_row(Sql).

create_ky_account(KYID, DevID, IP) ->
    Sql = io_lib:format("insert into account values (null, 12, '~s', '', null, '~s', ~s,~s);", [KYID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% END

%% 卓然登录接口
get_zr_accid(Ucid,DevID,IP)->
    Ucid2="zr_"++Ucid,
    case get_account_info_ets(Ucid2) of
        [Accid,_Password] ->
            [Accid];
        _ ->
            case get_zr_accid2(Ucid) of 
                [] ->
                    {ok, _} = create_zr_account(Ucid,DevID,IP),
                    [Accid2] = get_zr_accid2(Ucid),
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{Ucid2,Accid2,""}),
                    Result
            end
    end.

get_zr_accid2(Ucid) ->
    Ucid2 = "zr_"++Ucid,
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=13",[Ucid2]),
    get_row(Sql). 

create_zr_account(Ucid, DevID,IP) ->
    Ucid2 = "zr_"++Ucid,
    Sql = io_lib:format("insert into account values(null,13,'~s','',null,'~s',~s,~s)", [Ucid2,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%%------------END

%% 天象互动安卓
get_txhd_ard_accid(Uid, DevID,IP) ->
    Uid2 = "txhd_ard_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Password] ->
            [Accid];
        _ ->
            case get_txhd_ard_accid2(Uid2) of
                [] ->
                    {ok,_} = create_txhd_ard_account(Uid2, DevID,IP),
                    [Accid2] = get_txhd_ard_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.

get_txhd_ard_accid2(Uid) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w", [Uid,?ACCOUNT_TYPE_TXHD_ARD]),
    get_row(Sql).

create_txhd_ard_account(Uid, DevID,IP) ->
    Sql = io_lib:format("insert into account values(null,~w,'~s','',null,'~s',~s,~s)", [?ACCOUNT_TYPE_TXHD_ARD,Uid,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% 天象互动安卓ext
get_txhd_ard_ext_accid(Uid, DevID,IP) ->
    Uid2 = "txhd_ard_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Password] ->
            [Accid];
        _ ->
            case get_txhd_ard_ext_accid2(Uid2) of
                [] ->
                    {ok,_} = create_txhd_ard_ext_account(Uid2, DevID,IP),
                    [Accid2] = get_txhd_ard_ext_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.

get_txhd_ard_ext_accid2(Uid) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w", [Uid,?ACCOUNT_TYPE_TXHD_ARD_EXT]),
    get_row(Sql).

create_txhd_ard_ext_account(Uid, DevID,IP) ->
    Sql = io_lib:format("insert into account values(null,~w,'~s','',null,'~s',~s,~s)", [?ACCOUNT_TYPE_TXHD_ARD_EXT,Uid,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%%------------END

%% 天象互动IOS
get_txhd_ios_accid(Uid, DevID,IP) ->
    Uid2 = "txhd_ios_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Password] ->
            [Accid];
        _ ->
            case get_txhd_ios_accid2(Uid2) of 
                [] ->
                    {ok,_} = create_txhd_ios_account(Uid2, DevID,IP),
                    [Accid2] = get_txhd_ios_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.

get_txhd_ios_accid2(Uid) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w", [Uid,?ACCOUNT_TYPE_TXHD_IOS]),
    get_row(Sql). 

create_txhd_ios_account(Uid, DevID,IP) ->
    Sql = io_lib:format("insert into account values(null,~w,'~s','',null,'~s',~s,~s)", [?ACCOUNT_TYPE_TXHD_IOS,Uid,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%%------------END

%% i_tools 登录接口
get_it_accid(ITID, DevID, IP) ->
    ITID2 = "it_" ++ ITID,
    case get_account_info_ets(ITID2) of
        [Accid, _Password] ->
            [Accid];
        _ ->
            case get_it_accid2(ITID2) of
                [] ->
                    {ok,_} = create_it_account(ITID2,DevID,IP),
                    [Accid2] = get_it_accid2(ITID2),
                    ets:insert(?ETS_ACC_INFO, {ITID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {ITID2, Accid2, ""}),
                    Result
            end
    end.

get_it_accid2(ITID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 14;",[ITID]),
    get_row(Sql).

create_it_account(ITID, DevID,IP)->
    Sql = io_lib:format("insert into account values (null, 14, '~s','',null,'~s',~s,~s);", [ITID, datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ----END

%% 同步推 登录接口
get_tbt_accid(LYID,DevID,IP)->
    LYID2 = "tbt_"++LYID,
    case get_account_info_ets(LYID2) of
        [Accid, _Password] ->
            [Accid];
        _ ->
            case get_tbt_accid2(LYID2) of
                [] ->
                    {ok,_} = create_tbt_account(LYID2,DevID,IP),
                    [Accid2] = get_tbt_accid2(LYID2),
                    ets:insert(?ETS_ACC_INFO, {LYID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {LYID2, Accid2, ""}),
                    Result
            end
    end.

get_tbt_accid2(LYID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 15;",[LYID]),
    get_row(Sql).

create_tbt_account(LYID,DevID,IP)->
    Sql = io_lib:format("insert into account values (null, 15, '~s','',null,'~s',~s,~s);", [LYID, datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ----END

%% 华为 登录接口
get_hw_accid(HWID,DevID,IP)->
    HWID2 = "hw_" ++ HWID,
    case get_account_info_ets(HWID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_hw_accid2(HWID2) of
                [] ->
                    {ok, _} = create_hw_account(HWID2,DevID,IP),
                    [Accid2] = get_hw_accid2(HWID2),
                    ets:insert(?ETS_ACC_INFO, {HWID2, Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {HWID2, Accid2, ""}),
                    Result
            end
    end.

get_hw_accid2(HWID)->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 16;", [HWID]),
    get_row(Sql).

create_hw_account(HWID,DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 16, '~s','',null, '~s',~s,~s);",[HWID, datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ---END

%% ---- sina ---
get_sina_accid(SinaID,DevID,IP)->
    SinaID2 = "sina_" ++ SinaID,
    case get_account_info_ets(SinaID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_sina_accid2(SinaID2) of
                [] ->
                    {ok, _} = create_sina_account(SinaID2,DevID,IP),
                    [Accid2] = get_sina_accid2(SinaID2),
                    ets:insert(?ETS_ACC_INFO, {SinaID2, Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {SinaID2, Accid2, ""}),
                    Result
            end
    end.

get_sina_accid2(SinaID)->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 20;", [SinaID]),
    get_row(Sql).

create_sina_account(SinaID,DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 20, '~s','',null, '~s',~s,~s);",[SinaID, datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END


%% ---- meizu ---
get_mz_accid(MzID, DevID,IP) ->
    MzID2 = "mz_" ++ MzID,
    case get_account_info_ets(MzID2) of
        [Accid, _Passwd] -> [Accid];
        _ ->
            case get_mz_accid2(MzID2) of
                [] ->
                    {ok, _} = create_mz_account(MzID2, DevID,IP),
                    [Accid2] = get_mz_accid2(MzID2),
                    ets:insert(?ETS_ACC_INFO, {MzID2, Accid2, ""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {MzID2, Accid2, ""}),
                    [Accid2]
            end
    end.

get_mz_accid2(MzID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [MzID, ?ACCOUNT_TYPE_MZ]),
    get_row(Sql).

create_mz_account(MzID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_MZ, MzID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 37wan ---
get_37wan_accid(ID37Wan, DevID,IP) ->
    ID37Wan2 = "37wan_" ++ ID37Wan,
    case get_account_info_ets(ID37Wan2) of
        [Accid, _Passwd] -> [Accid];
        _ ->
            case get_37wan_accid2(ID37Wan2) of
                [] ->
                    {ok, _} = create_37wan_account(ID37Wan2, DevID,IP),
                    [Accid2] = get_37wan_accid2(ID37Wan2),
                    ets:insert(?ETS_ACC_INFO, {ID37Wan2, Accid2, ""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {ID37Wan2, Accid2, ""}),
                    [Accid2]
            end
    end.

get_37wan_accid2(ID37Wan) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [ID37Wan, ?ACCOUNT_TYPE_37WAN]),
    get_row(Sql).

create_37wan_account(ID37Wan, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_37WAN, ID37Wan, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END


%% ---- 金山 ----
get_ks_accid(KsID, DevID,IP) ->
    KsID2 = "ks_" ++ KsID,
    case get_account_info_ets(KsID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_ks_accid2(KsID2) of
                [] ->
                    {ok,_} = create_ks_account(KsID2, DevID,IP),
                    [Accid2] = get_ks_accid2(KsID2),
                    ets:insert(?ETS_ACC_INFO, {KsID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {KsID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_ks_accid2(KsID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [KsID,?ACCOUNT_TYPE_KS]),
    get_row(Sql).

create_ks_account(KsID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_KS,KsID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END


%% ---- 爱思助手 ----
get_i4_accid(I4ID, DevID,IP) ->
    I4ID2 = "i4_" ++ I4ID,
    case get_account_info_ets(I4ID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_i4_accid2(I4ID2) of
                [] ->
                    {ok,_} = create_i4_account(I4ID2, DevID,IP),
                    [Accid2] = get_i4_accid2(I4ID2),
                    ets:insert(?ETS_ACC_INFO, {I4ID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {I4ID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_i4_accid2(I4ID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [I4ID,?ACCOUNT_TYPE_I4]),
    get_row(Sql).

create_i4_account(I4ID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_I4,I4ID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 爱苹果 ----
get_iiapple_accid(IIAppleID, DevID,IP) ->
    IIAppleID2 = "iiapple_" ++ IIAppleID,
    case get_account_info_ets(IIAppleID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_iiapple_accid2(IIAppleID2) of
                [] ->
                    {ok,_} = create_iiapple_account(IIAppleID2, DevID,IP),
                    [Accid2] = get_iiapple_accid2(IIAppleID2),
                    ets:insert(?ETS_ACC_INFO, {IIAppleID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {IIAppleID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_iiapple_accid2(IIAppleID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [IIAppleID,?ACCOUNT_TYPE_IIAPPLE]),
    get_row(Sql).

create_iiapple_account(IIAppleID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_IIAPPLE,IIAppleID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 优酷 ----
get_yk_accid(YkID, DevID,IP) ->
    YkID2 = "yk_" ++ YkID,
    case get_account_info_ets(YkID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_yk_accid2(YkID2) of
                [] ->
                    {ok,_} = create_yk_account(YkID2, DevID,IP),
                    [Accid2] = get_yk_accid2(YkID2),
                    ets:insert(?ETS_ACC_INFO, {YkID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {YkID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_yk_accid2(YkID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [YkID,?ACCOUNT_TYPE_YK]),
    get_row(Sql).

create_yk_account(YkID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_YK,YkID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END


%% ---- PPS ----
get_pps_accid(PpsID, DevID,IP) ->
    PpsID2 = "pps_" ++ PpsID,
    case get_account_info_ets(PpsID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_pps_accid2(PpsID2) of
                [] ->
                    {ok,_} = create_pps_account(PpsID2, DevID,IP),
                    [Accid2] = get_pps_accid2(PpsID2),
                    ets:insert(?ETS_ACC_INFO, {PpsID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {PpsID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_pps_accid2(PpsID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [PpsID,?ACCOUNT_TYPE_PPS]),
    get_row(Sql).

create_pps_account(PpsID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_PPS,PpsID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- all sdk---
get_all_sdk_accid(UID, AccountType, DevID,IP) ->
    UID2 = "sdk_" ++ UID,
    case get_account_info_ets(UID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_all_sdk_accid2(UID2, AccountType) of
                [] ->
                    {ok,_} = create_all_sdk_account(UID2, DevID, AccountType,IP),
                    [Accid2] = get_all_sdk_accid2(UID2, AccountType),
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_all_sdk_accid2(UID, AccountType) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [UID,AccountType]),
    get_row(Sql).

create_all_sdk_account(UID, DevID, AccountType,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [AccountType,UID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 4399 ----
get_4399_accid(UID, DevID,IP) ->
    UID2 = "4399_" ++ UID,
    case get_account_info_ets(UID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_4399_accid2(UID2) of
                [] ->
                    {ok,_} = create_4399_account(UID2, DevID,IP),
                    [Accid2] = get_4399_accid2(UID2),
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_4399_accid2(UID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [UID,?ACCOUNT_TYPE_4399]),
    get_row(Sql).

create_4399_account(UID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_4399,UID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- xy ----
get_xy_accid(UID, DevID,IP) ->
    UID2 = "xy_" ++ UID,
    case get_account_info_ets(UID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_xy_accid2(UID2) of
                [] ->
                    {ok,_} = create_xy_account(UID2, DevID,IP),
                    [Accid2] = get_xy_accid2(UID2),
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_xy_accid2(UID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [UID,?ACCOUNT_TYPE_XY]),
    get_row(Sql).

create_xy_account(UID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_XY,UID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 乐逗 ----
get_ld_accid(UID, DevID,IP) ->
    UID2 = "ld_" ++ UID,
    case get_account_info_ets(UID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_ld_accid2(UID2) of
                [] ->
                    {ok,_} = create_ld_account(UID2, DevID,IP),
                    [Accid2] = get_ld_accid2(UID2),
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_ld_accid2(UID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [UID,?ACCOUNT_TYPE_LD]),
    get_row(Sql).

create_ld_account(UID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_LD,UID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 益玩 ----
get_cw_accid(UID, DevID,IP) ->
    UID2 = "cw_" ++ UID,
    case get_account_info_ets(UID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_cw_accid2(UID2) of
                [] ->
                    {ok,_} = create_cw_account(UID2, DevID,IP),
                    [Accid2] = get_cw_accid2(UID2),
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_cw_accid2(UID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [UID,?ACCOUNT_TYPE_CW]),
    get_row(Sql).

create_cw_account(UID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_CW,UID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 谷果 ----
get_gg_accid(UID, DevID,IP) ->
    UID2 = "gg_" ++ UID,
    case get_account_info_ets(UID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_gg_accid2(UID2) of
                [] ->
                    {ok,_} = create_gg_account(UID2, DevID,IP),
                    [Accid2] = get_gg_accid2(UID2),
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {UID2,Accid2,""}),
                    [Accid2]
            end
    end.

get_gg_accid2(UID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [UID,?ACCOUNT_TYPE_GG]),
    get_row(Sql).

create_gg_account(UID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_GG,UID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END


%% -- LEVONO --
get_lenovo_accid(LUID,DevID,IP)->
    LUID2 = "lnvo_" ++ LUID,
    case get_account_info_ets(LUID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_lenovo_accid2(LUID2) of
                [] ->
                    {ok,_} = create_lenovo_account(LUID2, DevID,IP),
                    [Accid2] = get_lenovo_accid2(LUID2),
                    ets:insert(?ETS_ACC_INFO, {LUID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {LUID2, Accid2, ""}),
                    Result
            end
    end.

get_lenovo_accid2(LUID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 21",[LUID]),
    get_row(Sql).

create_lenovo_account(LUID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 21, '~s', '', null, '~s',~s,~s);", [LUID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ---END ---

%% oppo 登录
get_oppo_accid(OPID, DevID,IP)->
    OPID2 = "op_" ++ OPID,
    case get_account_info_ets(OPID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_oppo_accid2(OPID2) of
                [] ->
                    {ok, _} = create_oppo_account(OPID2, DevID,IP),
                    [Accid2] = get_oppo_accid2(OPID2),
                    ets:insert(?ETS_ACC_INFO, {OPID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {OPID2, Accid2, ""}),
                    Result
            end
    end.

get_oppo_accid2(OPID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 22",[OPID]),
    get_row(Sql).

create_oppo_account(OPID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 22, '~s', '', null, '~s',~s,~s);",[OPID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% --- END OPPO

%% --- sougo 登录

get_sogou_accid(SGID, DevID,IP) ->
    SGID2 = "sg_" ++ SGID,
    case get_account_info_ets(SGID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_sogou_accid2(SGID2) of
                [] ->
                    {ok,_} = create_sogou_account(SGID2, DevID,IP) ,
                    [Accid2] = get_sogou_accid2(SGID2),
                    ets:insert(?ETS_ACC_INFO, {SGID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {SGID2, Accid2, ""}),
                    Result
            end
    end.

get_sogou_accid2(SGID) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type = 23;", [SGID]),
    get_row(Sql).

create_sogou_account(SGID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 23, '~s', '', null, '~s',~s,~s);",[SGID, datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% --- END SOGOU

%% ----机锋登录--
get_jf_accid(JFID,DevID,IP)->
    JFID2 = "jf_" ++ JFID,
    case get_account_info_ets(JFID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_jf_accid2(JFID2) of
                [] ->
                    {ok,_} = create_jf_account(JFID2, DevID,IP),
                    [Accid2] = get_jf_accid2(JFID2),
                    ets:insert(?ETS_ACC_INFO, {JFID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {JFID2, Accid2, ""}),
                    Result
            end
    end.

get_jf_accid2(JFID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 24;",[JFID]),
    get_row(Sql).

create_jf_account(JFID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 24, '~s', '', null, '~s',~s,~s);",[JFID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ----END 机锋 ---

%% ----- 应用宝登录 ---

get_yyb_accid(YYBID, DevID,IP) ->
    YYBID2 = "yyb_" ++ YYBID,
    case get_account_info_ets(YYBID2) of
        [Accid, _passwd] ->
            [Accid] ;
        _ ->
            case get_yyb_accid2(YYBID2) of
                [] ->
                    {ok, _} = create_yyb_account(YYBID2, DevID,IP) ,
                    [Accid2] = get_yyb_accid2(YYBID2), 
                    ets:insert(?ETS_ACC_INFO, {YYBID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {YYBID2, Accid2, ""}),
                    Result
            end
    end.

get_yyb_accid2(YYBID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 25;", [YYBID]),
    get_row(Sql).

create_yyb_account(YYBID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 25, '~s', '', null, '~s', ~s,~s);", [YYBID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ----- YY 登录 ---

get_yy_accid(YYID, DevID,IP) ->
    YYID2 = "yy_" ++ YYID,
    case get_account_info_ets(YYID2) of
        [Accid, _passwd] ->
            [Accid] ;
        _ ->
            case get_yy_accid2(YYID2) of
                [] ->
                    {ok, _} = create_yy_account(YYID2, DevID,IP) ,
                    [Accid2] = get_yy_accid2(YYID2), 
                    ets:insert(?ETS_ACC_INFO, {YYID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {YYID2, Accid2, ""}),
                    Result
            end
    end.

get_yy_accid2(YYID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 62;", [YYID]),
    get_row(Sql).

create_yy_account(YYID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 62, '~s', '', null, '~s', ~s,~s);", [YYID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ---- END YY ----

%%  ----- 应用汇登录 -----
get_yyh_accid(YYHID, DevID,IP) ->
    YYHID2 = "yyh_" ++ YYHID,
    case get_account_info_ets(YYHID2) of
        [Accid, _passwd] ->
            [Accid];
        _ ->
            case get_yyh_accid2(YYHID2) of
                [] ->
                    {ok, _} = create_yyh_account(YYHID2, DevID,IP) ,
                    [Accid2] = get_yyh_accid2(YYHID2),
                    ets:insert(?ETS_ACC_INFO, {YYHID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {YYHID2, Accid2, ""}),
                    Result
            end
    end.

get_yyh_accid2(YYHID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 26;",[YYHID]),
    get_row(Sql).

create_yyh_account(YYHID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 26, '~s', '', null, '~s', ~s,~s);",[YYHID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%%　----- END 应用汇----- 


%% ---- 悠悠村uu ---
get_uu_accid(UuID, DevID, IP) ->
    UuID2 = "uu_" ++ UuID,
    case get_account_info_ets(UuID2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_uu_accid2(UuID2) of
                [] ->
                    {ok,_} = create_uu_account(UuID2, DevID,IP),
                    [Accid2] = get_uu_accid2(UuID2),
                    ets:insert(?ETS_ACC_INFO, {UuID2, Accid2, ""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {UuID2, Accid2, ""}),
                    [Accid2]
            end
    end.

get_uu_accid2(UuID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [UuID, ?ACCOUNT_TYPE_UU]),
    get_row(Sql).

create_uu_account(UuID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null, '~s',~s,~s);", [?ACCOUNT_TYPE_UU, UuID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END

%% ---- 奇天乐地ARD ---
get_qtld_ard_accid(Uid, DevID,IP) ->
    Uid2 = "qtld_ard_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_qtld_ard_accid2(Uid2) of
                [] ->
                    {ok,_} = create_qtld_ard_account(Uid2, DevID,IP),
                    [Accid2] = get_qtld_ard_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.

get_qtld_ard_accid2(Uid) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w;", [Uid, ?ACCOUNT_TYPE_QTLD_ARD]),
    get_row(Sql).

create_qtld_ard_account(Uid, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null,~w,'~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_QTLD_ARD, Uid, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END


%% ---- 易接 ---
get_yijie_accid(Uid, DevID,IP) ->
    Uid2 = "yijie_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_yijie_accid2(Uid2) of
                [] ->
                    {ok,_} = create_yijie_account(Uid2, DevID,IP),
                    [Accid2] = get_yijie_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.
	

get_yijie_accid2(Uid) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w;", [Uid, ?ACCOUNT_TYPE_YIJIE]),
    get_row(Sql).

create_yijie_account(Uid, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null,~w,'~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_YIJIE, Uid, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
	
%% ---END


%% ---- quick ---
get_quick_accid(Uid, DevID,IP) ->
    Uid2 = "quick_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_quick_accid2(Uid2) of
                [] ->
                    {ok,_} = create_quick_account(Uid2, DevID,IP),
                    [Accid2] = get_quick_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.
	
get_quickios_accid(Uid, DevID,IP) ->
    Uid2 = "quickios_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_quick_accid2(Uid2) of
                [] ->
                    {ok,_} = create_quick_account(Uid2, DevID,IP),
                    [Accid2] = get_quick_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.


get_quick_accid2(Uid) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w;", [Uid, ?ACCOUNT_TYPE_QUICK]),
    get_row(Sql).
	
create_quick_account(Uid, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null,~w,'~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_QUICK, Uid, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END



%% ---- 奇天乐地IOS ---
get_qtld_ios_accid(Uid, DevID,IP) ->
    Uid2 = "qtld_ios_" ++ Uid,
    case get_account_info_ets(Uid2) of
        [Accid, _Passwd] ->
            [Accid];
        _ ->
            case get_qtld_ios_accid2(Uid2) of
                [] ->
                    {ok,_} = create_qtld_ios_account(Uid2, DevID,IP),
                    [Accid2] = get_qtld_ios_accid2(Uid2),
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2];
                [Accid2] ->
                    ets:insert(?ETS_ACC_INFO, {Uid2,Accid2,""}),
                    [Accid2]
            end
    end.

get_qtld_ios_accid2(Uid) ->
    Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w;", [Uid, ?ACCOUNT_TYPE_QTLD_IOS]),
    get_row(Sql).

create_qtld_ios_account(Uid, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null,~w,'~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_QTLD_IOS, Uid, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).
%% ---END
 
%% ----- vivo 登录 -----
get_vivo_accid(VIVOID, DevID,IP) ->
    VIVOID2 = "vivo_" ++ VIVOID,
    case get_account_info_ets(VIVOID2) of
        [Accid, _passwd] ->
            [Accid];
        _ ->
            case get_vivo_accid2(VIVOID2) of
                [] ->
                    {ok, _} = create_vivo_account(VIVOID2, DevID,IP),
                    [Accid2] = get_vivo_accid2(VIVOID2),
                    ets:insert(?ETS_ACC_INFO, {VIVOID2, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO,{VIVOID2, Accid2, ""}),
                    Result
            end
    end.

get_vivo_accid2(VIVOID)->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 27;",[VIVOID]),
    get_row(Sql).

create_vivo_account(VIVOID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 27, '~s', '', null, '~s', ~s,~s);",[VIVOID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ---END VIVO

%% ----- QQ login

get_qq_accid(UID, DevID,IP)->
    TEUID = "qq_" ++ UID,
    case get_account_info_ets(TEUID) of
        [Accid, _P] ->
            [Accid];
        _ ->
            case get_qq_accid2(TEUID) of
                [] ->
                    {ok, _} = create_qq_account(TEUID, DevID,IP),
                    [Accid2] = get_qq_accid2(TEUID),
                    ets:insert(?ETS_ACC_INFO, {TEUID, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {TEUID, Accid2, ""}),
                    Result
            end
    end.

get_qq_accid2(TEUID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;",[TEUID, ?ACCOUNT_TYPE_QQ]),
    get_row(Sql).

create_qq_account(TEUID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s', '', null, '~s', ~s,~s);",[?ACCOUNT_TYPE_QQ, TEUID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ---- END QQ

%% ----- 微信 login

get_wx_accid(UID, DevID,IP)->
    TEUID = "wx_" ++ UID,
    case get_account_info_ets(TEUID) of
        [Accid, _P] ->
            [Accid];
        _ ->
            case get_wx_accid2(TEUID) of
                [] ->
                    {ok, _} = create_wx_account(TEUID, DevID,IP),
                    [Accid2] = get_wx_accid2(TEUID),
                    ets:insert(?ETS_ACC_INFO, {TEUID, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {TEUID, Accid2, ""}),
                    Result
            end
    end.

get_wx_accid2(TEUID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;",[TEUID, ?ACCOUNT_TYPE_WEIXIN]),
    get_row(Sql).

create_wx_account(TEUID, DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s', '', null, '~s', ~s,~s);",[?ACCOUNT_TYPE_WEIXIN, TEUID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ---- END 微信
 
%% 偶玩 login
get_ouw_accid(Uid, DevID,IP)->
    OWID = "ouw_"++Uid,
    case get_account_info_ets(OWID) of
        [Accid, _P] ->
            [Accid];
        _ ->
            case get_ouw_accid2(OWID) of
                [] ->
                    {ok, _} = create_ouw_account(OWID, DevID,IP),
                    [Accid2] = get_ouw_accid2(OWID),
                    ets:insert(?ETS_ACC_INFO, {OWID, Accid2,""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {OWID, Accid2, ""}),
                    Result
            end
    end.

get_ouw_accid2(OWID)->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 30;", [OWID]),
    get_row(Sql).

create_ouw_account(OWID, DevID,IP)->
    Sql = io_lib:format("insert into account values (null, 30, '~s', '', null, '~s', ~s,~s);", [OWID, datetime(erlang:localtime()), quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% -- END ouwan ---

%% -- 金立 登录----
get_jl_accid(Uid,DevID,IP) ->
    JLID = "jl_" ++ Uid,
    case get_account_info_ets(JLID) of
        [Accid, _P] ->
            [Accid];
        _ ->
            case get_jl_accid2(JLID) of
                [] ->
                    {ok, _} = create_jl_account(JLID,DevID,IP),
                    [Accid2] = get_jl_accid2(JLID),
                    ets:insert(?ETS_ACC_INFO, {JLID,Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {JLID,Accid2,""}),
                    Result
            end
    end.

get_jl_accid2(JLID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 34;", [JLID]),
    get_row(Sql).

create_jl_account(JLID,DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 34,'~s', '', null, '~s', ~s,~s);", [JLID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

% --- 金立 END -----
    
%% ---快播 account ------
get_kw_accid(Uid,DevID,IP) ->
    KWID = "kw_" ++ Uid,
    case get_account_info_ets(KWID) of
        [Accid, _P] ->
            [Accid];
        _ ->
            case get_kw_accid2(KWID) of
                [] ->
                    {ok, _} = create_kw_account(KWID,DevID,IP),
                    [Accid2] = get_kw_accid2(KWID),
                    ets:insert(?ETS_ACC_INFO, {KWID,Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {KWID,Accid2,""}),
                    Result
            end
    end.

get_kw_accid2(KWID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = 36;", [KWID]),
    get_row(Sql).

create_kw_account(KWID,DevID,IP) ->
    Sql = io_lib:format("insert into account values (null, 36,'~s', '', null, '~s', ~s,~s);", [KWID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
    sql_execute_with_log(Sql).

%% ---拇指玩 account----
get_mzw_accid(Uid, DevID,IP) ->
	MZWID = "mzw_" ++ Uid,
	case get_account_info_ets(MZWID) of
		[Accid, _P] ->
			[Accid];
		_ ->
			case get_mzw_accid2(MZWID) of
				[] ->
					{ok, _} = create_mzw_account(MZWID,DevID,IP),
					[Accid2] = get_mzw_accid2(MZWID),
					ets:insert(?ETS_ACC_INFO, {MZWID,Accid2,""}),
					[Accid2];
				Result ->
					[Accid2] = Result,
					ets:insert(?ETS_ACC_INFO, {MZWID,Accid2, ""}),
					Result
			end
	end.

get_mzw_accid2(MZWID) ->
	Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;",[MZWID,?ACCOUNT_TYPE_MZW]),
	get_row(Sql).

create_mzw_account(MZWID,DevID,IP) ->
	Sql = io_lib:format("insert into account values (null, ~w, '~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_MZW,MZWID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
	sql_execute_with_log(Sql).

%% ---51畅梦 account----
get_51cm_accid(Uid, DevID,IP) ->
	CMID = "51cm_" ++ Uid,
	case get_account_info_ets(CMID) of
		[Accid, _P] ->
			[Accid];
		_ ->
			case get_51cm_accid2(CMID) of
				[] ->
					{ok, _} = create_51cm_account(CMID,DevID,IP),
					[Accid2] = get_51cm_accid2(CMID),
					ets:insert(?ETS_ACC_INFO, {CMID,Accid2,""}),
					[Accid2];
				Result ->
					[Accid2] = Result,
					ets:insert(?ETS_ACC_INFO, {CMID,Accid2, ""}),
					Result
			end
	end.

get_51cm_accid2(CMID) ->
	Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;",[CMID,?ACCOUNT_TYPE_51CM]),
	get_row(Sql).

create_51cm_account(CMID,DevID,IP) ->
	Sql = io_lib:format("insert into account values (null, ~w, '~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_51CM,CMID,datetime(erlang:localtime()),quote(DevID),quote(IP)]),
	sql_execute_with_log(Sql).
% --- 51畅梦 END -----

%% ---木蚂蚁 account----
get_mmy_accid(Uid, DevId,IP) ->
	MMYID = "mmy_"++Uid,
	case get_account_info_ets(MMYID) of
		[Accid, _] ->
			[Accid];
		_ ->
			case get_mmy_accid2(MMYID) of
				[] ->
					{ok, _} = create_mmy_account(MMYID, DevId,IP),
					[Accid2] = get_mmy_accid2(MMYID),
					ets:insert(?ETS_ACC_INFO, {MMYID, Accid2, ""}),
					[Accid2];
				Result ->
					[Accid2] = Result,
					ets:insert(?ETS_ACC_INFO, {MMYID, Accid2, ""}),
					Result
			end
	end.

get_mmy_accid2(MMYID) ->
	Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [MMYID, ?ACCOUNT_TYPE_MMY]),
	get_row(Sql).

create_mmy_account(MMYID, DevId,IP) ->
	Sql = io_lib:format("insert into account values (null, ~w, '~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_MMY, MMYID, datetime(erlang:localtime()), quote(DevId),quote(IP)]),
	sql_execute_with_log(Sql).
% --- 木蚂蚁 END -----


% --- 好接入 ----------
get_hjr_accid(Uid, DevId, IP) ->
    HJRID = "hjr_" ++ Uid,
    case get_account_info_ets(HJRID) of
        [Accid, _] ->
            [Accid];
        _ ->
            case get_hjr_accid2(HJRID) of
                [] ->
                    {ok, _} = create_hjr_account(HJRID, DevId,IP),
                    [Accid2] = get_hjr_accid2(HJRID),
                    ets:insert(?ETS_ACC_INFO, {HJRID, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {HJRID, Accid2, ""}),
                    Result
            end
    end.

get_hjr_accid2(HJRID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [HJRID, ?ACCOUNT_TYPE_HJR]),
    get_row(Sql).

create_hjr_account(HMID, DevId,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_HJR, HMID, datetime(erlang:localtime()), quote(DevId),quote(IP)]),
    sql_execute_with_log(Sql).
% --- 好接入 END -----


%% ---海马  account----
get_hm_accid(Uid, DevId,IP) ->
    HMID = "hm_"++Uid,
    case get_account_info_ets(HMID) of
        [Accid, _] ->
            [Accid];
        _ ->
            case get_hm_accid2(HMID) of
                [] ->
                    {ok, _} = create_hm_account(HMID, DevId,IP),
                    [Accid2] = get_hm_accid2(HMID),
                    ets:insert(?ETS_ACC_INFO, {HMID, Accid2, ""}),
                    [Accid2];
                Result ->
                    [Accid2] = Result,
                    ets:insert(?ETS_ACC_INFO, {HMID, Accid2, ""}),
                    Result
            end
    end.

get_hm_accid2(HMID) ->
    Sql = io_lib:format("select accountID from account where accountName = '~s' and type = ~w;", [HMID, ?ACCOUNT_TYPE_HM]),
    get_row(Sql).

create_hm_account(HMID, DevId,IP) ->
    Sql = io_lib:format("insert into account values (null, ~w, '~s','',null,'~s',~s,~s);", [?ACCOUNT_TYPE_HM, HMID, datetime(erlang:localtime()), quote(DevId),quote(IP)]),
    sql_execute_with_log(Sql).
% --- 海马  END -----

get_gift_info(Header, Body) ->
	case string:equal(Header,"WY") orelse string:equal(Header,"TQ")  of
		false ->
			Sql = io_lib:format("select accountID from giftcode where type=~s and code=~s;",[quote(Header),quote(Body)]),
			get_row(Sql);
		true ->
			[0]
	end.

is_use_spec_type_gift(Accid, Header) ->
	Sql = io_lib:format("select count(*) from giftcode where accountID=~w and type=~s;", [Accid, quote(Header)]),
	case get_row(Sql) of
		[0] ->
			false;
		_ ->
			true
	end.

update_gift_info(Header, Body, Accid) ->
	Sql = io_lib:format("update giftcode set accountID=~w,getTime='~s' where type=~s and code=~s;",[Accid, datetime(erlang:localtime()),quote(Header),quote(Body)]),
	sql_execute_with_log(Sql).

%% 导入礼品码，每次导入1000条
insert_gift_code(Header, List, DateTime) ->
	Len = length(List),
	if Len > 1000 ->
		   {ListInsert,ListRest} = lists:split(1000, List),
		   batch_insert_gift_code(Header, ListInsert, DateTime),
		   insert_gift_code(Header, ListRest, DateTime);
	   true ->
		   batch_insert_gift_code(Header, List, DateTime)
	end.



batch_insert_gift_code(Header, ListInsert, DateTime) ->
	HeaderQuote = quote(Header),
	ArgList = [[HeaderQuote,quote(E),datetime(DateTime),datetime({{0,0,0}, {0,0,0}})] || E<- ListInsert],
	Sql = make_sql_batch("insert into giftcode values ","(~s,~s,0,'~s','~s')",ArgList),
	sql_execute_with_log(Sql).

get_spec_type_gift_code_list(Type) ->
	Sql = io_lib:format("select code from giftcode where type = '~s';", [Type]),
	case sql_execute_with_log(Sql) of
		{ok,CodeList} when erlang:is_list(CodeList) ->
			lists:map(fun(Code) ->
							  erlang:binary_to_list(Code)
					  end, lists:flatten(CodeList));
		Err ->
			?ERR("get_spec_type_gift_code_list, Err:~w~n",[Err]),
			erlang:throw({get_spec_type_gift_code_list, Err})
	end.
	
%% 用accid查询srctype
get_acctype(Accid) ->
	Sql = io_lib:format("select type from account where accountID = ~w",[Accid]),
	case sql_execute_with_log(Sql) of
		{ok,[[Type]]} ->
			Type;
		Err ->
			?ERR("get accouont info:~p,~p~n",[Accid,Err]),
			0
	end.




%% ====================================================================
%% Internal functions
%% ====================================================================


%% 批量插入
make_sql_batch(Sql, Format, List) when List =/= []->
	Str = lists:foldl(fun(E,Acc) ->
							  ","++io_lib:format(Format,E)++Acc
					  end, ";", List),
	Sql++tl(Str).

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
	util:to_hex(term_to_binary(Term)).

datetime({{A,B,C},{D,E,F}}) ->
	io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[A,B,C,D,E,F]).
date({A,B,C}) ->
	io_lib:format("~w-~.2.0w-~.2.0w",[A,B,C]).
time({A,B,C}) ->
	io_lib:format("~.2.0w:~.2.0w:~.2.0w",[A,B,C]).

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


%% ets 缓存account验证
get_account_info_ets(AccountName) ->
	case ets:lookup(?ETS_ACC_INFO,AccountName) of
		[{_,Accid,Password}] ->
			[Accid,Password];
		_ ->
			[]
	end.

log_create_role(Accid, RoleID) ->
    Sql = io_lib:format("insert into create_process_log values (~w,~w,'~s','~s','~s');",
                        [Accid, RoleID, datetime(erlang:localtime()), datetime({{0,0,0},{0,0,0}}), datetime({{0,0,0},{0,0,0}})]),
    sql_execute_with_log(Sql).

log_select_ger(Accid, RoleID) ->
    Sql = io_lib:format("update create_process_log set selectTime = '~s' where accid = ~w and roleID = ~w", [datetime(erlang:localtime()), Accid, RoleID]),
    sql_execute_with_log(Sql).

log_enter_game(Accid, RoleID) ->
    Sql = io_lib:format("update create_process_log set enterTime = '~s' where accid = ~w and roleID = ~w", [datetime(erlang:localtime()), Accid, RoleID]),
    sql_execute_with_log(Sql).

log_talk_gm(AccountID, RoleID, RoleName, TarAccountID, TarRoleID, TarRoleName) ->
    Sql = io_lib:format("insert into chatGM values(~w,~w,~s,~w,~w,~s,'~s')", [AccountID, RoleID, quote(RoleName), TarAccountID, TarRoleID, quote(TarRoleName),datetime(erlang:localtime())]),
    sql_execute_with_log(Sql).
