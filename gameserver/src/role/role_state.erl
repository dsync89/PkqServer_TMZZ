%% @author admin
%% @doc 处理玩家登录后注册与注销进程流程

-module(role_state).
-export([init/0, login/2, logoff_without_flush/1, logoff_with_flush/4]).
-compile(export_all).
-define(DUMP_TIME_OUT, 600).
-define(DUMP_WAIT_TIME, 200).
-include("common.hrl").
-compile(export_all).

init()	->
	ets:new(?REG_JUDG_TABLE, [public, named_table]),
	ok.

login(RoleID, Pid)	->
	login(RoleID, Pid, 0).

login(RoleID, Pid, N)	->
	case ets:lookup(?REG_JUDG_TABLE, RoleID) of
		[]	-> 
			ets:insert(?REG_JUDG_TABLE, {RoleID, 0}),
			case catch erlang:register(role_lib:regName(RoleID), Pid) of
                true ->
                    true;
                Reason ->
                    ?ERR("Reason:~w,RoleID:~w,Pid:~w", [Reason,RoleID,Pid]),
                    ets:delete(?REG_JUDG_TABLE,RoleID),
                    false
            end;
		_	->
			if 
				N > 20 ->
					false;
				true	->
					timer:sleep(200),
					login(RoleID, Pid, N+1)
			end
	end.

%% 每次unregister完,都将对应的ets表项删除,防止服务运行很久的时候ets表过大

%% 添加这个进程字典内容是因为，玩儿家进行pvp，则玩儿家id被lock，然后进行战斗计算，若在战斗计算过程中出错，未返回消息给pvp_server
%% 玩儿家将被一直lock，无法继续参加战斗。添加pvpsign，当玩家下线或断线等任一情况发生，role_server都会调用unlock接口清除状态
add_pvp_sign(RoleID, TarRoleID)->
	erlang:put({pvp_sign,RoleID}, TarRoleID).

clear_pvp_sign(RoleID)->
	pvp_server:unexpecte_unlock(RoleID),
	erlang:erase({pvp_sign, RoleID}).

logoff_without_flush(RoleID)	->
	erlang:unregister(role_lib:regName(RoleID)),
	role_lib:leave_online_table(RoleID),
	clear_pvp_sign(RoleID),
	hist_server:clear_hist(RoleID),
	ets:delete(?REG_JUDG_TABLE,RoleID).

logoff_with_flush(RoleID,Accid,DeviceID,SrcType)	->
	erlang:unregister(role_lib:regName(RoleID)),
	?CATCH(do_terminate(RoleID)),
	%% 异常保护,防止因为上面某流程失败导致死锁
	clear_pvp_sign(RoleID),
	hist_server:clear_hist(RoleID),
    catch erlang:send(race_server, {role_offline, RoleID}),
    catch erlang:send(rule_server, {role_offline, RoleID}),
    role_server:notice_family_online(false),
	catch homestead_server:homestead_role_offline(RoleID),
	catch etc_server:role_offline(RoleID, DeviceID),
    behavior_logout_log:log(Accid, RoleID, DeviceID, role_data:get_ip(), get_online_seconds(),SrcType),
	ets:delete(?REG_JUDG_TABLE,RoleID).

get_online_seconds() ->
    DurationT = util:datetime_to_seconds(erlang:localtime()) - util:datetime_to_seconds(role_data:get_login_datetime()),
    if
        DurationT > 0 ->
            DurationT;
        true ->
            0
    end.

do_terminate(RoleID) ->
	role_lib:leave_online_table(RoleID),
	%% 等待10ms，接受并处理最后的协议
	timer:sleep(10),
	role_server:flush_msg(RoleID),
	%% 持久化
	case is_stop_server() of
		true ->
			check_dump_num(RoleID),
			?CATCH(role_persist:persist_all_data()),
			ets:delete(?ETS_ROLE_DUMP, RoleID),
%% 			?ERR("RoleID:~w,Now:~w", [RoleID,util:now_mili()]),
			ok;
		false ->
			?CATCH(role_persist:persist_all_data())
	end.

is_stop_server() ->
    case catch ets:lookup(?ETS_CLOSE_FLAG, close) of
        [{close, true}] ->
            true;
        _ ->
            false
    end.



check_dump_num(RoleID) ->
    {_IP,_Port,_Name,_Pass,_DBName,MaxDumpNum} = data_setting:get(database),
	check_dump_num(RoleID, util:now(), MaxDumpNum).

check_dump_num(RoleID, Timestamp, MaxDumpNum) ->
	case ets:info(?ETS_ROLE_DUMP, size) < MaxDumpNum of
		true ->
%%             ?ERR("RoleID:~w,DumpNum:~w,Now:~w", [RoleID,ets:info(?ETS_ROLE_DUMP, size),util:now_mili()]),
			ets:insert(?ETS_ROLE_DUMP, {RoleID, 0});
		false ->
			timer:sleep(?DUMP_WAIT_TIME),
			check_dump_num(RoleID, Timestamp, MaxDumpNum)
	end.

