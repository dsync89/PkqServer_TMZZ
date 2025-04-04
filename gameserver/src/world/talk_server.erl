%% @author : admin
%% @doc : 聊天服务器，处理gm禁言信息

-module(talk_server).
-behaviour(gen_server).
-include("def_role.hrl").

-compile(export_all).

-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([i/0, is_gaged/1, gag_one/1]).

-define(DUMP_INTERVAL, 300).%写数据库的间隔,单位：秒
-define(TALK_LOG_INTERVAL, 5). %聊天记录写文件间隔
-define(RECENT_NUM, 50).
-define(CLEAR_PERSON_TALK_TIME, (3600 * 24 * 15)).

-define(person_talk_cache, person_talk_cache).

-record(state, {gag_list=[], recent_talk_data=[], talk_log=[]}).

is_gaged(RoleID)->
	gen_server:call(?MODULE, {is_gaged, RoleID}).

gag_one(RoleID)->
	erlang:send(?MODULE, {gag_one, RoleID}).

ungag_one(RoleID)->
	erlang:send(?MODULE, {ungag_one, RoleID}).

get_gag_list()->
	gen_server:call(?MODULE, get_gag_list).

i() ->
	gen_server:call(?MODULE, i).

start() ->
	{ok,_}=
	supervisor:start_child(world_sup, 
						   {?MODULE,
							{?MODULE, start_link, []},
							permanent, 600000, worker, [?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	random:seed(util:gen_random_seed()),
	State=get_db_info(),
    init_person_talk_cache(),
	process_flag(trap_exit,true),
	dump_tick(),
	erlang:send_after(?TALK_LOG_INTERVAL * 1000, self(), write_talk_log),
	{ok, State}.

handle_call(i, _From, State) ->
	{reply, State, State};
handle_call({is_gaged, RoleID},_From, State)->
	Result = lists:member(RoleID, State#state.gag_list),
	{reply, Result, State};
handle_call(get_gag_list,_From, State)->
	{reply, State#state.gag_list, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info({cache_talk_person,_RoleID,TarRoleID, Data}, State) ->
    save_person_talk(TarRoleID, Data),
    {noreply, State};
handle_info({get_talk_person_cache, RoleID}, State) ->
    send_person_talk(RoleID),
    {noreply, State};
handle_info({talk_channel_world, Data}, #state{recent_talk_data=RecentTalkData, talk_log=TalkLog}=State)->
	NewRecentTalkData = lists:sublist([Data|RecentTalkData], ?RECENT_NUM),
	NewTalkLog = [Data|TalkLog],
	{noreply, State#state{recent_talk_data=NewRecentTalkData, talk_log=NewTalkLog}};
handle_info({talk_channel_family, Data}, #state{talk_log=TalkLog}=State) ->
    NewTalkLog = [Data|TalkLog],
    {noreply, State#state{talk_log=NewTalkLog}};
handle_info({client_msg, RoleID, #cs_talk_recent_list{channel=Channel}}, #state{recent_talk_data=List}=State)->
    case Channel of
        ?CHAT_CHANNEL_WORLD ->
            ?unicast(RoleID, #sc_talk_recent_list{list=lists:reverse(List),channel=?CHAT_CHANNEL_WORLD});
        ?CHAT_CHANNEL_FAMILY ->
            case erlang:whereis(role_lib:regName(RoleID)) of
                ?undefined ->
                    ?unicast(RoleID, #sc_talk_recent_list{list=[],channel=?CHAT_CHANNEL_FAMILY});
                _ ->
                    role_lib:send_server(RoleID, get_family_recent_talk)
            end;
        _ ->
            next
    end,
    {noreply, State};
handle_info({client_msg, RoleID, #cs_talk_gm{tarRoleID=TarRoleID}}, State) ->
    catch cs_talk_gm(RoleID, TarRoleID),
    {noreply, State};
handle_info(write_talk_log, #state{talk_log=TalkLog}=State)->
	erlang:send_after(?TALK_LOG_INTERVAL * 1000, self(), write_talk_log),
	do_write_talk_data(TalkLog),
	{noreply, State#state{talk_log=[]}};
handle_info(dump_tick, State) ->
	do_write_db(State),
	dump_tick(),
    clear_old_person_talk_cache(),
	{noreply, State, hibernate};

handle_info({gag_one, RoleID}, #state{gag_list=GagList}=State)->
	GagList2 =
	case lists:member(RoleID, GagList) of
		true ->
			GagList;
		false ->
			[RoleID|GagList]
	end,
	{noreply, State#state{gag_list=GagList2}};
handle_info({ungag_one, RoleID}, #state{gag_list=GagList}=State)->
	GagList2 = lists:delete(RoleID, GagList),
	{noreply, State#state{gag_list=GagList2}};
handle_info(clear, State)->
    {noreply, State#state{recent_talk_data=[]}};
handle_info(_Info, State) ->
	%?CATCH(do_handle_info(Info)),
	{noreply, State}.

terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	do_write_db(State),
    dump_person_talk_cache(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

save_person_talk(TarRoleID, Data) ->
    case erlang:get({?person_talk_cache, TarRoleID}) of
        List when erlang:is_list(List) ->
            erlang:put({?person_talk_cache, TarRoleID}, [Data|List]);
        _ ->
            erlang:put({?person_talk_cache, TarRoleID}, [Data])
    end.

send_person_talk(RoleID) ->
    case erlang:get({?person_talk_cache, RoleID}) of
        List when erlang:is_list(List) ->
            erlang:erase({?person_talk_cache, RoleID}),
            ?unicast(RoleID, #sc_talk_person_offline{list=List});
        _ ->
            erlang:erase({?person_talk_cache, RoleID}),
            ?unicast(RoleID, #sc_talk_person_offline{list=[]})
    end.

dump_tick() ->
	erlang:send_after(?DUMP_INTERVAL*1000, self(), dump_tick).

init_person_talk_cache() ->
    List = db_sql:get_etc(?DB_ETC_KEY_PERSON_TALK),
    [erlang:put(Key, Val) || {Key, Val} <- List].

get_db_info() ->
	case db_sql:get_etc(?DB_ETC_KEY_TALK) of
		X when erlang:is_record(X, state) ->
			X;
		_ ->
			#state{}
	end.

do_write_db(State) ->
	db_sql:set_etc(?DB_ETC_KEY_TALK, State).

dump_person_talk_cache() ->
    List = [{{?person_talk_cache, RoleID}, Data} || {{?person_talk_cache, RoleID}, Data} <- erlang:get()],
    db_sql:set_etc(?DB_ETC_KEY_PERSON_TALK, List).

clear_old_person_talk_cache() ->
    Now = util:now(),
    [begin
         NewData = filter_old_data(Data, Now),
         case NewData of
             [] ->
                 erlang:erase({?person_talk_cache, RoleID});
             Data ->
                 ignore;
             _ ->
                 erlang:put({?person_talk_cache, RoleID}, NewData)
         end
     end || {{?person_talk_cache, RoleID}, Data} <- erlang:get()].

filter_old_data(Data, Now) ->
    [E||E<-Data, E#sc_talk_message.timeStamp + ?CLEAR_PERSON_TALK_TIME > Now].

do_write_talk_data([]) ->
	ok;
do_write_talk_data(DataList) ->
	NewDataList =
		[util:to_list(RoleName) ++ " " ++ Message ++ " " ++ erlang:integer_to_list(Title) ++ " " ++ erlang:integer_to_list(Timestamp)
			 ++ " " ++ erlang:integer_to_list(RoleID) ++ " " ++ util:to_list(Location) ++"\n"
		 ||#sc_talk_message{roleName=RoleName,message=Message,roleTitle=Title,timeStamp=Timestamp,roleID=RoleID,location=Location}<-lists:reverse(DataList)],
	FileName = make_log_file(),
	file:write_file(FileName, NewDataList, [append]).

%%生成日志文件名
make_log_file() ->
    {{Year, Month, Day}, {Hour, _, _}} = erlang:localtime(),
	LogDir = get_log_dir(),
    filename:join([LogDir, io_lib:format("talk_~p_~p_~p_~p", [Year, Month, Day, Hour])]).

get_log_dir() ->
	case file:consult(filename:join([tk_config:root_dir(),"setting","setting.config"])) of
		{ok, KVList} ->
			case lists:keyfind(logger_file_dir, 1, KVList) of
				false ->
					"/data/log";
				{_,Dir} ->
					Dir
			end;
		_ ->
			"/data/log"
	end.

cs_talk_gm(RoleID, TarRoleID) ->
    case catch check_talk_gm(RoleID, TarRoleID) of
        {true, Accid, RoleName, TarAccid, TarRoleName} ->
            ?CATCH(global:send(util:get_platform_server(), {talk_gm, Accid rem ?AccidBase, RoleID, RoleName, TarAccid rem ?AccidBase, TarRoleID, TarRoleName})),
            ?unicast(RoleID, #sc_talk_gm{result=0});
        {false, Reason} ->
            ?unicast(RoleID, #sc_talk_gm{result=Reason})
    end.

check_talk_gm(RoleID, TarRoleID) ->
    case erlang:get(RoleID) of
        ?undefined ->
            next;
        LastSeconds ->
            Now = util:now(),
            case Now - LastSeconds > data_common:get(chat_gm_interval) of
                true ->
                    erlang:put(RoleID, Now);
                false ->
                    erlang:throw({false, 1})
            end
    end,
    #rolePublic{accid=Accid, roleName=RoleName} = role_lib:get_rolePublic(RoleID),
    case role_lib:get_rolePublic(TarRoleID) of
        #rolePublic{accid=TarAccid, roleName=TarRoleName} ->
            {true, Accid, RoleName, TarAccid, TarRoleName};
        _ ->
            {false, 2}
    end.



