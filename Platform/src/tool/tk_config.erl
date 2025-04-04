%% @author admin
%% @doc 配置管理进程
%% Created 2013-2-19


-module(tk_config).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("preload_config.hrl").
-include("record.hrl").
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

%% 刷新服务器状态
-define(REFRESH_INTERVAL, 60*1000).

-record(state, {}).

refresh_once() ->
	erlang:send(?MODULE, refresh_once).

update_server_state(ServerID,ServerState) ->
	erlang:send(?MODULE, {update_state, ServerID,ServerState}).

%% @doc 获取服务器根目录
root_dir() ->
	filename:dirname(filename:dirname(element(2,code:is_loaded(?MODULE)))).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc gen_server:init/1
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
init([]) ->
	preload_config(),
	refresh_server_list(),
	erlang:send_after(?REFRESH_INTERVAL, self(), refresh_server_list),
    {ok, #state{}, hibernate}.

-define(state_stop, 1).%维护
-define(state_new, 2).%新
-define(state_crowd,3).%爆满
-define(state_busy,4).%拥挤
-define(state_fluency,5).%流畅

refresh_server_list() ->
	ServerIDList = data_server_list:get_list(),
	ServerList = [server_format(ID) || ID <- ServerIDList ],
	ets:insert(ets_server_list,{server_list,ServerList}).

server_format(ID) ->
	case erlang:get(ID) of
		?undefined ->
			ServerInfo = server_format2(ID,?state_stop),
			erlang:put(ID,ServerInfo),
			ServerInfo;
		ServerInfo when erlang:is_record(ServerInfo, server) ->
			ServerInfo
	end.

server_format2(ServerID, State) when is_integer(ServerID)->
	Server = data_server_list:get(ServerID),
	Server#server{state=State};
server_format2(Server, State) when erlang:is_record(Server, server)->
	Server#server{state=State}.

get_server_list(AccountType, ?undefined) ->
	get_server_list(AccountType, {0,0,0});
get_server_list(AccountType, {_,_,_}=Version) ->
	[{_,ServerList}] = ets:lookup(ets_server_list,  server_list),
	{DelServerIDList, ServerList2} = filter_test_server(ServerList, AccountType, Version),
	ListData =
		string:join([integer_to_list(State)++","++ServerName++","++integer_to_list(ServerID)++","++ServerIP++","++integer_to_list(ServerPort)
					   || #server{serverID=ServerID,serverIP=ServerIP,serverPort=ServerPort,serverName=ServerName,state=State} <- ServerList2], "\n"),
%% 	?ERR("~n~s~n", [ListData]),
	{DelServerIDList, list_to_binary(ListData)};
get_server_list(AccountType, Version) ->
%% 	P = re:split(Version, "[.]",[{return,list}]),
%% 	io:format("ll,~s,~w",[P,P]),
	[CMT,CST,CLT|_] = re:split(Version, "[.]",[{return,list}]),
	CM = erlang:list_to_integer(CMT),
	CS = erlang:list_to_integer(CST),
	CL = erlang:list_to_integer(CLT),
	get_server_list(AccountType, {CM,CS,CL}).


filter_test_server(ServerList, AccountType, Version) ->
	TestServerIDList = data_common:get(test_server_id_list),
	DelServerIDListT =
		lists:filter(fun(TestServerID) ->
							 case data_common:get({test_server, TestServerID}) of
								 ?undefined ->
									 true;
								 ConfigList ->
									 case lists:keyfind(AccountType, 1, ConfigList) of
										 false ->
											 true;
										 {AccountType, VersionList} ->
											 not lists:member(Version, VersionList)
							         end
						     end 
				     end, TestServerIDList),
    DateTime = erlang:localtime(),
    DelServerIDList =
        lists:foldr(fun(#server{serverID=ServerID}, Acc) ->
                            case lists:member(ServerID, Acc) of
                                true ->
                                    Acc;
                                false ->
                                    case data_common:get(ServerID) of
                                        ?undefined ->
                                            OpenDateTime = {{1970,1,1},{8,0,0}};
                                        OpenDateTime ->
                                            next
                                    end,
                                    case DateTime >= OpenDateTime of
                                        true ->
                                            Acc;
                                        false ->
                                            [ServerID|Acc]
                                    end
                            end
                    end, DelServerIDListT, ServerList),
	LeftServerList =
		lists:filter(fun(#server{serverID=ServerID}) ->
							 not lists:member(ServerID, DelServerIDList)
					 end, ServerList),
	{DelServerIDList, LeftServerList}.
		
%% @doc gen_server:handle_call/3
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.


%% @doc gen_server:handle_cast/2
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


%% @doc gen_server:handle_info/2
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_info({update_state, ServerID,ServerState}, State) ->
	case data_server_list:get(ServerID) of
		?undefined ->
			ignore;
		Server ->
			ServerInfo = server_format2(Server, ServerState),
			put(ServerID, ServerInfo),
			case ServerState of
				?state_stop ->
					refresh_server_list();
				_ ->
					refresh_server_list()
			end
	end,
	{noreply, State};
handle_info(refresh_server_list, State) ->
	erlang:send_after(?REFRESH_INTERVAL, self(), refresh_server_list),
	refresh_server_list(),
	{noreply, State};			
handle_info(refresh_once, State) ->
	refresh_server_list(),
	{noreply, State};
handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.

%% @doc gen_server:terminate/2
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
terminate(Reason, State) ->
	?ERR("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.


%% @doc gen_server:code_change/3
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 预加载配置
preload_config() ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	lists:foreach(fun({ConfigPath, Module, Type, TransformFun}) ->
							io:format("load config ~s\n",[ConfigPath]),
						  config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun);
					 (Fun) when is_function(Fun,1) ->
						  Fun(OutDir);
					 (Fun) when is_function(Fun,0) ->
						  Fun()					 
				  end, ?PRELOAD_CONFIG).


reload_config(data_server_list=Mod) ->
	reload_config2(Mod),
	refresh_once();
reload_config(Mod) ->
	reload_config2(Mod).


reload_config2(ConfigMod) ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	case lists:keyfind(ConfigMod, 2, ?PRELOAD_CONFIG) of
		false ->
			io:format("young boy ,this config is new，please try reload like this: tk_config:reload_config(\"config/data_skill.config\",data_skill,key_value,original).");
		{ConfigPath, Module, Type, TransformFun} ->
			config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun),
			c:l(Module)
	end.

reload_config(ConfigRelativePath, ModuleAtom, Type, TransformFun) ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	config2erl:beam(filename:join([RootDir, ConfigRelativePath]), ModuleAtom, OutDir, Type, TransformFun),
	c:l(ModuleAtom).

