%% @author admin
%% @doc 内存表管理进程
%% Created 2013-2-19


-module(db_ram_server).
-behaviour(gen_server).
-include("common.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([dump/1]).


-record(state, {}).

%% @doc 持久化内存表的接口
dump(Tab) ->
	erlang:send(?MODULE, {dump, Tab}).

start() ->
    supervisor:start_child(tk_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 30000, worker, [?MODULE]}).

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
    {ok, #state{}}.


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
handle_info({dump, Table}, State) ->
	do_dump(Table),
	{noreply, State};
handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.

%% @doc gen_server:terminate/2 保存所有内存表
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
terminate(Reason, State) ->
	TableStruct = db:table_struct(),
	[begin
		 do_dump(Table),
		 ok
	 end || {Table, ram, _, _} <- TableStruct],
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

%% 持久化文件名称
fileName(Table) ->
	atom_to_list(Table) ++ ".ets".

%% @doc 持久化ets
do_dump(Table) ->
	case ets:tab2file(Table, fileName(Table)) of
		ok ->
			ok;
		Error ->
			?ERR("dump file error,Tab=~w,reason=~1000p",[Table, Error])
	end.
		  