%% author admin
%% 数据库清理进程
%% 定时创建日志表

-module(db_manage_server).
-include("common.hrl").
-include("log_create_sql.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).
-compile(export_all).

start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

-define(CREATE_LOG_TABLE_TIME, {4, 30, 0}).

-define(TICK_INTERVAL, (1000 * 5 * 60)).
-define(logout_list, logout_list).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	erlang:send_after(get_send_after_seconds(?CREATE_LOG_TABLE_TIME) * 1000, erlang:self(), create_log_table),
	create_log_table(),
    tick(),
    case db_sql:get_etc(?DB_ETC_KEY_LOGOUT) of
        [] ->
            next;
        List ->
            set_logout_list(List)
    end,
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
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
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({logout_list, List}, State) ->
    set_logout_list(get_logout_list() ++ List),
    {noreply, State};
handle_info(tick, State) ->
    tick(),
    case get_logout_list() of
        [] ->
            next;
        List ->
            case util:get_platform_server() of
                ?undefined ->
                    next;
                ServerName ->
                    ?CATCH(global:send(ServerName, {logout_list, List})),
                    set_logout_list([])
            end
    end,
    {noreply, State};
handle_info(create_log_table, State) ->
    erlang:send_after(get_send_after_seconds(?CREATE_LOG_TABLE_TIME) * 1000, erlang:self(), create_log_table),
    create_log_table(),
    {noreply, State};
handle_info(Info, State) ->
    ?INFO("unkown info:~w", [Info]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(Reason, _State) ->
    ?INFO("~w terminate, Reason:~w", [?MODULE, Reason]),
    case get_logout_list() of
        [] ->
            db_sql:set_etc(?DB_ETC_KEY_LOGOUT, []);
        List ->
            case util:get_platform_server() of
                ?undefined ->
                    db_sql:set_etc(?DB_ETC_KEY_LOGOUT, List);
                ServerName ->
                    ?CATCH(global:send(ServerName, {logout_list, List})),
                    db_sql:set_etc(?DB_ETC_KEY_LOGOUT, [])
            end
    end,
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_logout_list() ->
    case erlang:get(?logout_list) of
        ?undefined ->
            [];
        List ->
            List
    end.

set_logout_list(List) ->
    erlang:put(?logout_list, List).

tick() ->
    erlang:send_after(?TICK_INTERVAL, erlang:self(), tick).

get_next_timestamp(Time) ->
    {CurDate, CurTime} = erlang:localtime(),
    case Time > CurTime of
        false ->
            util:datetime_to_seconds({CurDate, Time}) + ?ONE_DAY_SECONDS;
        true ->
            util:datetime_to_seconds({CurDate, Time})
    end.

get_send_after_seconds(Time) ->
    NextTimestamp = get_next_timestamp(Time),
    CurTimestamp = util:now(),
    NextTimestamp - CurTimestamp.

get_next_year_and_month(Year, Month) ->
	case Month of
		12 ->
			{Year + 1, 1};
		 _ ->
			{Year, Month + 1}
	end.

create_log_table() ->
	{{Year, Month, Day}, _Time} = erlang:localtime(),
	create_log_table(Year, Month),
	%% 每个月的倒数第二天开始建下个月的日志表,如果建表失败能提前发现
	case Day >= calendar:last_day_of_the_month(Year, Month) - 1 of
		true ->
			{NextYear, NextMonth} = get_next_year_and_month(Year, Month),
			create_log_table(NextYear, NextMonth);
		false ->
			next
	end.

create_log_table(Year, Month) ->
	lists:foreach(fun(CeateSqlFormat) ->
						  CeateSql = io_lib:format(CeateSqlFormat, [Year, Month]),
%% 						  ?ERR("~s", [CeateSql])
						  db_sql:sql_execute_with_log(CeateSql)
				  end, ?CREATE_SQL_FORMAT_LIST).
