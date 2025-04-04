
-module(pm_platform_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/0,add_loglogin/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_loglogin(Accid, SrcType) ->
	erlang:send(?MODULE, {login_again, Accid, SrcType}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {login_list=[], logout_list=[]}).
-define(DUMP_LOGIN_INTERVAL, 1000).

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
    global:register_name(data_common:get(platform_server), erlang:self()),
	erlang:send_after(?DUMP_LOGIN_INTERVAL, erlang:self(), dump_login),
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
handle_call({get_special_reward, Accid}, _From, State) ->
    Reply = check_special_reward(Accid),
    {reply, Reply, State};
handle_call({process_create, AccID, RoleID, Process}, _From, State) ->
    Reply = mod_create_process:log_create_process(RoleID, AccID, Process),
    {reply, Reply, State};
handle_call({gift, ServerID, ServerKey, GiftCode, Accid}, _From, State) ->
    Reply = mod_gift:check_gift(ServerID, ServerKey, GiftCode, Accid),
    {reply, Reply, State};
handle_call({update_state, ServerID, ServerState}, _From, State) ->
    tk_config:update_server_state(ServerID,ServerState),
    {reply, ok, State};
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
handle_info({update_login_history, Accid, ServerID}, State) ->
    case erlang:is_integer(Accid) andalso erlang:is_integer(ServerID) of
        true ->
            db_func:insert_login_log(Accid, ServerID);
        false ->
            next
    end,
    {noreply, State};
handle_info({logout_list, List}, #state{logout_list=LogoutList}=State) ->
    {noreply, State#state{logout_list=LogoutList++List}};
handle_info({login_again, Accid, SrcType}, #state{login_list=LoginList}=State) ->
    {noreply, State#state{login_list=[{Accid, SrcType}|LoginList]}};
handle_info(dump_login, #state{login_list=LoginList,logout_list=LogoutList}=State) ->
	erlang:send_after(?DUMP_LOGIN_INTERVAL, erlang:self(), dump_login),
	erlang:spawn(fun() -> ?CATCH(dump_login(LoginList, LogoutList)) end),
	{noreply, State#state{login_list=[],logout_list=[]}};
handle_info({talk_gm, AccountID, RoleID, RoleName, TarAccountID, TarRoleID, TarRoleName}, State) ->
    ?CATCH(db_func:log_talk_gm(AccountID, RoleID, RoleName, TarAccountID, TarRoleID, TarRoleName)),
    {noreply, State};
handle_info(_Info, State) ->
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
terminate(_Reason, #state{login_list=LoginList,logout_list=LogoutList}=_State) ->
    dump_login(LoginList, LogoutList),
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
dump_login(LoginList, LogoutList) ->
    db_func:batch_add_loglogin(LoginList),
    db_func:batch_add_loglogout(LogoutList).


check_special_reward(Accid) ->
    IsReward = db_func:check_reward(Accid),
    case db_func:check_gold(Accid) of
        {true, Gold} ->
            {true, IsReward, Gold};
        false ->
            {true, IsReward, 0}
    end.



    