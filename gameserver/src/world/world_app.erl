%% @author admin
%% @doc application of world node
%% Created 2013-3-7


-module(world_app).
-behaviour(application).
-include("common.hrl").
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
    Return = 'world_sup':start_link(),
    random:seed(now()),
    case data_setting:get(is_cross_master) of
        false ->
            start_(broadcast_server),
            start_(state_sync_server),
            start_(pvp_server),
            start_(push_server),
            start_(nanm_server),
			start_(hula_server),
            start_(mail_server),
            start_(hist_server),
            start_(pay_server),
			start_(homestead_server),
			start_(enargy_server),
            start_(friend_server),
            start_(invite_server),
            start_(activity_server),
            start_(talk_server),
            start_(fire_server),
            start_(activityRank_server),
            start_(rebate_server),
            start_(family_manager_server),
            start_(race_server),
            start_(team_pk_server),
            start_(rule_server),
            start_(melee_server),
			start_(shop_server),
            start_(battle_rank_server),
            start_(rank_server),
            start_(festival_server),
            start_(etc_server);
        true ->
            start_(rule_master_server),
            start_(melee_master_server)
    end,
    start_(db_manage_server),
    Return.

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_(ServerName) ->
	?CATCH(ServerName:start()).

