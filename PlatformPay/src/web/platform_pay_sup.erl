%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Supervisor for the platform application.

-module(platform_pay_sup).
-include("common.hrl").
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
	?DEBUG("platform_pay_sup start_link "),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
	?DEBUG("platform_pay_sup upgrade0 "),
    {ok, {_, Specs}} = init([]),
	?DEBUG("platform_pay_sup upgrade1 "),
    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
	?DEBUG("platform_pay_sup upgrade2 "),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
	?DEBUG("platform_pay_sup upgrade3 "),
    Kill = sets:subtract(Old, New),
	?DEBUG("platform_pay_sup upgrade4 "),
	
    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),
	?DEBUG("platform_pay_sup upgrade5 "),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Web = web_specs(platform_pay_web),
	?DEBUG("platform_pay_sup init1 "),
    Processes = [Web],
	?DEBUG("platform_pay_sup init2 "),
    Strategy = {one_for_one, 10, 10},
	?DEBUG("platform_pay_sup init3 "),
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_specs(Mod) ->
    {Port1,IP1} = data_common:get(pay_port),
    WebConfig = [
                 {port1, [{ip,IP1},{port,Port1}]},
                 {docroot, platform_deps:local_path(["priv", "www"])}],
	?DEBUG("platform_pay_sup web_specs QueryList = ~p~n", [Mod]),
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.