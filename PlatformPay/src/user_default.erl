%% @author admin
%% @doc 常用命令模块
%% Created 2013-2-20


-module(user_default).

-include("common.hrl").
-include("record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

test() ->
	?CATCH(erlang:error(certain_error)).

app(List) ->
	tk_misc:start_applications(List).

s() ->
    tk_config:reload_config(data_common),
	app([sasl,crypto,logger,tool,inets,emysql,platform_pay, asn1, public_key, ssl]).

%% ====================================================================
%% Internal functions
%% ====================================================================


