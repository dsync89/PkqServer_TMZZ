%% @author admin
%% @doc 服务器状态推送
%% Created 2013-2-26


-module(mod_server_state).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 服务器状态推送
handle(Req) ->
	QueryString = Req:parse_qs(),
	%io:format("QS=~p\n",[QueryString]),
	ServerID = list_to_integer(proplists:get_value("serverID", QueryString)),
	ServerState = list_to_integer(proplists:get_value("state", QueryString)),	
	tk_config:update_server_state(ServerID,ServerState),
	platform_tool:return(Req, <<>>).


%% ====================================================================
%% Internal functions
%% ====================================================================


