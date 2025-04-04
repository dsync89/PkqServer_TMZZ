%% @author admin
%% @doc web工具
%% Created 2013-2-26


-module(platform_tool).

%% API functions
-export([return/2]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
return(Req, Reply) ->
	%io:format("Reply=~w",[Reply]),
	Req:ok({"text/html; charset=utf-8",Reply}).

get_value(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{Key, Value} ->
			Value;
		false ->
			undefined
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================


