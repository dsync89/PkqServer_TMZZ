%% @author 董泉
%% @doc 获取服务器列表
%% Created 2014-3-10


-module(mod_server_list).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).

%% Internal functions
-export([]).



%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 获取角色基本信息
handle(Req) ->
	QueryString = Req:parse_qs(),
	Version = proplists:get_value("version", QueryString),
	SrcType = proplists:get_value("srctype", QueryString),
	if Version =:= ?undefined orelse SrcType =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,1}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   SrcType2 =
			   case erlang:is_list(SrcType) of
				   true ->
					   erlang:list_to_integer(SrcType);
				   false ->
					   SrcType
			   end,
		   {_DelServerIDList, ServerList} = tk_config:get_server_list(SrcType2, Version),
		   Reply = ejson:encode({[{<<"result">>,0},
								  {<<"server_list">>,ServerList}]}),
		   platform_tool:return(Req, Reply)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


