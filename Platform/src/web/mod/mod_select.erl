%% @author admin
%% @doc 获取角色基本信息
%% Created 2013-2-26


-module(mod_select).
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
    platform_tool:return(Req, ""). 


%% ====================================================================
%% Internal functions
%% ====================================================================


