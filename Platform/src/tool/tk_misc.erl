%% @author admin
%% @doc 通用接口
%% Created 2013-2-19


-module(tk_misc).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
         manage_applications/6, 
         start_applications/1,  
         stop_applications/1
		 ]).

-export([
		 send_sock/2,
		 gen_roleID/0,
		 role_procname/1
		]).

gen_roleID() ->
	random:uniform(100000000).

%% @doc 生成玩家逻辑进程的注册名称
role_procname(RoleID) ->
	list_to_atom("role"++integer_to_list(RoleID)).

send_sock(S, Record) when is_tuple(Record)->
    gen_tcp:send(S, proto:encode(Record));
send_sock(S, Bin) when is_binary(Bin) ->
    gen_tcp:send(S, Bin).

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];
                        {error, {SkipError, _}} -> Acc;
                        {error, Reason} ->
                            lists:foreach(Undo, Acc),
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        already_started,
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).
