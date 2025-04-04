%% @author admin
%% @doc 通用接口
%% Created 2013-2-19


-module(tk_misc).
-compile(export_all).
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
		 role_procname/1
		]).

-export([random_role_name/1]).

%% 随机男性玩家名字
random_role_name(true) ->
	RandomFamilyNameSeq = random:uniform(data_male_family_name:get(max)),
	FamilyName = data_male_family_name:get(RandomFamilyNameSeq),
	RandomGivenNameSeq  = random:uniform(data_male_given_name:get(max)),
	GivenName = data_male_given_name:get(RandomGivenNameSeq),
	FamilyName++GivenName;
random_role_name(false) ->
	RandomFamilyNameSeq = random:uniform(data_female_family_name:get(max)),
	FamilyName = data_female_family_name:get(RandomFamilyNameSeq),
	RandomGivenNameSeq  = random:uniform(data_female_given_name:get(max)),
	GivenName = data_female_given_name:get(RandomGivenNameSeq),
	FamilyName++GivenName.

%% @doc 生成玩家逻辑进程的注册名称
role_procname(RoleID) ->
	list_to_atom("role"++integer_to_list(RoleID)).

send_sock_no_block(S, Record) when is_tuple(Record)->
    ?DEBUG("send record..~99999p",[Record]),
    case catch proto:encode(Record) of
        {'EXIT',Reason} ->
            ErrStr = io_lib:format("~1000p",[Record]), 
            exit({"cannot encode tuple=",ErrStr, Reason});
        Bin ->
            catch erlang:port_command(S, Bin, [nosuspend])
    end;
send_sock_no_block(S, Bin) ->
    ?DEBUG("send bin..~99999p",[Bin]),
    catch erlang:port_command(S, Bin, [nosuspend]).

send_sock(S, Record) when is_tuple(Record)->
	?DEBUG("send record..~99999p",[Record]),
	case catch proto:encode(Record) of
		{'EXIT',Reason} ->
			ErrStr = io_lib:format("~1000p",[Record]), 
			exit({"cannot encode tuple=",ErrStr, Reason});
		Bin ->
			%ok = 
				gen_tcp:send(S, Bin)
	end;
send_sock(S, Bin) ->
	?DEBUG("send bin..~99999p",[Bin]),
    %ok = 
		gen_tcp:send(S, Bin).

send_sock_force(S, Record) when is_tuple(Record)->
	?DEBUG("send record..~99999p",[Record]),
	case catch proto:encode(Record) of
		{'EXIT',Reason} ->
			ErrStr = io_lib:format("~1000p",[Record]), 
			exit({"cannot encode tuple=",ErrStr, Reason});
		Bin ->
			%erlang:port_command(S, Bin, [force])
			prim_inet:send(S,Bin,[nosuspend])
	end;
send_sock_force(S, Bin) ->
	%erlang:port_command(S, Bin, [force])
	prim_inet:send(S,Bin,[nosuspend]).

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

compile_options(RootDir) ->
	EmakefileName = filename:join([RootDir, "Emakefile"]),
	{ok, [{_,CompileOptions}|_]} = file:consult(EmakefileName),
	lists:map(fun({i,Dir}) ->
					  {i, filename:join([RootDir,Dir])};
				 ({outdir,Dir}) ->
					  {outdir, filename:join([RootDir, Dir])};
				 (E) ->
					  E
			  end, CompileOptions).
					  
	
find_file(Mod,Dir) ->
	FileName = atom_to_list(Mod)++".erl",
	filelib:fold_files(Dir, FileName, true, fun(E,_Acc) -> E end, undefined).

compile(Mod, RootDir) when is_atom(Mod) ->
	File = find_file(Mod, RootDir),
	compile:file(File, compile_options(RootDir)).
	
