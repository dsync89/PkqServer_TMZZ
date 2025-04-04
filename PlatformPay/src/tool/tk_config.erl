
-module(tk_config).
-compile(export_all).
-include("common.hrl").
-include("preload_config.hrl").
-include("record.hrl").


-export([]).


%% @doc 获取服务器根目录
root_dir() ->
	filename:dirname(filename:dirname(element(2,code:is_loaded(?MODULE)))).

%% @doc 预加载配置
preload_config() ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	lists:foreach(fun({ConfigPath, Module, Type, TransformFun}) ->
							io:format("load config ~s\n",[ConfigPath]),
						  config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun);
					 (Fun) when is_function(Fun,1) ->
						  Fun(OutDir);
					 (Fun) when is_function(Fun,0) ->
						  Fun()					 
				  end, ?PRELOAD_CONFIG).

reload_config(Mod) ->
	reload_config2(Mod).


reload_config2(ConfigMod) ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	case lists:keyfind(ConfigMod, 2, ?PRELOAD_CONFIG) of
		false ->
			io:format("young boy ,this config is new，please try reload like this: tk_config:reload_config(\"config/data_skill.config\",data_skill,key_value,original).");
		{ConfigPath, Module, Type, TransformFun} ->
			config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun),
			c:l(Module)
	end.

reload_config(ConfigRelativePath, ModuleAtom, Type, TransformFun) ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	config2erl:beam(filename:join([RootDir, ConfigRelativePath]), ModuleAtom, OutDir, Type, TransformFun),
	c:l(ModuleAtom).

