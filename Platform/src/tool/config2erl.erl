%% @Author: admin
%% @doc 服务器配置生成，由config文件生成data_*模块
%% Created: 2012-8-14
-module(config2erl).

-include("common.hrl").
-compile(export_all).


%% 从erlang-term文件生成key-value或者if-clause的erl代码
%% erlang-term文件格式1：
%% {1,value1}. 
%% {2,value2}.
%%
%% erlang-term文件格式2：
%% {data_what, 1, value1}. 
%% {data_what, 2, value2}.
%%
%% 参数：输入文件，输出文件，生成类型， 配置转化函数
%% 生成代码提供接口：
%% get_list/0  获取所有的key
%% get/1       获取key对应的value

-spec file(SrcFile :: file:filename(), OutPutFile :: file:filename(), Type :: key_value | if_clause, TransformFun :: original | fun((term()) ->term)) ->
	ok.
file(SrcFile, OutPutFile, Type, TransformFun) ->
	case file:consult(SrcFile) of
		{ok, Terms} ->
			file2(Terms,OutPutFile, Type, TransformFun);
		Error ->
			?CRITICAL("read config file error=~w, file=~s",[Error, SrcFile])
	end.


%% @doc 将策划配置的TermList转化成程序需要的TermList，然后 将record List和key_value List标准化
unique(Terms0, TransformFun) ->
	Terms = 
		if is_function(TransformFun, 1) ->
			   TransformFun(Terms0);
		   true ->
			   Terms0
		end,
	KeyValList = 
		case lists:all(fun(E) -> tuple_size(E) =:= 2 end, Terms) of
			true ->
				Terms;
			false ->
				[{element(2, Tuple),Tuple} || Tuple <- Terms]
		end,
	Dict = dict:from_list(KeyValList),
	case dict:size(Dict) =:= length(KeyValList) of
		true ->
			next;
		false ->
			DuplicateList = KeyValList -- dict:to_list(Dict),
			throw({duplicate_config_key,DuplicateList})
	end,
	KeyValList.
	

file2(Terms0, OutPutFile, Type, TransformFun) ->
	Terms = unique(Terms0, TransformFun),
	KeysList = [Key || {Key,_Value} <- Terms],
	file:delete(OutPutFile),
	ModStr = filename:basename(OutPutFile, ".erl"),
	SrcCode = gen_src(ModStr, KeysList, Terms, Type),
	file:write_file(OutPutFile, SrcCode, [write, binary, {encoding, utf8}]),
	ok.


%% @todo key_value方式生成代码
gen_src(ModStr, KeysList, Terms, key_value) ->
    FunctionClause = lists:foldl(fun({Key, Value}, C) ->
                                lists:concat([C,lists:flatten(io_lib:format("get(~w) -> ~w;\n", [Key, Value]))])
                        end,
                        "",
                        Terms),
    StrList = lists:flatten(io_lib:format("     ~w\n", [KeysList])),
"
-module(" ++ ModStr ++ ").
-export([get_list/0,get/1]).

get_list()->"++ StrList ++".\n\n" ++ FunctionClause ++
"get(_Key) -> undefined.
";
%% @todo if_clause方式生成代码
gen_src(ModStr, KeysList, Terms, if_clause) ->
    IfClause = lists:foldl(fun({Key, Value}, C) ->
                                lists:concat([C,lists:flatten(io_lib:format("\t\tKey >=  ~w -> ~w;\n", [Key, Value]))])
                        end,
                        "",
                        lists:reverse(Terms)),
    StrList = lists:flatten(io_lib:format("     ~w\n", [KeysList])),
	Min = lists:flatten(io_lib:format("\nget_min()->~w.\n",[hd(KeysList)])),
	Max = lists:flatten(io_lib:format("\nget_max()->~w.\n",[lists:last(KeysList)])),

"
-module(" ++ ModStr ++ ").
-export([get_list/0,get/1,get_min/0,get_max/0]).

get_list()->"++ StrList ++".\n\n"++Min ++Max++ "get(Key) -> \n\tif\n" ++IfClause ++
"\t\ttrue ->undefined\n\tend.
".

%% 从erlang-term文件生成key-value或者if-clause的模块，并编译好的beam文件，将代码加载到内存
%% erlang-term文件格式1：
%% {1,value1}. 
%% {2,value2}.
%%
%% erlang-term文件格式2：
%% {data_what, 1, value1}. 
%% {data_what, 2, value2}.
%%
%% 参数：输入文件， 生成的模块名，生成类型， 配置转化函数
%% 生成代码提供接口：
%% get_list/0  获取所有的key
%% get/1       获取key对应的value
%% 从erlang-term文件生成编译好的beam文件，并将代码加载到内存
%% 参数：输入文件，模块原子名
-spec load(SrcFile :: file:filename(), ModAtom :: atom(), Type :: key_value | if_clause, TransformFun :: original | fun((term()) ->term)) ->
	{ok, CodeBin :: binary()} | {error, term()}.
load(SrcFile, ModAtom, Type, TransformFun) ->
	case file:consult(SrcFile) of
		{ok, Terms} ->
			load2(Terms, ModAtom, Type, TransformFun);
		Error ->
			?CRITICAL("read config file error=~w, file=~s",[Error, SrcFile])
	end.
load2(Terms0, ModAtom, Type, TransformFun) ->
	Terms = unique(Terms0, TransformFun),
	KeysList = [Key || {Key,_Value} <- Terms],
	ModStr = atom_to_list(ModAtom),
    try
        SrcCode = gen_src(ModStr, KeysList, Terms, Type),
		{ModAtom, CodeBin} = dynamic_compile:from_string( SrcCode ),
        code:load_binary(ModAtom, ModStr ++ ".erl", CodeBin),
        {ok, CodeBin}
    catch
        Type:Reason -> 
            Trace = erlang:get_stacktrace(), string:substr(erlang:get_stacktrace(), 1,200),
            ?CRITICAL("Error compiling ~1000000000p: Type=~w,Reason=~1000000000p,Trace=~1000000000p,~n", [ModAtom, Type, Reason,Trace ]),
			{error, {Type, Reason}}			
    end.

%% 生成beam文件
beam(SrcFile, ModAtom, OutDir, Type, TransformFun) ->
	case file:consult(SrcFile) of
		{ok, Terms} ->
			beam2(Terms, ModAtom, OutDir, Type, TransformFun);
		Error ->
			?CRITICAL("read config file error=~w, file=~s",[Error, SrcFile])
	end.
beam2(Terms0, ModAtom, OutDir, Type, TransformFun) ->
	Terms = unique(Terms0, TransformFun),
	KeysList = [Key || {Key,_Value} <- Terms],
	ModStr = atom_to_list(ModAtom),
    try
        SrcCode = gen_src(ModStr, KeysList, Terms, Type),
		{ModAtom, CodeBin} = dynamic_compile:from_string( SrcCode ),
		OutFile = filename:join([OutDir,ModStr++".beam"]),
		write_file(OutFile, CodeBin),
        {ok, CodeBin}
    catch
        Type:Reason -> 
            Trace = erlang:get_stacktrace(), string:substr(erlang:get_stacktrace(), 1,200),
            ?CRITICAL("Error compiling ~1000000000p: Type=~w,Reason=~1000000000p,Trace=~1000000000p,~n", [ModAtom, Type, Reason,Trace ]),
			{error, {Type, Reason}}			
    end.

write_file(File, Bin) ->
	FileNameTmp = File++".tmp",
	file:write_file(File, Bin, [write]),
	file:rename(FileNameTmp, File).