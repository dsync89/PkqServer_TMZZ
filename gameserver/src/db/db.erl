%% @author admin
%% @doc	database operate interface
%% 抽象数据库层接口

-module(db).
-include("common.hrl").
-compile(export_all).
-export([]).

%% start() ->
%% 	mnesia:create_schema([]),
%% 	mnesia:start(),
%% 	init(),
%% 	mnesia:wait_for_tables(all_table(), infinity).
%% 
%% stop() ->
%% 	mnesia:stop().
%% 
%% init() ->
%% 	create_table().
%% 	%load_table().
%% 
%% %% @doc 创建表
%% create_table() ->
%% 	create_table(table_struct()).

%% @doc 读出表结构配置
table_struct() ->
	case file:consult(filename:join([tk_config:root_dir(), "config/app/table_struct.config"])) of
		{ok, [TableStruct]} ->
			TableStruct;
		{error, Reason} ->
			Error = io_lib:format("read config/table_struct.config error, Reason=~100p",[Reason]),
			?ERR(Error),
			[]
	end.

%% @doc 获取所有表
all_table() ->
	[TabName || {TabName, _TabType, _RecordName, _FieldsAndTypes} <- table_struct()].

%% %% @doc 初始化所有表
%% %% disk(磁盘表) ==>>  mnesia::disc_only_copies()
%% %% ram(内存表) ==>> 	mnesia::disc_copies()
%% create_table(TableStruct) ->
%% 	[begin
%% 		 Fields = [Field || {Field, _Type} <- FieldsAndTypes],
%% 		 %% 磁盘表
%% 		 case TabType of
%% 			 disk ->
%% 				 case mnesia:create_table(TabName, [{disc_only_copies, [node()]}, {attributes,Fields},{type,set},{record_name,RecordName}]) of
%% 					 {atomic, ok} ->
%% 						 ?INFO("创建表~-10w成功....",[TabName]);
%% 					 {aborted, {already_exists, _}} ->
%% 						 ignore;
%% 					 Error ->
%% 						 ?ERR("创建表~-10w失败...\n~200p",[TabName,Error])
%% 				 end;
%% 			 %% 内存表
%% 			 ram ->
%% 				 case mnesia:create_table(TabName, [{disc_copies, [node()]}, {attributes,Fields},{type,set},{record_name,RecordName}]) of
%% 					 {atomic, ok} ->
%% 						 ?INFO("创建表~-10w成功....",[TabName]);
%% 					 {aborted, {already_exists, _}} ->
%% 						 ignore;
%% 					 Error ->
%% 						 ?ERR("创建表~-10w失败...\n~200p",[TabName,Error])
%% 				 end
%% 		 end
%% 		
%% 	 end || {TabName, TabType, RecordName, FieldsAndTypes} <- TableStruct].
		 
connect_mysql() ->
	{IP,Port,Name,Pass,DBName,Num} = data_setting:get(database),
	emysql:add_pool(?DB, Num,Name,Pass, IP, Port,DBName, utf8).
