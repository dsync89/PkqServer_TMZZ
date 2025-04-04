%% @author admin
%% @doc 常用命令模块
%% Created 2013-2-20


-module(user_default).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

num() ->
	ets:info(?ETS_ROLE_ONLINE,size).

test() ->
	?CATCH(erlang:error(certain_error)).

app(List) ->
	tk_misc:start_applications(List).

log(Term) ->
	?ERR("Log: ~1000000p",[Term]).

emu(RoleID, Msg) ->
	erlang:send(role_lib:gatewayRegName(RoleID),{emu, Msg}).

a(Tab) ->
	ets:tab2list(Tab).

a() ->
	RoleIDList = [E||{E,_}<-a(?ETS_ROLE_ONLINE)],
	First100 = lists:sublist(RoleIDList, 100),
	[begin
		 case catch ri(RoleID) of 
			 #role{roleName=Name} ->
				 show(Name),
				 io:format("\t\t~w\n",[RoleID]);
			 _ ->
				 io:format("error\t\t~w\n",[RoleID])
		 end
	 end || RoleID <- First100],
	io:format("\n").

refresh_onliners() ->
	RoleIDList = [E||{E,_}<-a(?ETS_ROLE_ONLINE)],
	[begin
		 case catch ri(RoleID) of 
			 #role{roleName=Name} ->
				 show(Name),
				 io:format("\t\t~w\n",[RoleID]);
			 _ ->
				 catch(erlang:unregister(role_lib:regName(RoleID))),
				 role_lib:leave_online_table(RoleID),
				 ets:delete(role_state,RoleID)
		 end
	 end || RoleID <- RoleIDList],
	RoleIDList2 = [E||{E,_}<-a(role_state)],
	[begin
		 case catch ri(RoleID) of 
			 #role{roleName=Name} ->
				 show(Name),
				 io:format("\t\t~w\n",[RoleID]);
			 _ ->
				 catch(erlang:unregister(role_lib:regName(RoleID))),
				 role_lib:leave_online_table(RoleID),
				 ets:delete(role_state,RoleID)
		 end
	 end || RoleID <- RoleIDList2],
	io:format("\n").

id() ->
	case ets:first(?ETS_ROLE_ONLINE) of
		'$end_of_table' ->
			0;
		ID->
			ID
	end.

s() ->
	tk:start().

stop() ->
	tk:stop().

t() ->
	role_battle:test().

l() ->
	tclient:login("dsfredsf").

%% 重新加载全部配置
lc()->
	tk_config:preload_config().
lc(A) ->
	tk_config:reload_config(A).
lc(A,B,C,D) ->
	tk_config:reload_config(A,B,C,D).
	
%% 重新生成协议
pg() ->
	RootDir = tk_config:root_dir(),
	proto_compile:scan_dir(filename:join([RootDir, "proto"]),filename:join([RootDir, "src"])).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 清除所有历史日志
cl() ->
	Dir = data_setting:get(logger_file_dir),
	filelib:fold_files(Dir, ".+", false, fun(E,_) -> io:format("~s\n",[E]),ok = file:delete(E) end, ok).

p(Name) when is_atom(Name) ->
	whereis(Name);
p(RoleID) when is_integer(RoleID) ->
	whereis(role_lib:regName(RoleID));
p(Pid) when is_pid(Pid) ->
	Pid.
	

d(P) ->
	element(2, process_info(p(P),dictionary)).
	
d(P,Key) ->
	Dict = element(2, process_info(p(P),dictionary)),
	case lists:keyfind(Key, 1, Dict) of
		false ->
			?undefined;
		{_, Value} ->
			Value
	end.



put(P, Key, Value) ->
	erlang:send(p(P), {func, fun erlang:put/2, [Key, Value]}).

i(P) ->
	gen_server:call(p(P), i).

ri(P) ->
	d(P, roleInfo).

kill(P) ->
	exit(p(P), kill).

%% 做xref分析
xref() ->
	List = xref:d(filename:join([tk_config:root_dir(), "ebin"])),
	{value, {undefined, UndefList}, List2} = lists:keytake(undefined, 1, List),
	UndefList2 = 
	lists:filter(fun(E) ->
						 EL = tuple_to_list(E),
						 Last = lists:last(EL),
						 if element(1, Last) == logger ->
								false;
							true ->
								true
						 end							 
				 end, UndefList),
	List2 ++ [{undefined,UndefList2}].
	

reload() ->
	LibDir = lib_dir(),
	%% 蛋疼的代码，在windows下，盘符，有时返回E:,有时返回e:
	[erlang:insert_element(2, c:l(E), E)||
	   {E,P}<-code:all_loaded(),P=/=preloaded, 
	   {P2,_} <-[filename:find_src(E)],
	   not(is_list(P2) andalso lists:prefix(LibDir, filename:dirname(P2)))].

lib_dir() ->
	{F, _Options} = filename:find_src(lists),
	filename:dirname(filename:dirname(filename:dirname(F))).

r(Mod) ->
	tk_misc:compile(Mod, tk_config:root_dir()),
	c:l(Mod).

show(Str0) ->
	Str = if is_binary(Str0) -> binary_to_list(Str0); true ->Str0 end,
	io:format("~ts",[unicode:characters_to_binary(Str,utf8,latin1)]).

repu(ID, Repu) ->
	role_lib:send_server(ID, {route, role_gm, {add_reputation, Repu}}).

ener(ID, Value) ->
	role_lib:send_server(ID, {route, role_gm, {set_energy, Value}}).
	
coin(ID, Coin) ->
	role_lib:send_server(ID, {route, role_gm, {add_coin, Coin}}).

gold(ID, Gold) ->
	role_lib:send_server(ID, {route, role_gm, {set_gold, Gold}}).

level(ID, Level) ->
	role_lib:send_server(ID, {route, role_gm, {set_level, Level}}).

refresh_today_task(RoleID)->
	role_lib:send_server(RoleID, {route,role_gm,{refresh_today_task}}).
	
add_ger(ID, GerTypeID) ->
	role_lib:send_server(ID, {route, role_gm, {add_ger, GerTypeID}}).

add_ger(ID, GerTypeID, Level, Rank) ->
	role_lib:send_server(ID, {route, role_gm, {add_ger2, GerTypeID, Level, Rank}}).



add_item(ID, ItemTypeID, Num) ->
	role_lib:send_server(ID, {route, role_gm, {add_item, ItemTypeID, Num}}).

add_item(ID, ItemTypeID, Num, Level, Rank) ->
	role_lib:send_server(ID, {route, role_gm, {add_item2, ItemTypeID, Num, Level, Rank}}).

vip(ID, VipLevel) ->
	role_lib:send_server(ID, {route, role_gm, {set_vip, VipLevel}}).
	

stack(P) ->
	process_info(p(P), current_stacktrace).

w(Term) ->
	file:write_file("temp.txt",io_lib:format("~p",[Term]),[write]).

refresh_energy(ID) ->
	role_lib:send_server(ID, {route, role_gm, {refresh_energy}}).
	
%% 刷点东西
shua(ID) ->
	coin(ID, 2000000000),
	gold(ID, 2000000000),
	vip(ID, 10),
	ener(ID, 30000),
	repu(ID, 2000000000),
	level(ID, 40),
	[add_item(ID,E,10)||E<-[11001,11001,11001]], 
	[add_ger(ID,E)||E<-data_ger:get_list(),E<12000],
	[add_ger(ID,E)||E<-data_ger:get_list(),E<12000],
	[add_ger(ID,E)||E<-data_ger:get_list(),E<12000],
	[add_ger(ID,E)||E<-data_ger:get_list(),E<12000].

shua2(ID) ->
	coin(ID, 2000000000),
	vip(ID, 10),
	ener(ID, 30000),
	repu(ID, 2000000000),
	level(ID, 100),
	[add_ger(ID,E,100,19)||E<-data_ger:get_list(),E<20000,is_star_bigger_than(E,3)].

shua3(ID) ->
	[add_item(ID,E,1,104,19)||E<-data_item:get_list(),
							  #data_item{itemType=ItemType,itemStar=ItemStar}<-[data_item:get(E)],
							  lists:member(ItemType,[weapon,armor,horse]) andalso ItemStar>=4].

	
is_star_bigger_than(E,Star) ->
	#data_ger{gerStar=ItemStar}=data_ger:get(E),
	ItemStar >= Star.
	
is_equip_star_bigger_than(E,Star) ->
	#data_item{itemStar=ItemStar}=data_item:get(E),
	ItemStar >= Star.

%% 将玩家的战斗相关数据设置为最强配置
best_fire(ID) ->
	F = fun(_GerID,GerTypeID) -> [E || E<- ger_lib:best_equip_for_ger(GerTypeID)] end,
	erlang:send(p(ID), {func, fun() -> 
									 PosList = role_data:get_posList(),
									 [begin 
										  put({gerEquip,E},F(E,GerTypeID)),
										  {value, #ger{gerBase=GerBase}=Ger,_,_}=role_data:get_ger(E),
										  _Ger2=Ger#ger{gerBase=GerBase#gerBase{gerLevel=100,gerQuality=19}},
										  ger_attr:recacl_f(E),
										  ok
									  end||#ger{gerID=E,gerBase=#gerBase{gerTypeID=GerTypeID}}<-PosList]
					   end, []}).

gc() ->
	lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, erlang:processes()).

z() ->
	make:all(),
	reload(),
	ok.

func(P, F, Args) ->
	gen_server:call(p(P), {func, F, Args}).

n() ->
	ets:info(?REG_JUDG_TABLE, size).



%% 编译并热更新某个模块
le(M) ->
	File = find_src(M),
	{ok,[{_F, Options}|_]} = file:consult(filename:join([tk_config:root_dir(),"Emakefile"])),
	compile:file(File, Options),
	c:l(M).

find_src(M) ->
	FileName = atom_to_list(M)++".erl",
	Dir = tk_config:root_dir(),
	filelib:fold_files(Dir, FileName, true, fun(E,_) ->
														  io:format("~s",[E]),
														  E end, 0).

rw(A) ->
	io:format("~w",[A]).

%%  测试命令
tbc(N) ->
	util:for(1, N, 
			 fun(_) ->
					 broadcast_server:bc_msg("a时天使天使时天使天使时天使天使时天使天使天时天使天使！！~") 
			 end).

ss(State) ->
	state_sync_server:sync_state(State).

purchase(RoleID,Num) ->
	List = [data_pay:get(E)||E<-data_pay:get_list()],
	case lists:keyfind(Num, #data_pay.payGold, List) of
		false ->
            Receipt = integer_to_list(RoleID)++integer_to_list(Num)++integer_to_list(util:now()),
            Md5 = md5(Receipt),
            gen_server:call(pay_server, {func, fun pay_server:do_pay_amount/5, [RoleID,Num,Receipt,Md5,0]});
		#data_pay{payID=PayID} ->
			Receipt = integer_to_list(RoleID)++integer_to_list(PayID)++integer_to_list(util:now()),
			Md5 = md5(Receipt),
			gen_server:call(pay_server, {func, fun pay_server:do_pay/5, [RoleID, PayID, Receipt, Md5,0]})
	end.

change_role_vip(RoleID, VipLevel) when VipLevel =< 16 ->
	case role_lib:is_online(RoleID) of
		true ->
			erlang:send(role_lib:pid(RoleID),{change_role_vip_level, VipLevel,ensure_action});
		_ ->
			Sql=io_lib:format("update gRole set vipLevel=~w where roleID = ~w;",[VipLevel,RoleID]),
			db_sql:sql_execute_with_log(Sql)
	end.

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

gg() ->
    get_gw_process_list().

get_gw_process_list() ->
    [X||X<-erlang:registered(),
        "gw" == lists:sublist(erlang:atom_to_list(X), 2),
        erlang:is_integer(catch erlang:list_to_integer(erlang:atom_to_list(X) -- "gw"))].

gr() ->
    get_role_process_list().

get_role_process_list() ->
    [X||X<-erlang:registered(),
        "role" == lists:sublist(erlang:atom_to_list(X), 4),
        erlang:is_integer(catch erlang:list_to_integer(erlang:atom_to_list(X) -- "role"))].

gf() ->
    get_family_process_list().

get_family_process_list() ->
    [X||X<-erlang:registered(),
        "family_" == lists:sublist(erlang:atom_to_list(X), 7),
        erlang:is_integer(catch erlang:list_to_integer(erlang:atom_to_list(X) -- "family_"))].

load_all_beam() ->
    {ok, BeamFileList} = file:list_dir("./ebin"),
    lists:foreach(fun(BeamFile) ->
                          BeamFile2 = lists:reverse(BeamFile),
                          case BeamFile2 of
                              "maeb." ++ BeamMod ->
                                  Mod = erlang:list_to_atom(lists:reverse(BeamMod)),
                                  io:format("~w~n", [Mod]),
                                  c:l(Mod);
                              _ ->
                                  io:format("File:~ts~n", [BeamFile2]),
                                  false
                          end
                  end, BeamFileList).




