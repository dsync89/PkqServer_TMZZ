%% @author admin
%% @doc test edoc.
%% created 2013-2-18
-module(util).
-include("common.hrl").

-export([
		 log/5,
		 now/0,
		 now_mili/0,
		 md5/1,
		 rand/2,
		 ceil/1,
		 floor/1,
		 sleep/1,
		 sleep/2,
		 get_list/2,
		 implode/2,
		 implode/3,
		 explode/2,
		 explode/3,
		 for/3,
		 for/4,
		 string_to_term/1,
		 bitstring_to_term/1,
		 term_to_string/1,
		 term_to_bitstring/1,
		 to_string/1,
		 null_proc/0,
		 to_atom/1,
		 toSqlDate/1,
		 toUnixTime/1,
		 datetime_to_seconds/1,
		 seconds_to_datetime/1,
		 foldl/3,
		 nth_take/2,
		 fun_take/2,
		 fun_find/2,
		 fun_replace/3,
		 keymax/2,
		 keymin/2,
		 is_duplicate/1,
		 copy_list/2,
		 to_list/1,
		 append_kvList/2,
		 append_kvList/1,
		 nth/2,
		 element_pos/2,
		 random_int/2,
		 random_list/2,
		 random_list2/2,
		 random_list2/1,
		 random_weigh_list/2,
		 tc/2,
		 catch_binary_to_term/1,
		 ip_to_str/1
		]).

-export([sqlDayToDate/1
		,dateToSqlDay/1
		,int_format_two/1
		 ]).

-export([
		 read_timer/1
		]).

-export([
		 ets_foreach_key/2
		,ets_all_key/1
		 ]).

-export([
		 count_src_line_num/1
		 ]).
-record(constant, {data1 =calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}}) } ).
-define(GREGORIAN_INTERVIAL_TIME,  ((#constant{})#constant.data1)  ).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
	implode(S, L, []).
implode(_S, [H], NList) ->
	lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
	L = [thing_to_list(H) | NList],
	implode(S, T, [S | L]).

%% 字符->列
explode(S, B)->
	re:split(B, S, [{return, list}]).
explode(S, B, int) ->
	[list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 日志记录函数
%% log(T, F, A, Mod, Line) ->
%%     {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
%%     Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
%%     {{Y, M, D},{H, I, S}} = erlang:localtime(),
%%     Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
%%     io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
%%     file:close(Fl).    

log(_,_,_,_,_) ->
	ok.

%% @doc 取得当前的unix时间戳,单位：秒
now() ->
	{M, S, _} = erlang:now(),
	M * 1000000 + S.

%% @doc 当前时间戳，单位：毫秒
now_mili() ->
	{M, S, Ms} = erlang:now(),
	M * 1000000000 + S*1000 + Ms div 1000.

%% 转换成HEX格式的md5
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) ->
	%% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
	case get("rand_seed") of
		undefined ->
			RandSeed = mod_rand:get_seed(),
			random:seed(RandSeed),
			put("rand_seed", RandSeed);
		_ -> skip
	end,
	%% random:seed(erlang:now()),
	M = Min - 1,
	random:uniform(Max - M) + M.

%%向上取整
ceil(N) ->
	T = trunc(N),
	case N == T of
		true  -> T;
		false -> 1 + T
	end.

%%向下取整
floor(X) ->
	T = trunc(X),
	case (X < T) of
		true -> T - 1;
		_ -> T
	end.

sleep(T) ->
	receive
		after T -> ok
	end.

sleep(T, F) ->
	receive
		after T -> F()
	end.

get_list([], _) ->
	[];
get_list(X, F) ->
	F(X).

%% for循环
for(Max, Max, F) ->
	F(Max);
for(I, Max, F)  when I =< Max ->
	F(I),
	for(I+1, Max, F);
for(_I, _Max, _F) ->
	nil.

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
	binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
	erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
	case erl_scan:string(String++".") of
		{ok, Tokens, _} ->
			case erl_parse:parse_term(Tokens) of
				{ok, Term} -> Term;
				_Err -> undefined
			end;
		_Error ->
			undefined
	end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
	string_to_term(binary_to_list(BitString)).

to_string(Term) when is_atom(Term) ->
	atom_to_list(Term);
to_string(Term) when is_integer(Term) ->
	integer_to_list(Term);
to_string(Term) when is_list(Term) ->
	Term.

null_loop() ->
	receive 
		_ ->
			null_loop()
	end.

null_proc() ->
	spawn(fun null_loop/0).

to_atom(A) when is_list(A) ->
	list_to_atom(A);
to_atom(A) when is_integer(A) ->
	list_to_atom(integer_to_list(A));
to_atom(A) when is_atom(A) ->
	A;
to_atom(A) ->
	[List] = io_lib:format("~w", [A]),
	list_to_atom(List).

datetime_to_seconds({_Date,_Time}=Datetime)->
    calendar:datetime_to_gregorian_seconds(Datetime)
        - ?GREGORIAN_INTERVIAL_TIME.

seconds_to_datetime(MTime)->
    calendar:gregorian_seconds_to_datetime( 
      ?GREGORIAN_INTERVIAL_TIME+ MTime).

toSqlDate({A,B,_C}) when A < 1900->
	UnixTime = A *1000000 +B,
	toSqlDate(UnixTime);
toSqlDate({{A,B,C},{D,E,F}}) ->
	A*10000000000 + B*100000000 + C*1000000 + D*10000 + E*100 + F;
toSqlDate(UnixTime) when is_integer(UnixTime)->
	{{A,B,C},{D,E,F}} = seconds_to_datetime(UnixTime),
	A*10000000000 + B*100000000 + C*1000000 + D*10000 + E*100 + F.

int_format_two(A) ->
	lists:flatten(io_lib:format("~2..0w",[A])).

dateToSqlDay({A,B,C}) ->
	integer_to_list(A)++"-" ++ int_format_two(B) ++ "-" ++ int_format_two(C).

sqlDayToDate(String) ->
	case string:tokens(String, "-") of
		[A,B,C] ->
			{list_to_integer(A), list_to_integer(B), list_to_integer(C)};
		_ ->
			{0,0,0}
	end.

toUnixTime({A,B,_C}) ->
	A*1000000 + B;
toUnixTime({datetime, Time}) ->
	datetime_to_seconds(Time);
toUnixTime(SqlDate) ->
	A = SqlDate div 10000000000,
	Rest = SqlDate rem 10000000000,
	B = Rest div 100000000,
	Rest2 = Rest rem 100000000,
	C = Rest2 div 1000000,
	Rest3 = Rest2 rem 1000000,
	D = Rest3 div 10000,
	Rest4 = Rest3 rem 10000,
	E = Rest4 div 100,
	F = Rest rem 100,
	datetime_to_seconds({{A,B,C},{D,E,F}}).

		   
foldl(_F, {return,Acc}, _L) ->
    Acc;
foldl(F, Acc, [Tail|L]) ->
    foldl(F,F(Tail,Acc), L);
foldl(F, Acc, []) when is_function(F,2)->
    Acc.

to_list(A) when is_binary(A) ->
	binary_to_list(A);
to_list(A) when is_integer(A) ->
    integer_to_list(A);
to_list(A) when is_atom(A) ->
	atom_to_list(A);
to_list(A) when is_list(A) ->
	A.

append_kvList(List) ->
	lists:foldl(fun({K,V}, Acc) ->
						case lists:keytake(K, 1, Acc) of
							false ->
								[{K,V}|Acc];
							{value, {K,V1}, Acc1} ->
								[{K,V+V1}|Acc1]
						end
				end, [], List).

append_kvList(List1,List2) when length(List1) >= length(List2) ->
	append_kvList2(List2, List1);
append_kvList(List1, List2) ->
	append_kvList2(List1, List2).

append_kvList2([], List) ->
	List;
append_kvList2([KV | Rest], List) ->
	append_kvList2(Rest, insert_kv(KV, List)).

insert_kv({Key, Value}, List) ->
	case lists:keytake(Key, 1, List) of
		{value, {Key, OldValue}, List2} ->
			[{Key, OldValue+Value} | List2];
		false ->
			[{Key, Value} | List]
	end.

%% 超过范围返回最后一个，非列表时直接返回
nth(_N, [H]) ->H;
nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T);
nth(_N, H) ->
	H.


%% 从一个带权重的列表List中随机选取SelectNum个不同元素，结果列表顺序随机
%% weighed_list:  [{value1, weigh1}, {value2, weigh2}, {value3, weigh3}]
random_weigh_list(List, SelectNum) ->
	Len = length(List),
	TotalWeigh = get_total_weigh(List,0),
	if Len =< SelectNum ->
		   List;
	   true ->
		   random_weigh_list(List, SelectNum, TotalWeigh, [])
	end.


random_weigh_list(_List, 0, _, Result) ->
	Result;
random_weigh_list(List, SelectNum, TotalWeigh, Result) ->
	Random = random:uniform() * TotalWeigh,
	{_, Weigh} = Element = 
	foldl(fun({_, UnitWeigh}=Ele, Acc) ->
						if UnitWeigh + Acc > Random ->
							   {return, Ele};
						   true ->
							   Acc + UnitWeigh
						end
		  end, 0, List),
	NewList = lists:delete(Element, List),
	random_weigh_list(NewList, SelectNum -1, TotalWeigh-Weigh, [Element| Result]).


get_total_weigh([], Weigh) ->
	Weigh;
get_total_weigh([{_, WeighUnit} | Rest],Weigh) ->
	get_total_weigh(Rest, Weigh + WeighUnit).
	
	
%% 从列表List中随机选取SelectNum个元素，组成新的列表，新列表的元素排列顺序与其在List中顺序相同
random_list(List, SelectNum) ->
	Len = length(List),
	if Len =< SelectNum ->
		   List;
	   true ->
		   random_list(List, SelectNum, Len, [])
	end.


random_list(_, 0, _, Result) ->
	lists:reverse(Result);
random_list([Head| Rest], SelectNum, Len, Result) ->
	case random:uniform() =< SelectNum / Len of
		true ->
			random_list(Rest, SelectNum-1, Len-1, [Head|Result]);
		false ->
			random_list(Rest, SelectNum, Len-1, Result)
	end.
	
%% 将一个列表元素随机一遍
random_list2(List) ->
	Len = length(List),
	random_list2(List, Len, Len, []).
%% 从一个列表中随机抽取N个，顺序随机 ，N可以超过界限
random_list2(List, N) ->
	random_list2(List, N, length(List),[]).

random_list2(_List, 0, _Length, Result) ->
	Result;
random_list2(List, N, Length, Result) ->
	if Length =:= 1 ->
		   Select = hd(List),
		   Rest = [],
		   random_list2(Rest, N-1, Length-1, [Select|Result]);
	   Length =:= 0 ->
		   Result;
	   true ->
		   
		   Rand = random:uniform(Length),
		   {value, Select, Rest} = nth_take(Rand, List),		   
		   random_list2(Rest, N-1, Length-1, [Select|Result])
	end.

%% 计算一个元素在一个列表或tuple中第一次出现的位置
%% 元素在集合中不存在时，返回0
element_pos(Element, List) when is_list(List) ->
	element_pos(Element, List, 1);
element_pos(Element, Tuple) when is_tuple(Tuple) ->
	element_pos(Element, tuple_to_list(Tuple), 1);
element_pos(_, _) ->
	0.

element_pos(Element, [Element|_Rest], Index) ->
	Index;
element_pos(Element, [_|Rest], Index) ->
	element_pos(Element, Rest, Index+1);
element_pos(_, [], _) ->
	0.

%% 从[Lower...Higher]包括边界的整数区间中随机一个数
random_int(Lower, Higher) when Lower =< Higher ->
	random:uniform(Higher -Lower+1) +Lower-1;
random_int(Higher, Lower) ->
	random_int(Lower, Higher).

fun_take(F,L) ->
    fun_take(F, L, []).

fun_take(F, [H|T], L)  ->
	case F(H) of
		true ->
    		{value, H, lists:reverse(L, T)};
		false ->
    		fun_take(F, T, [H|L])
	end;
fun_take(_F, [], _L) -> false.

fun_find(_F,[]) ->
	false;
fun_find(F,[E|Rest]) ->
	case F(E) of
		true ->
			E;
		_ ->
			fun_find(F,Rest)
	end.


fun_replace(F, [Tup|Tail], New) ->
	case F(Tup) of
		true ->
    		[New|Tail];
		_ ->
			[Tup|fun_replace(F, Tail, New)]
	end;
fun_replace(_F, [], _) -> [].

%% 读timer
read_timer(Timer) when is_reference(Timer)->
	case erlang:read_timer(Timer) of
		false ->
			0;
		A ->
			A
	end;
read_timer(_) ->
	0.

%% 判断列表中是否有重复项
is_duplicate(List) ->
	is_duplicate(List, []).

is_duplicate([], _) ->
	false;
is_duplicate([H|T], List) ->
	case lists:member(H, List) of
		true ->
			true;
		false ->
			is_duplicate(T, [H|List])
	end.

%% 将列表复制N分，并组合成一个新的列表
copy_list(List,N) ->
	copy_list(List,N,[]).

copy_list(_List, 0, Result) ->
	Result;
copy_list(List, N, Result) ->
	copy_list(List, N-1, List++Result).

%% 找到tupleList中某字段最大的tuple
%% return: {MaxKey, Tuple}
keymax(List, Pos) ->
	[Head|Tail] = List,
	HeadKey = element(Pos, Head),
	lists:foldl(fun(E, {MaxKey, Tuple}) ->
						Key = element(Pos, E),
						if Key > MaxKey ->
							   {Key, E};
						   true ->
							   {MaxKey, Tuple}
						end
				end, {HeadKey, Head}, Tail).

%% 找到tupleList中某字段最大的tuple
%% return: {MinKey, Tuple}
keymin(List, Pos) ->
	[Head|Tail] = List,
	HeadKey = element(Pos, Head),
	lists:foldl(fun(E, {MaxKey, Tuple}) ->
						Key = element(Pos, E),
						if Key < MaxKey ->
							   {Key, E};
						   true ->
							   {MaxKey, Tuple}
						end
				end, {HeadKey, Head}, Tail).

%% 删除第N个，并返回新列表
%% return: {value, NthVar, NewList} | false
nth_take(N, List) ->
	nth_take(N, List, []).
nth_take(1, [NthVar|Tail], Temp) ->
	{value, NthVar, lists:reverse(Temp, Tail)};
nth_take(_N, [], _Temp) ->
	false;
nth_take(N, [Hd | Tail], Temp) ->
	nth_take(N-1, Tail, [Hd|Temp]).

%% 测试
tc(F, N) ->
    Time1 = erlang:now(),
    do_times(N, F),
    Time2 = erlang:now(),
    MicroDiffPerTime = timer:now_diff(Time2, Time1) / N,
    io:format("Times :~w ,  Each time consume: ~w us\n", [N, MicroDiffPerTime]).

do_times(N,F) when N =< 0 ->
    F();
do_times(N,F) ->
    F(),
    do_times(N -1, F).

%% 遍历ets的所有key
ets_foreach_key(Fun, Table) ->
    ets:safe_fixtable(Table, true),
    First = ets:first(Table),
    try
        do_ets_foreach_key(Fun, First, Table)
    after
	ets:safe_fixtable(Table, false)
    end.

do_ets_foreach_key(F, Key, Table) ->
    case Key of
	'$end_of_table' ->
	    ok;
	_ ->
	    F(Key),
		do_ets_foreach_key(F, ets:next(Table, Key), Table)
	end.
%% 获取ets所有key
ets_all_key(Table) ->
    ets:safe_fixtable(Table, true),
    First = ets:first(Table),
    try
        do_ets_all_key(First, Table, [])
    after
	ets:safe_fixtable(Table, false)
    end.

do_ets_all_key(Key, Table, Result) ->
    case Key of
	'$end_of_table' ->
	    Result;
	_ ->
		do_ets_all_key(ets:next(Table, Key), Table, [Key|Result])
	end.

catch_binary_to_term(Binary) ->
	if is_binary(Binary) ->
		   case catch binary_to_term(Binary) of
			   {'EXIT', _Reason} ->
				   [];
			   Term ->
				   Term
		   end;
	   true ->
		   ?ERR("wrong use of function util:catch_binary_to_term,when arg is not binary..~w",[Binary]),
		   []
	end.
	
%% 计算源码行数
count_src_line_num(Dir) ->
	count_src_line_num(Dir, [".+data_.+\.erl"]).
count_src_line_num(Dir, DisCardRegularExpList) ->
	count_src_line_num(Dir, DisCardRegularExpList, ".+\.erl$").
count_src_line_num(Dir, DisCardRegularExpList, MatchExp) ->
	{TotalNum, Info} = 
	filelib:fold_files(Dir, MatchExp, true, fun(FileName, {LineAcc, InfoList}) ->
													case lists:any(fun(E) ->
																		   case re:run(FileName, E) of
																			   {match, _} ->
																				   true;
																			   _ ->
																				   false
																		   end
																   end, DisCardRegularExpList) of
														true ->
															{LineAcc, InfoList};
														false ->
															ThisFileLineNum = cacl_file_line(FileName),
															io:format("~1000p ------ ~w\n",[filename:basename(FileName), ThisFileLineNum]),
															{LineAcc + ThisFileLineNum, [{ThisFileLineNum, FileName} |InfoList]}
													end
					   end, {0, []}),
	Info2 = lists:sort(Info),
	lists:foreach(fun({Lines, Name}) ->
															io:format("~1000p ------ ~w\n",[filename:basename(Name), Lines])
				  end, Info2),
	io:format("TotalNum=~w\n",[TotalNum]).
						  

cacl_file_line(FileName) ->
	{ok, Bin} = file:read_file(FileName),
	cacl_file_line2(binary_to_list(Bin),0, false).
cacl_file_line2([], Acc, true) ->
	Acc+1;
cacl_file_line2([], Acc, false) ->
	Acc;
cacl_file_line2([$\n|Rest], Acc, true) ->
	cacl_file_line2(Rest, Acc+1, false);
cacl_file_line2([$\n|Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_E|Rest], Acc, true) ->
	cacl_file_line2(Rest, Acc, true);
cacl_file_line2([$\r|Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, false);
cacl_file_line2([$\ |Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_|Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, true).

%% @doc convert IP(tuple) to string()
ip_to_str(IP) ->
    case IP of
        {A, B, C, D} ->
            lists:concat([A, ".", B, ".", C, ".", D]);
        {A, B, C, D, E, F, G, H} ->
            lists:concat([A, ":", B, ":", C, ":", D, ":", E, ":", F, ":", G, ":", H]);
        Str when is_list(Str) ->
            Str;
        _ ->
            []
    end.

-define(random_str, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-++_[]{};,.?<>").
-define(random_str2, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").
random_str(N) ->
	random:seed(erlang:now()),
	Len = length(?random_str),
	[lists:nth(random:uniform(Len),?random_str)||_<-lists:seq(1,N)].

random_str2(N) ->
	random:seed(erlang:now()),
	Len = length(?random_str2),
	[lists:nth(random:uniform(Len),?random_str2)||_<-lists:seq(1,N)].

bin_to_hexstr(Bin) ->
   lists:flatten([io_lib:format("~2.16.0B", [X]) ||
                  X <- binary_to_list(Bin)]).
 
hexstr_to_bin(S) ->
   hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
   list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
   {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
   hexstr_to_bin(T, [V | Acc]).
	
	