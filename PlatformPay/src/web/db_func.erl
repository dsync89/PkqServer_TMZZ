%% @author crimoon11
%% @doc @todo Add description to db_func.
-include("common.hrl").

-module(db_func).
-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================

sql_execute_with_log(Sql)	->
	case emysql:execute(?DB,Sql) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~s*****execute with err:~p,~s",[Sql,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~s*****execute with err:~p~n",[Sql,Exception])
	end.

sql_execute_with_log(Statement, Args)	->
	case emysql:execute(?DB,Statement,Args) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~p,~p*****execute with err:~p,~s",[Statement,Args,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~p,~p*****execute with err:~p~n",[Statement,Args,Exception])
	end.

get_all(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,List} ->
			List;
		_ ->
			[]
	end.

get_all(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			List;
		_ ->
			[]
	end.

get_row(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.

get_row(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.


check_pay_receipt_duplicate(Md5) ->
	Sql = io_lib:format("select count(*) from all_pay_log where receiptMd5='~s';",[Md5]),
	case get_row(Sql) of
		[0] ->
			true;
		_ ->
			false
	end.

add_pay_receipt(Md5,RoleID,Receipt,SrcType,Amount,Accid) ->
	Sql = io_lib:format("insert into all_pay_log values (null, ~w,~w,~w,~w,'~s',~w,~w,'~s','~s');", 
						[1, RoleID, Accid, Amount, datetime(erlang:localtime()), 1, SrcType,
						 Md5,Receipt]),
	sql_execute_with_log(Sql).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% 批量插入
make_sql_batch(Sql, Format, List) when List =/= []->
	Str = lists:foldl(fun(E,Acc) ->
							  ","++io_lib:format(Format,E)++Acc
					  end, ";", List),
	Sql++tl(Str).

to_term(Bin)->
	to_term(Bin,[]).
to_term(Bin, Default) ->
	case catch binary_to_term(Bin) of
		{'EXIT',_} ->
			Default;
		Term ->
			Term
	end.

to_bin(Term) ->
	util:to_hex(term_to_binary(Term)).

datetime({{A,B,C},{D,E,F}}) ->
	io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[A,B,C,D,E,F]).
date({A,B,C}) ->
	io_lib:format("~w-~.2.0w-~.2.0w",[A,B,C]).
time({A,B,C}) ->
	io_lib:format("~.2.0w:~.2.0w:~.2.0w",[A,B,C]).

bool2int(true)->
	1;
bool2int(false)->
	0.

int2bool(1)->
	true;
int2bool(0)->
	false.

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];	%% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).


log_pay_info(IsSucc, RoleID, AccID, Amount, PayTime, PayStatus, SrcType, Receipt)->
	Receipt2 = io_lib:format("~w", [Receipt]),
	Sql = io_lib:format("insert into all_pay_log values (null, ~w,~w,~w,~w,'~s',~w,~w,~s,~s);", 
						[IsSucc, RoleID, AccID, Amount, datetime(PayTime), PayStatus, SrcType,
						 quote(util:md5(Receipt2)),quote(Receipt2)]),
	sql_execute_with_log(Sql).
