%% @author crimoon11
%% @doc @todo 


-module(gift_code).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-define(MAX_INTERGER, 2821109907455).
-define(GIFT_CODE_LEN, 10).
-define(GIFT_CODE_HEAD_LEN,2).
%% ====================================================================
%% Internal functions
%% ====================================================================

%% 生成新的类型的礼品码，写入数据库，并导出config/gift_code_list.txt
gen(Type,Num) ->
	random:seed(erlang:now()),
	Type2 = to_list(Type),
	OldCodeList = db_func:get_spec_type_gift_code_list(Type2),
	OldSets =
		lists:foldr(fun(OldCode, AccSets) ->
							sets:add_element(OldCode, AccSets)
					end, sets:new(), OldCodeList),
	CodeList = random_code(Num, OldSets, sets:new()),
	GiftCodeComplete = [Type2++E++"\n" || E<- CodeList],
	FileName = filename:join([tk_config:root_dir(), "config", Type++"gift_code_list.txt"]),
	file:write_file(FileName, GiftCodeComplete, [append]),
    DateTime = erlang:localtime(),
	db_func:insert_gift_code(Type2, CodeList, DateTime).

import_code(Type) ->
    Type2 = to_list(Type),
    FileName = filename:join([tk_config:root_dir(), "config", Type2++"gift_code_list.txt"]),
    {ok, Binary} = file:read_file(FileName),
    String = erlang:binary_to_list(Binary),
    List = string:tokens(String, "\n"),
    db_func:insert_gift_code(Type2, [E -- Type||E<-List], erlang:localtime()).

to_list(A) when is_binary(A) ->
	binary_to_list(A);
to_list(A) ->
	A.

draw_gift(Code, Accid, ServerID) ->
    %% 	?ERR("Code:~w, Accid:~w, ServerID:~w", [Code, Accid, ServerID]),
    Len = length(Code),
    if Len == 10 orelse Len == 9->
           {Header, Body} = lists:split(?GIFT_CODE_HEAD_LEN, Code),
           case data_gift:get(Header) of
               undefined ->
                   {false, 5};
               {Reward, CanUseServerList, LimitTypeList, {StartTime, EndTime}} ->
                   %% 				   ?ERR("Reward:~w, CanUseServerList:~w, LimitTypeList:~w", [Reward, CanUseServerList, LimitTypeList]),
                   case check_can_use_server(CanUseServerList, ServerID) of
                       true ->
							?ERR("lingqu jiangli  Reward:~w, CanUseServerList:~w, LimitTypeList:~w", [Reward, CanUseServerList, LimitTypeList]),
                           case string:equal(Header,"WY") orelse string:equal(Header,"TQ") orelse check_limit_type(Accid, LimitTypeList)  of
                               true ->
									?ERR("lingqu jiangli   11111  Reward:~w", [Reward]),
                                   DateTime = erlang:localtime(),
                                   case DateTime >= StartTime andalso DateTime =< EndTime of
                                       true ->
											?ERR("lingqu jiangli   222  Reward:~w", [Reward]),
                                           case db_func:get_gift_info(Header, Body) of
                                               [0] ->
													?ERR("lingqu jiangli   3333  Reward:~w", [Reward]),
                                                   db_func:update_gift_info(Header,Body, Accid),
                                                   {true,Reward};
                                               [A] when is_integer(A) ->
													?ERR("lingqu jiangli   4444  Reward:~w", [Reward]),
                                                   {false, 2};
                                               _ ->
													?ERR("lingqu jiangli   999  Reward:~w", [Reward]),
                                                   {false,3}
                                           end;
                                       false ->
											?ERR("lingqu jiangli 5555  Reward:~w", [Reward]),
                                           {false, 9}
                                   end;
                               false ->
                                   %% 								   ?ERR("Accid:~w, LimitTypeList:~w", [Accid, LimitTypeList]),
								   ?ERR("lingqu jiangli   666  Reward:~w", [Reward]),
                                   {false, 8}
                           end;
                       false ->
                           %% 						   ?ERR("ServerID:~w, Code:~w, CanUseServerList:~w", [ServerID, Code, CanUseServerList]),
						   ?ERR("lingqu jiangli  777  Reward:~w", [Reward]),
                           {false, 7}
                   end
           end;
       true ->
           {false, 4}
    end.

check_can_use_server([], _ServerID) ->
	true;
check_can_use_server(CanUseServerList, ServerID) ->
	lists:member(ServerID, CanUseServerList).

check_limit_type(_Accid, []) ->
	true;
check_limit_type(Accid, [Type|LimitTypeList]) ->
	case db_func:is_use_spec_type_gift(Accid, Type) of
		false ->
			check_limit_type(Accid, LimitTypeList);
		true ->
			false
	end.

random_code(0, _OldSets, Sets)->
	sets:to_list(Sets);
random_code(N, OldSets, Sets) ->
	Num = random_code2(OldSets, Sets),
	Sets2 = sets:add_element(Num, Sets),
	random_code(N-1, OldSets, Sets2).

random_code2(OldSets, Sets) ->
	Int = random:uniform(?MAX_INTERGER),
	Int36 = integer_to_list(Int,36),
	case sets:is_element(Int36, Sets) orelse sets:is_element(Int36, OldSets) orelse string:tokens(Int36, "1I0O") =/= [Int36] of
		true ->
			random_code2(OldSets, Sets);
		false ->
			Int36
	end.
		   
	