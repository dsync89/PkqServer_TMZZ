%% @author admin
%% @doc 服务器所有ID生成相关接口
%% Alarm : 请确保合服时不用修改ID
%% Created 2013-3-1


-module(tk_id).
-include("common.hrl").
-compile(export_all).
%% API functions
-export([gen_roleID/0]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
init() ->
	init_id(roleID,"roleID","gRole",robot_roleID_max()),
	init_id(gerID, "gerID","gGer",?GER_ID_BASE),
    init_id(familyID, "familyID", "gFamily", ?FAMILY_ID_BASE),
	ItemID1 = init_id2("itemUID", "gBagItem", ?ITEM_ID_BASE),
	ItemID2 = init_id2("itemUID", "gEquip", ?ITEM_ID_BASE),
	ItemID3 = erlang:max(ItemID1,ItemID2),
	ets:insert(?ETS_ID, {itemUID,ItemID3}),
	%% MailUID 表的最大值
	MailUID1 = init_id2("mailUID", "gMail", ?MAIL_ID_BASE),
	MailUID2 = init_id2("histUID", "gHist", ?MAIL_ID_BASE),
	ReplayUIDPVP = init_id2("replayUID", get_replay_table_name(?REPLAY_TYPE_PVP), ?REPLAY_ID_BASE),
	ReplayUIDRULE = init_id2("replayUID", get_replay_table_name(?REPLAY_TYPE_RULE), ?REPLAY_ID_BASE),
	MailUID3 = lists:max([MailUID1, MailUID2, ReplayUIDPVP, ReplayUIDRULE]),
	ets:insert(?ETS_ID, {mailUID, MailUID3}),
    lists:foreach(fun(ReplayType) ->
                     init_id(get_replay_id_name(ReplayType), "replayUID", get_replay_table_name(ReplayType), ?REPLAY_ID_BASE)  
                  end, ?REPLAY_TYPE_LIST_EXCEPT_PVP_RULE).

get_replay_table_name(ReplayType) ->
    io_lib:format("gReplay~w", [ReplayType]).

get_replay_id_name(ReplayType) ->
    erlang:list_to_atom(lists:flatten(io_lib:format("replayUID~w", [ReplayType]))).

init_id(IDName, Key,Table,Base) ->
	ID = init_id2(Key,Table,Base),
	ets:insert(?ETS_ID, {IDName, ID}).

init_id2(Key,Table,Base) ->
	Sql = io_lib:format("select max(~s) from ~s;",[Key,Table]),
	case db_sql:get_row(Sql) of
		[Max] when is_integer(Max)->
			next;
		_ ->
			Max=0
	end,
	erlang:max(Max, Base).
	


%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc 判断是否是机器人ID
is_robot(RoleID) ->
	RoleID < robot_roleID_max() andalso RoleID > robot_roleID_min().

%% @doc 最大的机器人ID，要求生成的机器人的roleID，都小于这个ID
robot_roleID_max() ->
	?ROLE_ID_BASE + 10000.

robot_roleID_min() ->
    ?ROLE_ID_BASE.
	
%% @doc 排行榜初始帐号对应的RoleID
rank_roleID(Rank) ->
	?ROLE_ID_BASE + Rank.
	
%% @doc 生成玩家ID,请注意，务必大于10000，否则会与pvp中的rank重复
gen_roleID() ->
	ets:update_counter(?ETS_ID, roleID, 1).

%% @doc 生成武将iD
gen_gerID() ->
	ets:update_counter(?ETS_ID, gerID, 1).

%% @doc 生成联盟ID
gen_familyID() ->
    ets:update_counter(?ETS_ID, familyID, 1).

%% @doc 生成道具ID
gen_itemUID() ->
	ets:update_counter(?ETS_ID, itemUID, 1).

%% @doc 生成邮件UID
gen_mailUID() ->
	ets:update_counter(?ETS_ID, mailUID, 1).

%% @doc 生成战斗录像UID
gen_replayUID(?REPLAY_TYPE_PVP) ->
    ets:update_counter(?ETS_ID, mailUID, 1);
gen_replayUID(?REPLAY_TYPE_RULE) ->
    ets:update_counter(?ETS_ID, mailUID, 1);
gen_replayUID(ReplayType) ->
    case lists:member(ReplayType, ?REPLAY_TYPE_LIST_EXCEPT_PVP_RULE) of
        true ->
	       ets:update_counter(?ETS_ID, get_replay_id_name(ReplayType), 1);
        false ->
            erlang:throw(unkown_replay_type)
    end.

gen_payID() ->
    case ets:lookup(?ETS_ID, payID) of
        [_] ->
            ets:update_counter(?ETS_ID, payID, 1);
        [] ->
            ets:insert(?ETS_ID, {payID, 1}),
            1
    end.

%% %% @doc 生成邮件UID
%% gen_histUID() ->
%% 	ServerID = data_setting:get(server_id),
%% 	ServerID * (10000 * 10000 * 10000) + gen_new(histUID).



%% 邀请码生成规则
%% 基础定义：RoleID=玩家ID，
%% 	  "0"=0
%% 	  "1"=1
%% 	     .
%% 	     .
%% 	     .
%% 	  "9"=9
%% 	  "a"=10
%% 	  "b"=11
%% 	  "c"=12
%% 	  "d"=13
%% 	     .
%% 	     .
%% 	     .
%% 	  "z"=35
%% 
%% RoleID生成邀请码：
%% 	1、RoleID依据基础定义中的规则，转化为36位的字符串Str1
%% 	2、将Str1逆序，得到Str2
%% 	3、如果Str2的长度小于6个字符，则在末尾依次补足6位，得到6位邀请码字符串Str3。补位规则：加z
%% 
%% 邀请码还原成RoleID的规则：
%% 	1、将邀请码字符串Str3逆序得到Str4
%% 	2、如果第一位为z，则去掉第一位。否则直接进入下一步。
%% 	3、将步骤2获得的字符串，转化成36进制的数字，得到roleID
%% @doc 玩家ID生成邀请码
roleID2inviteCode(RoleID) ->
	Str1 = string:to_lower(integer_to_list(RoleID, 36)),
	Length = length(Str1),
	if Length > 6 ->
		   exit("fatal error, roleID exceed planed");
	   Length == 6 ->
		   Str2 = lists:reverse(Str1),
		   if hd(Str2) == $z->
				  exit("fatal error, roleID exceed planed");
			  true ->
				  Str2
		   end;
	   true ->	   
			lists:reverse(lists:duplicate(6-Length, $z)++Str1)
	end.


%% @doc 邀请码转化成玩家ID
inviteCode2roleID(InviteCode) ->
	Str1 = lists:reverse(InviteCode),
	if hd(Str1) =:= $z->
		   Str2 = tl(Str1);
	   true ->
		   Str2 = Str1
	end,
	list_to_integer(Str2, 36).

%% base36_to_10(Base36) when Base36 >= $a -> Base36-$a+10;
%% base36_to_10(Base36) -> Base36-$0.