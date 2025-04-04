%% @author : admin
%% @doc : web mod

-module(mail_gift_mod).
-export([mail_gift/2]).
-include("def_role.hrl").

%% type: 0:全体玩家  1:列表内的玩家  2:全体在线玩家  3:指定等级玩儿家 4:指定vip范围的玩家 5:指定等级和vip 6:指定渠道玩家

mail_gift(Req, _DocRoot)->
	case parse_Req(Req) of 
		{ok, Type,Message, Users, Rewards, Items, Gers,Range} -> 
			Reward = get_rewards(Rewards, Items, Gers),
			case Type of
				"0" ->
					role_mail_gift:send_everyone_reward(Reward, Message, 0, []);
				"1" ->
					if Users =:= [] ->
						   ok;
					   true->
						   UserList = get_users(Users),
						   role_mail_gift:send_gift_by_name(UserList, Reward, Message)
					end;
				"2" ->
					role_mail_gift:send_onliners_gift(Reward, Message);
				"3" ->
					{[{_,LevelLow},{_,LevelHigh}]} = ejson:decode(Range),
					role_mail_gift:send_level_reward(Reward, Message, trans(LevelLow), trans(LevelHigh));
				"4" ->
					{[{_,VipLow},{_,VipHigh}]} = ejson:decode(Range),
					role_mail_gift:send_vip_reward(Reward, Message, trans(VipLow), trans(VipHigh));
                "5" ->
                    {[{_,LevelLow},{_,LevelHigh},{_,VipLow},{_,VipHigh}]} = ejson:decode(Range),
                    role_mail_gift:send_level_vip_reward(Reward, Message, trans(LevelLow), trans(LevelHigh),
                                                         trans(VipLow), trans(VipHigh));
                "6" ->
                    {[{_,SrcType}]} = ejson:decode(Range),
                    role_mail_gift:send_src_type_reward(Reward, Message, trans(SrcType));
				 _ ->
					ignore
			end,
			Reply = ejson:encode({[{<<"result">>,<<"succ">>}]}),
			Req:ok({"text/html; charset=utf-8", Reply});
		_ -> 
			Reply = ejson:encode({[{<<"result">>,<<"err">>}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

trans(V) when erlang:is_binary(V) ->
    erlang:list_to_integer(erlang:binary_to_list(V));
trans(V) when erlang:is_list(V) ->
    erlang:list_to_integer(V);
trans(V) when erlang:is_integer(V) ->
    V.

parse_Req(Req)->
	QueryString = Req:parse_post(),
	%% 提取消息中的信息
	Pass = proplists:get_value("pass", QueryString),
	Message = proplists:get_value("message", QueryString),
	{[{_,MessageT}]} = ejson:decode(Message),
	Message1 = binary:bin_to_list(MessageT),
	UserList = proplists:get_value("userlist", QueryString),
	RewardList = proplists:get_value("reward", QueryString),
	ItemList = proplists:get_value("item", QueryString),
	GerList = proplists:get_value("ger", QueryString),
	Type = proplists:get_value("type", QueryString),
	Range = proplists:get_value("range", QueryString),
	{can_pass(Pass, Message1),Type, Message1, UserList, RewardList, ItemList, GerList, Range}.

can_pass(Pass, Message)	->
	%% 认证消息是否有效,认证失败,则消息不进行公告广播
	{Auth, _, _} = data_setting:get(passinfo),
	Auth3 = util:md5(Auth ++ Message),	
	if Pass =:= Auth3 -> ok;
	   true -> false
	end.

get_users(T) ->
	%List = lists:flatten(tuple_to_list(ejson:decode(T))),
	{_, List} = mochijson2:decode(T),
	lists:map(fun(X)->{_,B} = X,
					  B
			  end, List).

get_rewards(Reward, Item, Ger)->
	Rewards = parse_reward(Reward),
	Items = parse_item(Item), 
	Gers = parse_ger(Ger),
	[Coin, Gold, Repu, Exp] = Rewards,
	{sell_reward,Coin, Exp,0, Gold, Items, Repu, Gers}.

parse_reward(Reward) ->
	List = lists:flatten(tuple_to_list(ejson:decode(Reward))),
	lists:map(fun(X)->{_,B} = X,
					  %B1=erlang:list_to_integer(lists:flatten(binary:bin_to_list(B))),
					  B
			  end,List).

parse_item(Item)->
	List = lists:flatten(tuple_to_list(ejson:decode(Item))),
	List1 = lists:map(fun(X)->{A,B} = X,
							  A1=erlang:list_to_integer(lists:flatten(binary:bin_to_list(A))),
							  %B1=erlang:list_to_integer(lists:flatten(binary:bin_to_list(B))),
							  {A1,B}
					  end,List),
	lists:flatten(
	  lists:map(fun(X)->{A,B}=X,
						case data_item:get(A) of
							undefined ->
								[];
							#data_item{itemLevel=ItemLevel,itemRank=ItemRank}->
								[{new_item,A,B,ItemLevel,ItemRank}]
						end
				end, List1)).
parse_ger(Ger)->
	List = lists:flatten(tuple_to_list(ejson:decode(Ger))),
	List1 = lists:map(fun(X)->{A,B} = X,
							  A1=erlang:list_to_integer(lists:flatten(binary:bin_to_list(A))),
							  %B1=erlang:list_to_integer(lists:flatten(binary:bin_to_list(B))),
							  {A1,B}
					  end,List),
	lists:flatten(
	  lists:map(fun(X)->{A,B}=X,
						%case data_ger:get(A) of
						%	undefined ->
						%		[];
						%	#data_ger{gerStar=GerQuality} ->
								Seq = lists:seq(1,B),
						%		lists:map(fun(_)->[{new_ger,A,0, GerQuality}]
								lists:map(fun(_)->[{new_ger,A,1, 0}]
										  end, Seq)
						%end
						%if A =:= 0->
						%	   [];
						%   true->
						%	   #data_ger{gerStar=GerQuality}=data_ger:get(A),
						%	   [{new_ger,A,0,GerQuality}]
						%end
				end, List1)).

%-------------------------------------------------------------------------------------------------------------
	
web_test()	->
	inets:start(),
	UserList=ejson:encode({[{<<"1">>,<<"冯晓倩">>}
							,{<<"2">>,<<"ddd">>}
							,{<<"3">>,<<"孔文莹">>}
						   ]}),
	Users = http_uri:encode(binary:bin_to_list(UserList)),
	Msg1 = ejson:encode({[{<<"msg">>,<<"今天天气不错">>}]}),
	{[{_,MsgT}]}=ejson:decode(Msg1),
	Message = http_uri:encode(binary:bin_to_list(Msg1)),
	%Pass=erlang:md5(lists:merge("passed", binary:bin_to_list(Msg1))),
	Pass = util:md5(lists:merge("passed", binary:bin_to_list(MsgT))),
	Type = "2",
	%RewardList = ejson:encode({[{<<"gold">>,<<"0">>}%coin
	%							,{<<"coin">>,<<"0">>}%gold
	%							,{<<"renown">>,<<"0">>}%repu
	%							,{<<"experience">>,<<"1">>}%exp
	%						   ]}),
		RewardList = ejson:encode({[{<<"gold">>,0}
								,{<<"coin">>,0}
								,{<<"renown">>,0}
								,{<<"experience">>,1}
							   ]}),
	Reward = http_uri:encode(binary:bin_to_list(RewardList)),
	%ItemList = ejson:encode({[{<<"1">>,<<"10">>}
	%					  ,{<<"11001">>,<<"10000">>}
	%					 ]}),
		ItemList = ejson:encode({[{<<"1">>,10}
						  ,{<<"11001">>,30}
						 ]}),
	Item = http_uri:encode(binary:bin_to_list(ItemList)),
	%GerList = ejson:encode({[{<<"12403">>,<<"1">>}
	%						 ,{<<"12402">>,<<"2">>}]}),
	GerList = ejson:encode({[]}),%{<<"12403">>,1}
							 %,{<<"12402">>,2}]}),
	Ger = http_uri:encode(binary:bin_to_list(GerList)),
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s&userlist=~s&reward=~s&item=~s&ger=~s", 
									  [Pass,Message, Type, Users, Reward,Item,Ger])),
	%io:format("~w\n",[Arg]),
	httpc:request(post, {"http://127.0.0.1:8088/mailgift",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).
web_test2()	->
	inets:start(),
	Range = http_uri:encode(binary:bin_to_list(ejson:encode({[{<<"low">>,5},{<<"high">>,8}]}))),
	UserList=ejson:encode({[{<<"1">>,<<"冯晓倩">>}
							,{<<"2">>,<<"ddd">>}
							,{<<"3">>,<<"孔文莹">>}
						   ]}),
	Users = http_uri:encode(binary:bin_to_list(UserList)),
	Msg1 = ejson:encode({[{<<"msg">>,<<"今天天气不错">>}]}),
	{[{_,MsgT}]}=ejson:decode(Msg1),
	Message = http_uri:encode(binary:bin_to_list(Msg1)),
	Pass = util:md5(lists:merge("passed", binary:bin_to_list(MsgT))),
	Type = "4",

		RewardList = ejson:encode({[{<<"gold">>,0}
								,{<<"coin">>,0}
								,{<<"renown">>,0}
								,{<<"experience">>,1}
							   ]}),
	Reward = http_uri:encode(binary:bin_to_list(RewardList)),
		ItemList = ejson:encode({[{<<"1">>,10}
						  ,{<<"11001">>,30}
						 ]}),
	Item = http_uri:encode(binary:bin_to_list(ItemList)),

	GerList = ejson:encode({[{<<"12401">>,3},{<<"12402">>,3},{<<"12403">>,3}]}),
	Ger = http_uri:encode(binary:bin_to_list(GerList)),
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s&userlist=~s&reward=~s&item=~s&ger=~s&range=~s", 
									  [Pass,Message, Type, Users, Reward,Item,Ger,Range])),
	%io:format("~w\n",[Arg]),
	httpc:request(post, {"http://192.168.1.27:8089/mailgift",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).

web_test3() ->
    inets:start(),
    Msg1 = ejson:encode({[{<<"msg">>,<<"三连败的湖人今天主场迎来了西部老大勇士队的挑战，科比本场轮休，湖人7人得分上双，全场攻守俱佳，以115-105送给联盟第一的勇士一场大败。
                                       本场比赛视频集锦：骚！库里单手把湖人全队耍个遍，克拉克森反击折叠暴扣,林书豪高难度后仰压哨打板命中
                                       比赛录像：第一节，第二节，第三节
                                       ">>}]}),
    Message = http_uri:encode(binary:bin_to_list(Msg1)),
    Pass = util:md5(lists:merge("passed", binary:bin_to_list(<<"测试福利">>))),
    Type = "6",
    RewardList = ejson:encode({[
                                {<<"coin">>,100000},
                                {<<"gold">>,0},
                                {<<"renown">>,0},
                                {<<"experience">>,0}
                               ]}),
    Reward = http_uri:encode(binary:bin_to_list(RewardList)),
%%     ItemList = ejson:encode({[{<<"10101">>,10}
%%                               ,{<<"10102">>,30}
%%                              ]}),
    ItemList = ejson:encode({[]}),
    Item = http_uri:encode(binary:bin_to_list(ItemList)),
%%     GerList = ejson:encode({[{<<"2010">>,1},{<<"2020">>,2}]}),
    GerList = ejson:encode({[]}),
    Ger = http_uri:encode(binary:bin_to_list(GerList)),
    Range = http_uri:encode(binary:bin_to_list(ejson:encode({[{<<"srctype">>, 1}]}))),
    Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s&reward=~s&item=~s&ger=~s&range=~s", 
                                      [Pass,Message, Type, Reward,Item,Ger,Range])),
    httpc:request(post, {"http://192.168.0.124:8089/mailgift",
                         [], "application/x-www-form-urlencoded",Arg}, [], []).
