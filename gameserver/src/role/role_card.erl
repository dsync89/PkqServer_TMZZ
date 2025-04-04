%% @author admin
%% @doc 点将功能
%% Created 2013-4-23


-module(role_card).
-compile(export_all).
-include("def_role.hrl").
-include("def_weibo_share.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).


%% ====================================================================
%% API functions
%% ====================================================================
cs_card_get_list(#cs_card_get_list{}) ->
    #cardInfo{cardList=CardList,openedCardList=OpenCardList} = CardInfo = role_data:get_cardInfo(),
    IsOpen = data_card:get(is_open),
    GoldPrice = data_card:get(gold_price),
    NeedNumList = [begin
                       {_, NeedNum} = data_card:get(Seq),
                       NeedNum
                   end||Seq<-lists:seq(1, 6)],
    %% 判断是否是第一次打开这个界面
    if OpenCardList =:=[] andalso CardList =:= [] ->
           %% 免费刷新一次
           CardList2 = random_card_list(),
           CardInfo2 = CardInfo#cardInfo{cardList=CardList2},
           role_data:set_cardInfo(CardInfo2),
           ?sendself(#sc_card_get_list{cardList=card2p_card(CardList2),openedCardList=[],isOpen=IsOpen,needNumList=NeedNumList,goldPrice=GoldPrice});
       true ->
           ?sendself(#sc_card_get_list{cardList=card2p_card(CardList),openedCardList=OpenCardList,isOpen=IsOpen,needNumList=NeedNumList,goldPrice=GoldPrice})
    end.

cs_card_draw(#cs_card_draw{pos=Pos}) ->
	case check_draw() of
		{true, CardInfo, BagOther2, DelAcc, UpdateAcc, UpdateLogAcc} ->
			role_data:set_bagItem(BagOther2),
			Role = role_data:get_roleInfo(),
			do_draw(Role, CardInfo, Pos, DelAcc, UpdateAcc, UpdateLogAcc);
		{true, CardInfo, BagOther2, Role, NeedGold, DelAcc, UpdateAcc, UpdateLogAcc} ->
			role_data:set_bagItem(BagOther2),
			Role2=role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_DRAW_CARD, 0, ""),
			do_draw(Role2, CardInfo, Pos, DelAcc, UpdateAcc, UpdateLogAcc);
		{false, Reason} ->
			?sendself(#sc_card_draw{card=[],pos=Pos,result=Reason})
    end.

cs_card_onekey(_) ->
	case check_onekey() of
		{true, CardInfo, BagOther2, DelAcc, UpdateLogAcc} ->
			role_data:set_bagItem(BagOther2),
			Role = role_data:get_roleInfo(),
			do_onekey(Role, CardInfo, DelAcc, UpdateLogAcc);
		{true, CardInfo, BagOther2, Role, NeedGold, DelAcc, UpdateLogAcc} ->
			role_data:set_bagItem(BagOther2),
			Role2=role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_ONEKEY_DRAW_CARD, 0, ""),
			do_onekey(Role2, CardInfo, DelAcc, UpdateLogAcc);
		{false, Reason} ->
			?sendself(#sc_card_onekey{card=[],result=Reason})
			end.

cs_card_refresh(_) ->
	case check_refresh() of
		{true, CardInfo, NeedCoin, Role} ->
            case NeedCoin > 0 of
                true ->
                    role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_REFRESH_CARD, 0, "");
                false ->
                    next
            end,
			do_refresh(CardInfo);
		{true, CardInfo} ->
			do_refresh(CardInfo);
		{false, Reason}->
			?sendself(#sc_card_refresh{cardList=[],result=Reason})
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

do_refresh(CardInfo) ->
			CardList = random_card_list(),
			CardInfo2 = CardInfo#cardInfo{cardList=CardList,openedCardList=[]},
			role_data:set_cardInfo(CardInfo2),
			?sendself(#sc_card_refresh{cardList=card2p_card(CardList), result=1}).

check_refresh() ->
	#cardInfo{cardList=CardList} = CardInfo = role_data:get_cardInfo(),
	case length(CardList) of
		6 ->
			NeedCoin = data_card:get(refresh_card_coin),
			Role = role_data:get_roleInfo(),
			case role_lib:check_money(Role, coin, NeedCoin) of
				false ->
					{false,2};
				true ->
					{true, CardInfo, NeedCoin, Role}
			end;
		_ ->
			{true, CardInfo}
	end.

do_draw(Role, CardInfo, Pos, DelAcc, UpdateAcc, UpdateItemLogAcc) ->
	#role{roleID=RoleID} = Role,
	%% 写道具日志
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_DRAW_CARD, 0, ""),

	#cardInfo{openedCardList=OpenCardList,cardList=CardList, drawCount=DrawCount} = CardInfo,
	NewCount = DrawCount+1,
	{CardList2, GroupID, Type, Value} = random_card(NewCount, CardList),	
	OpenCardList2 = [#p_opened_card{pos=Pos,type=Type,value=Value}|OpenCardList],
	CardInfo2 = CardInfo#cardInfo{cardList=CardList2,drawCount=NewCount,openedCardList=OpenCardList2},
	role_data:set_cardInfo(CardInfo2),
    case UpdateAcc =/= [] of
        true ->
            UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
            ?sendself(#sc_item_update{updateList=UpdateList});
        false ->
            next
    end,
    case DelAcc =/= [] of
        true ->
            DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
            ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList});
        false ->
            next
    end,
	?sendself(#sc_card_draw{card=[#p_card{type=Type,value=Value}],result=1,pos=Pos}),
	role_reward:handle_card_reward_f(Type, Value),
	case Type of
		2 ->
			#data_item{itemType=ItemType} = data_item:get(Value),
			IsEquip = item_lib:is_main_equip(ItemType),
			case GroupID >= 5 andalso IsEquip of
				true ->
					#role{roleName=RoleName} = Role,
					broadcast_server:bc(#sc_message_best_card{roleName=RoleName,type=Type,value=Value});
				false ->
					ignore
			end;
		_ ->
			ignore
	end,
	%% 增加可微博分享的事件
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_CARD).

-define(POS_LIST, [0,1,2,3,4,5]).

do_onekey(Role, CardInfo, DelAcc, UpdateItemLogAcc) ->
	#role{roleName=RoleName,roleID=RoleID} = Role,
	%% 写道具日志
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ONEKEY_DRAW_CARD, 0, ""),

	#cardInfo{openedCardList=OpenCardList,cardList=CardList, drawCount=DrawCount} = CardInfo,
	UsedPosList = [E||#p_opened_card{pos=E}<-OpenCardList],
	PosList = ?POS_LIST--UsedPosList,
	NewOpenedCardList = lists:zipwith(fun(Pos,Card) ->do_onekey2(RoleName, Card, Pos) end, PosList, CardList),						
	CardInfo2 = CardInfo#cardInfo{cardList=[],drawCount=DrawCount+1,openedCardList=OpenCardList++NewOpenedCardList},
	role_data:set_cardInfo(CardInfo2),
	?sendself(#sc_card_onekey{result=1,card=NewOpenedCardList}),
	%% 增加可微博分享的事件
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_CARD).


do_onekey2(RoleName, Card, Pos) ->
	#card{groupID=GroupID,type=Type,value=Value}=Card,
	role_reward:handle_card_reward_f(Type, Value),
	case GroupID >= 5 of
		true ->
			broadcast_server:bc(#sc_message_best_card{roleName=RoleName,type=Type,value=Value});
		false ->
			ignore
	end,
	#p_opened_card{pos=Pos,type=Type,value=Value}.
				  

random_card(DrawCount, CardList) ->
	case data_fixed_card:get(DrawCount) of
		GroupID when is_integer(GroupID) ->
			case lists:keytake(GroupID, #card.groupID, CardList) of
				false ->
					random_card2(CardList);
				{value, Card, CardList2} ->
					{CardList2, GroupID, Card#card.type,Card#card.value}
			end;
		_ ->
			random_card2(CardList)
	end.

random_card2([#card{groupID=GroupID, type=Type, value=Value}]) ->
	{[], GroupID, Type, Value};
random_card2(CardList) ->
	Config = data_card:get(prob),
	RandomList = [lists:nth(GroupID,Config) || #card{groupID=GroupID} <- CardList],
	Total = lists:sum(RandomList),
	Random = random:uniform()*Total,
	RandomSeq=
	util:foldl(fun(E,{Seq, Acc}) ->
					   Acc2 = E+Acc,
					   if  Acc2 >= Random ->
							   {return, Seq+1};
						   true ->
							   {Seq+1, Acc2}
					   end
			   end, {0, 0}, RandomList),
	?DEBUG("random=~w,randomList=~w,randomSeq=~w",[Random, RandomList, RandomSeq]),
	{value, #card{groupID=TGroupID, type=Type,value=Value}, CardList2} = util:nth_take(RandomSeq, CardList),
	{CardList2, TGroupID, Type, Value}.

check_draw() ->
	#cardInfo{openedCardList=OpenCardList,cardList=CardList} = CardInfo = role_data:get_cardInfo(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case CardList of
				[] ->
					{false, 3};
				_ ->
					Seq = length(OpenCardList) +1,
                    GoldUnit = data_card:get(gold_price),
					{ItemTypeID, ItemNum} = data_card:get(Seq),
					BagOther = role_data:get_bagItem(),
					case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
						{BagOther2, 0, DelAcc, UpdateAcc, UpdateLogAcc} ->
							{true, CardInfo, BagOther2, DelAcc, UpdateAcc, UpdateLogAcc};
						{BagOther2, RestNum, DelAcc, UpdateAcc, UpdateLogAcc} ->
							NeedGold = RestNum * GoldUnit,
							Role = role_data:get_roleInfo(),
							case role_lib:check_money(Role, gold, NeedGold) of
								true ->
									{true, CardInfo, BagOther2, Role, NeedGold, DelAcc, UpdateAcc, UpdateLogAcc};
								false ->
									{false, 2}
							end;
						_ ->
							{false, 4}
					end
			end;
		false ->
			{false, 255}
	end.

check_onekey() ->
	#cardInfo{openedCardList=OpenCardList,cardList=CardList} = CardInfo = role_data:get_cardInfo(),
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true ->
			case CardList of
				[] ->
					{false, 3};
				_ ->
					Seq = length(OpenCardList) +1,
                    GoldUnit = data_card:get(gold_price),
					{ItemTypeID, ItemNum} = cacl_onekey_need(Seq, length(OpenCardList)+length(CardList)),
					BagOther = role_data:get_bagItem(),
					case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
						{BagOther2, 0, DelAcc, _UpdateAcc, UpdateLogAcc} ->
							{true, CardInfo, BagOther2, DelAcc, UpdateLogAcc};
						{BagOther2, RestNum, DelAcc, _UpdateAcc, UpdateLogAcc} ->
							NeedGold = RestNum * GoldUnit,
							Role = role_data:get_roleInfo(),
							case role_lib:check_money(Role, gold, NeedGold) of
								true ->
									{true, CardInfo, BagOther2, Role, NeedGold, DelAcc, UpdateLogAcc};
								false ->
									{false, 2}
							end;
						_ ->
							{false, 4}
					end
			end;
	false ->
		{false, 255}
	end.

%% 计算一键所需
cacl_onekey_need(SeqStart, SeqEnd) ->
	lists:foldl(fun(Seq,{_A,B,_C}) ->
						{ItemTypeID, ItemNum} = data_card:get(Seq),
						{ItemTypeID, ItemNum+B}
				end, {0,0,0}, lists:seq(SeqStart,SeqEnd)).

	
card2p_card(CardList) ->
	[#p_card{type=Type,value=Value}||#card{type=Type,value=Value}<-CardList].
random_card_list() ->
	Config = data_card:get(random),
	List = 
	[begin 
		 {Type,Value} = util:random_one_from_list(List),
		 #card{groupID=GroupID, type=Type,value=Value}
	 end || {GroupID, List} <- Config],
	util:random_list2(List).