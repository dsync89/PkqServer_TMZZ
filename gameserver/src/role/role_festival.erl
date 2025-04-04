-module(role_festival).

-compile(export_all).

-include("def_role.hrl").

-define(CLICK_TYPE_ONE, 0).
-define(CLICK_TYPE_TEN, 1).
-define(CLICK_TYPE_ACC, 2).

cs_festival_click(#cs_festival_click{clickType=ClickType,id=ID}) ->
    case catch check_festival_click(ClickType,ID) of
        {ok, NewRoleFestival, RewardList, RoleInfo, NeedGold,AccBoxIDList} ->
            do_festival_click(NewRoleFestival, RewardList, RoleInfo, NeedGold, ClickType,AccBoxIDList,ID);
        {false, Reason} ->
            ?sendself(#sc_festival_click{
                                         id=ID
                                         ,clickType=ClickType
                                         ,result=Reason
                                         ,nextTotalCount=0
                                         ,totalCount=0
                                         ,totalGetCount=0
                                         ,freeCount=0
                                         ,clickReward=[]
                                        })
    end.

do_festival_click(#role_festival{freeCount=FreeCount,totalCount=TotalCount,activeList=ActiveList,dropList=DropList}=NewRoleFestival, RewardList, #role{roleID=RoleID}=RoleInfo, NeedGold, ClickType,AccBoxIDList,ID) ->
    festival_server:update_role_festival(RoleID, NewRoleFestival, ID),
    RoleInfo2 = role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_FESTIVAL, ClickType, erlang:integer_to_list(ID)),
    role_reward:handle_sys_reward(RoleInfo2, lists:flatten(RewardList), ?MONEY_ADD_TYPE_FESTIVAL, ClickType, erlang:integer_to_list(ID)),
    RewardView = [role_reward:transform2p_mail_reward(Reward)||Reward<-RewardList],
    ?sendself(#sc_festival_click{
                                 id=ID
                                 ,clickType=ClickType
                                 ,result=0
                                 ,nextTotalCount=festival_server:get_next_total_count(TotalCount, AccBoxIDList)
                                 ,totalCount=TotalCount
                                 ,totalGetCount=festival_server:calc_now_total_get(ActiveList, DropList)
                                 ,freeCount=FreeCount
                                 ,clickReward=RewardView}).

check_festival_click(ClickType,ID) ->
	case tencent_pay:check_pay_arg(role_data:get_roleInfo()) of
		true  -> next;
		false -> erlang:throw({false, 255})
	end,
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    Now = util:now(),
    Info = festival_server:role_festival(RoleID, ID),
    case Info of
        ?undefined ->
            erlang:throw({false, 1}),
            RoleFestival = ?undefined,
            Config = ?undefined;
        {ok, ?undefined, Config} ->
            RoleFestival = init_role_festival(Config, Now);
        {ok, RoleFestival, Config} ->
            next
    end,
    
    #data_festival{startTime=StartTime,
                   stopTime=StopTime,
                   onePrice=OnePrice,
                   tenPrice=TenPrice,
                   coinBoxID=CoinBoxID,
                   itemBoxID=ItemBoxID,
                   ruleBoxIDList=RuleBoxIDList,
                   accBoxIDList=AccBoxIDList} = Config,
    
    case Now >= util:configtime_to_seconds(StartTime) andalso Now < util:configtime_to_seconds(StopTime) of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    
    case ClickType of
        ?CLICK_TYPE_ONE ->
            case RoleFestival#role_festival.freeCount > 0 of
                true ->
                    NeedGold = 0;
                false ->
                    NeedGold = OnePrice
            end;
        ?CLICK_TYPE_TEN ->
            NeedGold = TenPrice;
        ?CLICK_TYPE_ACC ->
            NeedGold = 0
    end,
    case role_lib:check_money(RoleInfo, gold, NeedGold) of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    {ok, NewRoleFestival, RewardList} = update_role_festival_click(ClickType, RoleFestival,CoinBoxID,ItemBoxID,RuleBoxIDList,AccBoxIDList),
    {ok, NewRoleFestival, RewardList, RoleInfo, NeedGold,AccBoxIDList}.

init_role_festival(#data_festival{freeTimes=FreeTimes}, Now) ->
    #role_festival{freeCount = FreeTimes,timestamp = Now,totalCount = 0,activeList = [], dropList = []}.

update_role_festival_click(?CLICK_TYPE_ONE, #role_festival{freeCount=FreeCount,totalCount=TotalCount}=RoleFestival,CoinBoxID,ItemBoxID,RuleBoxIDList,AccBoxIDList) ->
    case FreeCount > 0 of
        true ->
            NewFreeCount = FreeCount - 1;
        false ->
            NewFreeCount = FreeCount
    end,
    NewTotalCount = TotalCount + 1,
    NewActiveList = update_active_list(NewTotalCount, AccBoxIDList),
    BoxIDList = get_box_id_list(CoinBoxID,ItemBoxID,RuleBoxIDList,1,TotalCount),
    {ok, RoleFestival#role_festival{freeCount=NewFreeCount,totalCount=NewTotalCount,activeList=NewActiveList}, gen_reward_list(BoxIDList)};

update_role_festival_click(?CLICK_TYPE_TEN, #role_festival{totalCount=TotalCount}=RoleFestival,CoinBoxID,ItemBoxID,RuleBoxIDList,AccBoxIDList) ->
    NewTotalCount = TotalCount + 10,
    NewActiveList = update_active_list(NewTotalCount, AccBoxIDList),
    BoxIDList = get_box_id_list(CoinBoxID,ItemBoxID,RuleBoxIDList,10,TotalCount),
    {ok, RoleFestival#role_festival{totalCount=NewTotalCount,activeList=NewActiveList}, gen_reward_list(BoxIDList)};

update_role_festival_click(?CLICK_TYPE_ACC, #role_festival{activeList = ActiveList, dropList = DropList}=RoleFestival,_CoinBoxID,_ItemBoxID,_RuleBoxIDList,AccBoxIDList) ->
    case util:fun_find(fun(ActiveVal) ->
                               not lists:member(ActiveVal, DropList)
                       end, lists:sort(ActiveList)) of
        false ->
            erlang:throw({false, 4});
        Val ->
            case lists:keyfind(Val, 1, AccBoxIDList) of
                false ->
                    erlang:throw({false, 5});
                {Val, BoxID} ->
                    NewDropList = [Val|DropList],
                    BoxIDList = [BoxID],
                    {ok, RoleFestival#role_festival{dropList = NewDropList}, gen_reward_list(BoxIDList)}
            end
    end.

get_box_id_list(CoinBoxID,ItemBoxID,RuleBoxIDList,LeftTimes,TotalCount) ->
    get_box_id_list2(CoinBoxID,ItemBoxID,RuleBoxIDList,LeftTimes,TotalCount+1, []).

get_box_id_list2(_CoinBoxID,_ItemBoxID,_RuleBoxIDList,0,_TotalCount,BoxIDList) ->
    BoxIDList;
get_box_id_list2(CoinBoxID,ItemBoxID,RuleBoxIDList,LeftTimes,TotalCount,BoxIDList) ->
    case lists:keyfind(TotalCount, 1, RuleBoxIDList) of
        false ->
            NewItemBoxID = ItemBoxID;
        {_, NewItemBoxID} ->
            next
    end,
    get_box_id_list2(CoinBoxID,ItemBoxID,RuleBoxIDList,LeftTimes-1,TotalCount+1,[{CoinBoxID,NewItemBoxID}|BoxIDList]).

update_active_list(NewTotalCount, AccBoxIDList) ->
    [Val||{Val, _}<-lists:keysort(1, AccBoxIDList), NewTotalCount >= Val].

gen_reward_list(BoxIDList) ->
    [begin
         case BoxIDT of
             {BoxID1, BoxID2} ->
                 [RConfig1|_] = data_box:get({BoxID1, 0}),
                 Reward1 = util:random_one_from_weigh_list(RConfig1),
                 [RConfig2|_] = data_box:get({BoxID2, 0}),
                 Reward2 = util:random_one_from_weigh_list(RConfig2),
                 [Reward1, Reward2];
             BoxID ->
                 [RConfig|_] = data_box:get({BoxID, 0}),
                 Reward = util:random_one_from_weigh_list(RConfig),
                 [Reward]
         end
     end||BoxIDT<-BoxIDList].



