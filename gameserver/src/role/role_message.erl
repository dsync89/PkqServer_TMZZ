%% @author admin
%% @doc 公告、信息
%% Created 2013-4-1


-module(role_message).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

cs_message_notice(#cs_message_notice{curMaxNoticeID=CurMaxNoticeID}) ->
	IDList = data_notice:get_list(),
	case lists:member(CurMaxNoticeID,IDList) of
	  	 false ->
			Content = [data_notice:get(E)||E<-IDList];
		 _ ->
			Content = []
	end,		 
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    ExtList = festival_server:get_activity_info(), 
    {IconList, DataList} = activity_server:get_activity_list(),
    %% 获取玩家的活动信息   （以前是 活动进程处理完数据后发回，现在是 活动进程返回玩家数据，玩家进程自己处理）
    RoleActivityInfo = activity_server:get_info(RoleID),
    case get_all(RoleActivityInfo, Content, IDList, ExtList, IconList, DataList) of
        {ok, Record} -> 
            case ets:lookup(?ETS_ETC, activityIDList) of 
                [] ->
                    ?sendself(Record#sc_message_notice{activityIDList=[]});
                [{activityIDList, ActivityIDList}] ->
                    ?sendself(Record#sc_message_notice{activityIDList=ActivityIDList})
            end;
        Err ->
            ?ERR("~w", [Err]),
            ?sendself(#sc_message_notice{picID=0, noticeIDList=IDList, noticeList=Content, iconList=[], infoList=[], list=ExtList, activityIDList=[]})
    end.


cs_message_certain_notice(#cs_message_certain_notice{noticeIDList=NoticeIDList}) ->
	Reply = #sc_message_certain_notice{noticeList=[data_notice:get(E)||E<-NoticeIDList]},
	?sendself(Reply).
	
							  
cs_message_test(#cs_message_test{msg=Msg}) ->
	%% 正式发布版，gm命令无效
	case data_setting:get(is_release) of
		false ->
			role_gm:test(Msg);
		_ ->
			ignore
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================
    
data_notice_transform_list(List) ->
	[data_notice_transform(E)||E<-List].

data_notice_transform(Notice) ->
	#notice{content=C,noticeID=I,title=T} = Notice,
	#p_notice{content=C,noticeID=I,title=T}.

get_all(RoleActivityInfo, Content, IDList, ExtList, IconList, DataList) ->
    {ok, IconList2} = role_activity:do_get_list2({IconList, DataList}),
    InfoList = [begin 
                    {ok, Info} = do_info2(RoleActivityInfo, AID), 
                    Info
                end||#p_activity_icon{activityID=AID}<-IconList2],
    PicList = data_common:get(pic_list),
    PicID =
        case PicList of
            [] ->
                0;
            _ ->
                lists:nth(random:uniform(erlang:length(PicList)), PicList)
        end,
    {ok, #sc_message_notice{picID=PicID, noticeIDList=IDList, noticeList=Content, iconList=IconList2, infoList=InfoList, list=ExtList}}.

type(exchange) ->1;
type(exchange2) ->5;
type(exchange3) ->7;
type(pay_special_num) ->3;
type(pay_acc_num) ->2;
type(pay_day_num) ->4;
type(consume) ->6.

default_value(pay_special_num) ->0;
default_value(pay_acc_num) ->0;
default_value(pay_day_num) ->{0, 0};
default_value(consume) ->{0, 0, 0};
default_value(exchange) ->0;
default_value(exchange2) ->0;
default_value(exchange3) ->0.

act(#dc{actList=List}, ID, Type) ->
    case lists:keyfind(ID, #act.actID, List) of
        false ->
            #act{actID=ID,list=[],value=default_value(Type)};
        #act{}=Act ->
            Act
    end.

do_info2(Info, ID) ->
    #data_activity{type=Type, isForever=IsForever, isDailyRefresh=IsDailyRefresh,startTime=StartTime} = Config = data_activity:get(ID),
    %% value结构不同 充值天数,{0,0};其他:{0}
    if Type =:= pay_day_num ->
            #act{value={TypeValue,_TypeDate}} = Act = act(Info, ID, Type);
       Type =:= consume ->
           #act{value={TypeValue, _OtherType1, _OtherType2}} = Act = act(Info, ID, Type);
        true ->
            #act{value=TypeValue} = Act = act(Info, ID, Type)
    end,
    if IsForever =:= true->
           IsForever2 = 1;
       true ->
           IsForever2 = 2
    end,
    if IsDailyRefresh =:= true->
           IsDailyRefresh2 = 1;
       true ->
           IsDailyRefresh2 = 2
    end,
    {StartDay,StartSec} = StartTime,
    if erlang:is_tuple(StartDay) ->
           Record = #sc_activity_info{startTime=util:datetime_to_seconds(StartTime),
                                      stopTime=util:datetime_to_seconds(Config#data_activity.stopTime),
                                      type=type(Type),
                                      drawList=proto_draw_list(Act,Config),
                                      activityID=ID,
                                      description=Config#data_activity.description,
                                      isForever = IsForever2,
                                      isDailyRefresh = IsDailyRefresh2,
                                      typeValue=TypeValue};
       true ->
           {ServerOpenDate,_} = data_setting:get(serverOpenTime),
           Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay - 1) * ?ONE_DAY_SECONDS,
           {StopDay, StopSec} = Config#data_activity.stopTime,
           Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay -1) * ?ONE_DAY_SECONDS,
           Record = #sc_activity_info{startTime=Start,
                                      stopTime=Stop,
                                      type=type(Type),
                                      drawList=proto_draw_list(Act,Config),
                                      activityID=ID,
                                      description=Config#data_activity.description,
                                      isForever = IsForever2,
                                      isDailyRefresh = IsDailyRefresh2,
                                      typeValue=TypeValue}
    end,
    {ok, Record}.

proto_draw_list(Act, DataActivity) ->
    #data_activity{drawList=DrawList} = DataActivity,
    #act{list=List} = Act,
    [begin
         case lists:keyfind(ID, #draw.drawID, List) of
             false ->
                 Already = 0,
                 CanDrawTimes = 0;
             #draw{ableDrawTimes=CanDrawTimes,alreadyDrawTimes=Already} ->
                 next
         end,
         if is_list(Condition) ->
         NeedMaterial =role_reward:transform2p_reward_view(Condition,[]);
            true ->
                NeedMaterial=[]
         end,
         #p_activity_draw{alreadyDrawTimes=Already,canDrawTimes=CanDrawTimes,description=Description,drawID=ID,maxDrawTimes=Max,
                          rewardInfo=sell_reward2p_reward_info(Reward),
                          needMaterial=NeedMaterial}                         
     end || #data_activity_draw{description=Description,drawID=ID,maxDrawNum=Max,reward=Reward,condition=Condition} <- DrawList].

sell_reward2p_reward_info(SellReward) ->
    #sell_reward{coin=Coin,gerExp=GerExp,gold=Gold,item=Item,newGer=NewGer,reputation=Reputation,roleExp=RoleExp} = SellReward,
    NewGer2 = ger_lib:new_gerList2p_ger_view(NewGer),
    Item2 = item_lib:new_itemList2p_item_view(Item),
    #p_reward_info{coin=Coin,gerExp=GerExp,gerList=NewGer2,gold=Gold,itemList=Item2,reputation=Reputation,roleExp=RoleExp}.






