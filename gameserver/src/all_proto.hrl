-record(cs_melee_info,{
	}).
-record(sc_melee_info,{
	leftTimes=[] :: ?int8
	,melee_status=[] :: ?int8
	,countdown=[] :: ?int32
	,miaomiao_score=[] :: ?int32
	,pipi_score=[] :: ?int32
	,self_score=[] :: ?int16
	,camp=[] :: ?int8
	,cur_win_times=[] :: ?int8}).
-record(cs_melee_sign,{
	}).
-record(sc_melee_sign,{
	result=[] :: ?int8}).
-record(cs_melee_fight,{
	}).
-record(p_id_num,{
	typeID=[] :: ?int16
	,num=[] :: ?int32}).
-record(p_mail_reward,{
	itemList=[] :: [#p_id_num{}]
	,gerList=[] :: [#p_id_num{}]}).
-record(p_action,{
	gerPos=[] :: ?int8
	,actionID=[] :: ?int8
	,targetPos=[] :: [?int8]
	,addSp=[] :: ?int8
	,addHp=[] :: ?int32
	,state=[] :: ?int8}).
-record(p_fighter,{
	gerID=[] :: ?int64
	,gerTypeID=[] :: ?int16
	,gerPos=[] :: ?int8
	,gerHp=[] :: ?int64
	,gerHpMax=[] :: ?int64
	,gerSp=[] :: ?int32
	,gerQuality=[] :: ?int8
	,gerLevel=[] :: ?int16}).
-record(sc_fight_request,{
	fighterList=[] :: [#p_fighter{}]
	,actionList=[] :: [#p_action{}]
	,result=[] :: boolean()}).
-record(sc_melee_fight,{
	result=[] :: ?int8
	,tar_role_id=[] :: ?int64
	,tar_role_name=[] :: ?string
	,add_score=[] :: ?int16
	,miaomiao_score=[] :: ?int32
	,pipi_score=[] :: ?int32
	,reward=[] :: #p_mail_reward{}
	,fightInfo=[] :: [#sc_fight_request{}]
	,cur_win_times=[] :: ?int8}).
-record(p_homestead_log,{
	roleName=[] :: ?string
	,type=[] :: ?int8
	,machine=0 :: ?int8
	,gerName=[] :: ?string
	,add=[] :: ?int8
	,second=[] :: ?int32}).
-record(p_homestead_machine,{
	num=[] :: ?int8
	,seedItemID=0 :: ?int16
	,endSecond=0 :: ?int32
	,harvest=0 :: ?int32
	,add4Energy=0 :: ?int32
	,addEnergyEndS=0 :: ?int32
	,get_count=0 :: ?int8}).
-record(p_homestead,{
	roleName=[] :: ?string
	,energyTimes=[] :: ?int8
	,matingTimes=[] :: ?int8
	,matingCoolSecond=0 :: ?int32
	,add4mating=0 :: ?int8
	,gerTypeID=0 :: ?int16
	,gerID=0 :: ?int64
	,quality=0 :: ?int16
	,level=0 :: ?int16
	,refreshMatingSecond=0 :: ?int32}).
-record(sc_homestead_error,{
	reason_code=[] :: ?int8}).
-record(cs_homestead_get_info,{
	}).
-record(sc_homestead_get_info,{
	baseinfo=[] :: #p_homestead{}
	,machineList=[] :: [#p_homestead_machine{}]}).
-record(cs_homestead_get_friend_info,{
	roleID=[] :: ?int32}).
-record(sc_homestead_get_friend_info,{
	roleID=[] :: ?int32
	,baseinfo=[] :: #p_homestead{}
	,machineList=[] :: [#p_homestead_machine{}]}).
-record(cs_homestead_unlock_machine,{
	num=[] :: ?int8}).
-record(sc_homestead_unlock_machine,{
	machine=[] :: #p_homestead_machine{}}).
-record(cs_homestead_uproot_seed,{
	num=[] :: ?int8}).
-record(sc_homestead_uproot_seed,{
	num=[] :: ?int8}).
-record(cs_homestead_harvest,{
	num=[] :: ?int8}).
-record(p_reward_view,{
	type=[] :: ?int8
	,value=[] :: ?int32}).
-record(sc_homestead_harvest,{
	updata_machine=[] :: #p_homestead_machine{}
	,reward=[] :: #p_reward_view{}}).
-record(cs_homestead_seeding,{
	num=[] :: ?int8
	,seedItemID=[] :: ?int16}).
-record(sc_homestead_seeding,{
	updata_machine=[] :: #p_homestead_machine{}}).
-record(sc_homestead_update_machine,{
	updata_machine=[] :: #p_homestead_machine{}}).
-record(cs_homestead_change_ger,{
	gerID=[] :: ?int64}).
-record(sc_homestead_change_ger,{
	gerID=[] :: ?int64}).
-record(cs_homestead_mating,{
	roleID=[] :: ?int32}).
-record(sc_homestead_mating,{
	matingTimes=[] :: ?int8
	,rewardList=[] :: [#p_reward_view{}]
	,fRoleID=[] :: ?int32
	,matingCoolSecond=[] :: ?int32
	,add4mating=[] :: ?int8}).
-record(sc_homestead_mating_to_friend,{
	log=[] :: #p_homestead_log{}
	,matingCoolSecond=[] :: ?int32
	,add4mating=[] :: ?int8}).
-record(cs_homestead_addenergy,{
	roleID=[] :: ?int32
	,num=[] :: ?int8}).
-record(sc_homestead_addenergy,{
	roleID=[] :: ?int32
	,num=[] :: ?int8
	,energyTimes=[] :: ?int8
	,add4Energy=[] :: ?int32
	,addEnergyEndS=[] :: ?int32
	,rewardList=[] :: [#p_reward_view{}]}).
-record(sc_homestead_addenergy_to_friend,{
	num=[] :: ?int8
	,log=[] :: #p_homestead_log{}
	,addEnergyEndS=[] :: ?int32
	,add4Energy=[] :: ?int32}).
-record(cs_homestead_get_log,{
	}).
-record(sc_homestead_get_log,{
	list=[] :: [#p_homestead_log{}]}).
-record(cs_homestead_get_friend_log,{
	roleID=[] :: ?int32}).
-record(sc_homestead_get_friend_log,{
	roleID=[] :: ?int32
	,list=[] :: [#p_homestead_log{}]}).
-record(sc_homestead_sync_mating_cool_second,{
	roleID=[] :: ?int32
	,matingCoolSecond=[] :: ?int32}).
-record(sc_homestead_sync_ger,{
	roleID=[] :: ?int32
	,gerTypeID=[] :: ?int32
	,gerQuality=[] :: ?int16}).
-record(sc_homestead_sync_add_enagy,{
	roleID=[] :: ?int32
	,beginGold=0 :: ?int32
	,endGold=0 :: ?int32
	,beginBadge=0 :: ?int32
	,endBadge=0 :: ?int32}).
-record(p_task,{
	task_id=[] :: ?int32
	,status=[] :: ?int8
	,trigger_num=[] :: ?int32}).
-record(cs_task_get_info,{
	}).
-record(sc_task_get_info,{
	main_task_list=[] :: [#p_task{}]
	,today_task_list=[] :: [#p_task{}]
	,ach_task_list=[] :: [#p_task{}]}).
-record(cs_task_operate,{
	operate_code=[] :: ?int8
	,task_id=[] :: ?int32}).
-record(sc_task_operate,{
	task_type=[] :: ?int8
	,task_id=[] :: ?int32
	,status=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(sc_task_error,{
	result=[] :: ?int8}).
-record(sc_task_notify_change,{
	task_type=[] :: ?int8
	,updata_task_list=[] :: [#p_task{}]
	,del_task_id=[] :: [?int32]}).
-record(cs_fight_request,{
	}).
-record(cs_road_info,{
	}).
-record(sc_road_info,{
	resetTimes=[] :: ?int8
	,nowID=[] :: ?int8
	,status=[] :: ?int8
	,extID=[] :: ?int8}).
-record(cs_road_reset,{
	}).
-record(sc_road_reset,{
	result=[] :: ?int8}).
-record(cs_road_fight,{
	}).
-record(sc_road_fight,{
	result=[] :: ?int8
	,isWin=[] :: boolean()
	,extID=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_mail_reward{}]}).
-record(cs_road_fight_ext,{
	}).
-record(sc_road_fight_ext,{
	result=[] :: ?int8
	,isWin=[] :: boolean()
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_mail_reward{}]}).
-record(cs_road_box,{
	}).
-record(sc_road_box,{
	result=[] :: ?int8
	,reward=[] :: [#p_mail_reward{}]}).
-record(cs_hula_open,{
	}).
-record(p_hula_boss_open,{
	bossID=[] :: ?int8
	,maxHp=[] :: ?int64
	,bossQuality=[] :: ?int16
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32}).
-record(sc_hula_open,{
	isOpen=[] :: boolean()
	,list=[] :: [#p_hula_boss_open{}]
	,beginTime=[] :: ?int32}).
-record(p_hula_boss_state,{
	bossID=[] :: ?int8
	,curHp=[] :: ?int64
	,maxHP=[] :: ?int64
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32}).
-record(sc_hula_init_state,{
	list=[] :: [#p_hula_boss_state{}]
	,curHarm=[] :: ?int64
	,curRank=[] :: ?int32
	,rebornTime=[] :: ?int32
	,endTime=[] :: ?int32}).
-record(cs_hula_close,{
	}).
-record(p_hula_info,{
	roleName=[] :: ?string
	,harmValue=[] :: ?int64}).
-record(cs_hula_cur_info,{
	}).
-record(sc_hula_cur_info,{
	hulaInfoList=[] :: [#p_hula_info{}]}).
-record(sc_hula_hp_sync,{
	bossID=[] :: ?int8
	,bossHp=[] :: ?int64}).
-record(p_hula_harm,{
	name=[] :: ?string
	,harm=[] :: ?int64}).
-record(sc_hula_harm_broadcast,{
	harmList=[] :: [#p_hula_harm{}]}).
-record(p_role_stastic,{
	harm=[] :: ?int64
	,coin=[] :: ?int32
	,repu=[] :: ?int32}).
-record(sc_hula_stop,{
	type=[] :: ?int8
	,nextTime=[] :: ?int32
	,roleSta=[] :: #p_role_stastic{}}).
-record(cs_hula_rank_sync,{
	}).
-record(sc_hula_rank_sync,{
	curRank=[] :: ?int16}).
-record(cs_hula_fight,{
	}).
-record(sc_hula_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,rebornTime=[] :: ?int32
	,rewardCoin=[] :: ?int32
	,rewardReputation=[] :: ?int32}).
-record(cs_hula_reborn,{
	}).
-record(sc_hula_reborn,{
	result=[] :: ?int8}).
-record(cs_hula_open_time,{
	}).
-record(sc_hula_open_time,{
	beginTime=[] :: ?int32}).
-record(cs_festival_info,{
	id=[] :: ?int32}).
-record(p_festival_rank_reward,{
	rankStart=[] :: ?int16
	,rankEnd=[] :: ?int16
	,reward=[] :: #p_mail_reward{}}).
-record(sc_festival_info,{
	id=[] :: ?int32
	,startTimestamp=[] :: ?int32
	,stopTimestamp=[] :: ?int32
	,totalCount=[] :: ?int32
	,totalGetCount=[] :: ?int32
	,freeCount=[] :: ?int32
	,nextTotalCount=[] :: ?int16
	,price=[] :: ?int16
	,tenPrice=[] :: ?int16
	,info=[] :: ?string
	,rewardList=[] :: [#p_festival_rank_reward{}]}).
-record(cs_festival_click,{
	id=[] :: ?int32
	,clickType=[] :: ?int8}).
-record(sc_festival_click,{
	id=[] :: ?int32
	,clickType=[] :: ?int8
	,result=[] :: ?int8
	,nextTotalCount=[] :: ?int16
	,totalCount=[] :: ?int32
	,totalGetCount=[] :: ?int32
	,freeCount=[] :: ?int32
	,clickReward=[] :: [#p_mail_reward{}]}).
-record(cs_festival_rank,{
	id=[] :: ?int32
	,start=[] :: ?int16
	,num=[] :: ?int16}).
-record(p_festival_rank,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,totalCount=[] :: ?int32
	,rank=[] :: ?int16}).
-record(sc_festival_rank,{
	id=[] :: ?int32
	,minRankCount=[] :: ?int8
	,selfRank=[] :: ?int16
	,rankList=[] :: [#p_festival_rank{}]}).
-record(cs_rule_info,{
	}).
-record(p_rule_fighter,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,score=[] :: ?int32
	,rank=[] :: ?int32
	,winTimes=[] :: ?int16
	,fightTimes=[] :: ?int16
	,winConTimes=[] :: ?int16
	,winConMaxTimes=[] :: ?int16}).
-record(p_hist,{
	histUID=[] :: ?int64
	,histType=[] :: ?int8
	,name=[] :: ?string
	,roleID=[] :: ?int32
	,time=[] :: ?int32
	,arg=[] :: ?int16
	,addRepu=[] :: ?int32}).
-record(sc_rule_info,{
	score=[] :: ?int32
	,rank=[] :: ?int32
	,winTimes=[] :: ?int16
	,fightTimes=[] :: ?int16
	,winConTimes=[] :: ?int16
	,winConMaxTimes=[] :: ?int16
	,tarFighter=[] :: #p_rule_fighter{}
	,list=[] :: [#p_hist{}]}).
-record(cs_rule_rank,{
	}).
-record(sc_rule_rank,{
	list=[] :: [#p_rule_fighter{}]}).
-record(cs_rule_last_rank,{
	}).
-record(sc_rule_last_rank,{
	list=[] :: [#p_rule_fighter{}]}).
-record(cs_rule_fight,{
	}).
-record(sc_rule_fight,{
	result=[] :: ?int8
	,isWin=[] :: boolean()
	,tarRoleName=[] :: ?string
	,newScore=[] :: ?int32
	,rank=[] :: ?int32
	,winTimes=[] :: ?int16
	,fightTimes=[] :: ?int16
	,winConTimes=[] :: ?int16
	,winConMaxTimes=[] :: ?int16
	,reward=[] :: #p_mail_reward{}
	,tarFighter=[] :: #p_rule_fighter{}
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_rule_leave,{
	}).
-record(cs_rank_info,{
	rankType=[] :: ?int8
	,type=[] :: ?int8
	,start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_rank_info,{
	rank=[] :: ?int8
	,roleID=[] :: ?int32
	,level=[] :: ?int16
	,name=[] :: ?string
	,head=[] :: ?int32
	,title=[] :: ?int8
	,isMale=[] :: boolean()
	,fightPower=[] :: ?int64
	,chapterID=[] :: ?int16
	,dungeonName=[] :: ?string}).
-record(sc_rank_info,{
	rankType=[] :: ?int8
	,type=[] :: ?int8
	,list=[] :: [#p_rank_info{}]}).
-record(cs_alien_info,{
	}).
-record(p_alien_fighter,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,rank=[] :: ?int32
	,serverID=[] :: ?int16
	,hpPercent=[] :: ?int8
	,canBeAtkTime=[] :: ?int32}).
-record(sc_alien_info,{
	isOpen=[] :: boolean()
	,endTimestamp=[] :: ?int32
	,groupID=[] :: ?int8
	,isSign=[] :: boolean()
	,leftTimes=[] :: ?int8
	,fighterList=[] :: [#p_alien_fighter{}]
	,resetTime=[] :: ?int32
	,resetNeedGold=[] :: ?int16
	,maxTimes=[] :: ?int8
	,price=[] :: ?int8}).
-record(sc_alien_sign_info,{
	needVipLevel=[] :: ?int8
	,needLevel=[] :: ?int16
	,isSign=[] :: boolean()
	,signEndTimestamp=[] :: ?int32}).
-record(cs_alien_first_five,{
	}).
-record(sc_alien_first_five,{
	fighterList=[] :: [#p_alien_fighter{}]}).
-record(cs_alien_kill_num_rank,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_alien_fighter2,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,rank=[] :: ?int32
	,serverID=[] :: ?int16
	,killNum=[] :: ?int16}).
-record(sc_alien_kill_num_rank,{
	fighterList=[] :: [#p_alien_fighter2{}]}).
-record(cs_alien_kill_continuous_rank,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_alien_fighter3,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,rank=[] :: ?int32
	,serverID=[] :: ?int16
	,killContinuousNum=[] :: ?int16
	,isInContinuous=[] :: boolean()}).
-record(sc_alien_kill_continuous_rank,{
	fighterList=[] :: [#p_alien_fighter3{}]}).
-record(cs_alien_guess_info,{
	}).
-record(sc_alien_guess_info,{
	guessCoin=[] :: ?int32
	,guessType=[] :: boolean()
	,guessOddNum=[] :: ?int32
	,guessEvenNum=[] :: ?int32
	,coinValList=[] :: [?int32]}).
-record(cs_alien_guess,{
	guessCoin=[] :: ?int32
	,guessType=[] :: boolean()}).
-record(sc_alien_guess,{
	result=[] :: ?int8}).
-record(cs_alien_reset,{
	}).
-record(sc_alien_reset,{
	result=[] :: ?int8
	,timestamp=[] :: ?int32
	,fighterList=[] :: [#p_alien_fighter{}]}).
-record(cs_alien_fight,{
	tarRoleID=[] :: ?int32
	,tarRank=[] :: ?int16}).
-record(sc_alien_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,newRank=[] :: ?int16
	,addCoin=[] :: ?int32
	,fighterList=[] :: [#p_alien_fighter{}]}).
-record(cs_alien_sign,{
	}).
-record(sc_alien_sign,{
	result=[] :: ?int8}).
-record(cs_alien_self_record,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_alien_self_record,{
	isAtk=[] :: boolean()
	,isWin=[] :: boolean()
	,enemyName=[] :: ?string
	,newRank=[] :: ?int16
	,replayUID=[] :: ?int64
	,timestamp=[] :: ?int32}).
-record(sc_alien_self_record,{
	recordList=[] :: [#p_alien_self_record{}]}).
-record(cs_alien_record,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_alien_record,{
	type=[] :: ?int8
	,atkName=[] :: ?string
	,defName=[] :: ?string
	,continuousCount=[] :: ?int16
	,killCount=[] :: ?int16
	,replayUID=[] :: ?int64
	,timestamp=[] :: ?int32}).
-record(sc_alien_record,{
	recordList=[] :: [#p_alien_record{}]}).
-record(sc_alien_update_times,{
	leftTimes=[] :: ?int8
	,timestamp=[] :: ?int32}).
-record(cs_alien_self_fight_replay,{
	replayUID=[] :: ?int64}).
-record(sc_alien_self_fight_replay,{
	fightInfoList=[] :: #sc_fight_request{}}).
-record(cs_alien_fight_replay,{
	replayUID=[] :: ?int64}).
-record(sc_alien_fight_repaly,{
	fightInfoList=[] :: #sc_fight_request{}}).
-record(sc_alien_new_fighter_list,{
	fighterList=[] :: [#p_alien_fighter{}]}).
-record(cs_alien_leave,{
	}).
-record(sc_alien_new_self_record,{
	}).
-record(cs_alien_view_other,{
	tarRoleID=[] :: ?int32}).
-record(p_ger_view,{
	gerQuality=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerTypeID=[] :: ?int16}).
-record(sc_alien_view_other,{
	tarRoleID1=[] :: ?int32
	,roleName1=[] :: ?string
	,roleLevel1=[] :: ?int16
	,fightPower1=[] :: ?int64
	,gerList1=[] :: [#p_ger_view{}]
	,tarRoleID2=[] :: ?int32
	,roleName2=[] :: ?string
	,roleLevel2=[] :: ?int16
	,fightPower2=[] :: ?int64
	,gerList2=[] :: [#p_ger_view{}]}).
-record(cs_alien_view_other_dtl,{
	tarRoleID=[] :: ?int32}).
-record(cs_alien_buy_times,{
	buyTimes=[] :: ?int8}).
-record(sc_alien_buy_times,{
	result=[] :: ?int8
	,newTimes=[] :: ?int8}).
-record(cs_alien_active,{
	}).
-record(sc_alien_active,{
	status=[] :: ?int8}).
-record(cs_team_pk_info,{
	}).
-record(p_team_member,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string}).
-record(sc_team_pk_open,{
	fightPower=[] :: ?int64
	,rank=[] :: ?int32
	,score=[] :: ?int32
	,refreshSelf=[] :: ?int8
	,refreshOther=[] :: ?int8
	,selfTeam=[] :: [#p_team_member{}]
	,otherTeam=[] :: [#p_team_member{}]
	,closeTimestamp=[] :: ?int32}).
-record(sc_team_pk_close,{
	fightPower=[] :: ?int64
	,score=[] :: ?int32
	,rank=[] :: ?int32
	,nextTimestamp=[] :: ?int32
	,rankList=[] :: [#p_team_member{}]}).
-record(cs_team_refresh,{
	type=[] :: ?int8}).
-record(sc_team_refresh,{
	type=[] :: ?int8
	,result=[] :: ?int8
	,list=[] :: [#p_team_member{}]}).
-record(cs_team_fight,{
	}).
-record(p_team_member2,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,isDead=[] :: boolean()}).
-record(sc_team_fight_result,{
	isWin=[] :: boolean()
	,addCoin=[] :: ?int32
	,addExp=[] :: ?int32
	,addScore=[] :: ?int32
	,oldRank=[] :: ?int32
	,newRank=[] :: ?int32
	,refreshSelf=[] :: ?int8
	,refreshOther=[] :: ?int8
	,otherList=[] :: [#p_team_member2{}]
	,selfList=[] :: [#p_team_member2{}]
	,fightInfoList=[] :: [#sc_fight_request{}]
	,selfTeam=[] :: [#p_team_member{}]
	,otherTeam=[] :: [#p_team_member{}]}).
-record(cs_team_rank,{
	}).
-record(p_team_member3,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,score=[] :: ?int32
	,rank=[] :: ?int32}).
-record(sc_team_rank,{
	selfRank=[] :: ?int32
	,rankList=[] :: [#p_team_member3{}]}).
-record(cs_team_record,{
	}).
-record(p_team_record,{
	isWin=[] :: boolean()
	,timestamp=[] :: ?int32
	,roleName=[] :: ?string
	,godName=[] :: ?string
	,replayUIDList=[] :: [?int64]}).
-record(sc_team_record,{
	recordList=[] :: [#p_team_record{}]}).
-record(cs_team_self_record,{
	}).
-record(p_team_self_record,{
	timestamp=[] :: ?int32
	,isWin=[] :: boolean()
	,addExp=[] :: ?int32
	,addCoin=[] :: ?int32
	,addScore=[] :: ?int32
	,selfNameList=[] :: [?string]
	,otherNameList=[] :: [?string]
	,replayUIDList=[] :: [?int64]}).
-record(sc_team_self_record,{
	recordList=[] :: [#p_team_self_record{}]}).
-record(cs_team_move,{
	fromPos=[] :: ?int8
	,toPos=[] :: ?int8}).
-record(sc_team_move,{
	result=[] :: ?int8}).
-record(sc_team_pk_not_open,{
	needLevel=[] :: ?int16}).
-record(sc_team_fight_error,{
	result=[] :: ?int8}).
-record(cs_team_fight_replay,{
	replayUIDList=[] :: [?int64]}).
-record(sc_team_fight_replay,{
	result=[] :: ?int8
	,fightInfoList=[] :: [#sc_fight_request{}]
	,otherList=[] :: [#p_team_member2{}]
	,selfList=[] :: [#p_team_member2{}]}).
-record(cs_team_self_fight_replay,{
	replayUIDList=[] :: [?int64]}).
-record(sc_team_self_fight_replay,{
	result=[] :: ?int8
	,fightInfoList=[] :: [#sc_fight_request{}]
	,otherList=[] :: [#p_team_member2{}]
	,selfList=[] :: [#p_team_member2{}]}).
-record(cs_team_view_other,{
	tarRoleID=[] :: ?int32}).
-record(sc_team_view_other,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger_view{}]}).
-record(cs_team_view_other_dtl,{
	tarRoleID=[] :: ?int32}).
-record(p_lieu_view,{
	lieuGerTypeID=[] :: ?int16}).
-record(p_equip,{
	itemUID=[] :: ?int64
	,itemTypeID=[] :: ?int16
	,itemLevel=[] :: ?int8
	,itemRank=[] :: ?int8
	,itemGerID=[] :: ?int64
	,itemPos=[] :: ?int8
	,itemDecay=[] :: ?int32
	,itemExp=[] :: ?int16}).
-record(p_ger_pos,{
	gerID=[] :: ?int64
	,gerPos=[] :: ?int8}).
-record(p_ger,{
	gerID=[] :: ?int64
	,gerTypeID=[] :: ?int16
	,gerQuality=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerAttack=[] :: ?int32
	,gerHpMax=[] :: ?int32
	,gerFightPower=[] :: ?int64
	,gerExp=[] :: ?int64}).
-record(sc_team_view_other_dtl,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger{}]
	,equipList=[] :: [#p_equip{}]
	,gerPosList=[] :: [#p_ger_pos{}]
	,atkAdd=[] :: ?int16
	,hpAdd=[] :: ?int16
	,lieuViewList=[] :: [#p_lieu_view{}]}).
-record(cs_team_new_status,{
	}).
-record(sc_team_new_status,{
	isOpen=[] :: boolean()}).
-record(p_race_rec,{
	atk_name=[] :: ?string
	,def_name=[] :: ?string
	,replay_uid_list=[] :: [?int64]
	,atk_role_id=[] :: ?int32
	,def_role_id=[] :: ?int32
	,atk_fight_power=[] :: ?int64
	,def_fight_power=[] :: ?int64
	,round=[] :: ?int8
	,group_id=[] :: ?int8
	,atk_is_male=[] :: boolean()
	,def_is_male=[] :: boolean()
	,atk_title=[] :: ?int8
	,def_title=[] :: ?int8
	,atk_head=[] :: ?int32
	,def_head=[] :: ?int32
	,win_or_lose_list=[] :: [boolean()]}).
-record(sc_race_new_fight,{
	new_fight=[] :: #p_race_rec{}}).
-record(p_race_fighter,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,fight_power=[] :: ?int64
	,role_level=[] :: ?int16
	,is_male=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32}).
-record(cs_race_history,{
	round=[] :: ?int8
	,group_id=[] :: ?int8
	,start=[] :: ?int16
	,num=[] :: ?int16}).
-record(sc_race_history,{
	round=[] :: ?int8
	,group_id=[] :: ?int8
	,history_list=[] :: [#p_race_rec{}]}).
-record(cs_race_replay,{
	replay_id=[] :: ?int64}).
-record(sc_race_replay,{
	result=[] :: ?int8
	,fight_info=[] :: #sc_fight_request{}}).
-record(cs_race_fight_list,{
	group_id=[] :: ?int8}).
-record(sc_race_fight_list,{
	group_id=[] :: ?int8
	,fighter_list=[] :: [#p_race_fighter{}]}).
-record(cs_race_sign,{
	}).
-record(sc_race_sign,{
	reason_code=[] :: ?int8}).
-record(cs_race_info,{
	}).
-record(p_race_pos,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,is_male=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,pos=[] :: ?int8}).
-record(sc_race_info,{
	status=[] :: ?int8
	,timestamp=[] :: ?int32
	,session_id=[] :: ?int16
	,is_sign=[] :: boolean()
	,list=[] :: [#p_race_pos{}]
	,champion_name=[] :: ?string
	,self_group_id=[] :: ?int8
	,is_auto=[] :: boolean()}).
-record(cs_race_enter,{
	}).
-record(cs_race_leave,{
	}).
-record(cs_race_pos_history,{
	pos=[] :: ?int8}).
-record(sc_race_pos_history,{
	pos=[] :: ?int8
	,race_rec=[] :: #p_race_rec{}}).
-record(sc_race_new_first,{
	new_pos=[] :: #p_race_pos{}}).
-record(sc_race_new_status,{
	status=[] :: ?int8
	,timestamp=[] :: ?int32}).
-record(cs_race_is_open,{
	}).
-record(sc_race_is_open,{
	is_open=[] :: boolean()}).
-record(cs_race_auto_sign,{
	}).
-record(sc_race_auto_sign,{
	reason_code=[] :: ?int8}).
-record(cs_race_auto_unsign,{
	}).
-record(sc_race_auto_unsign,{
	reason_code=[] :: ?int8}).
-record(cs_race_self_history,{
	}).
-record(sc_race_self_history,{
	history_list=[] :: [#p_race_rec{}]}).
-record(cs_race_guess_info,{
	}).
-record(sc_race_guess_info,{
	guessCoin=[] :: ?int32
	,roleID=[] :: ?int32
	,coinValList=[] :: [?int32]}).
-record(cs_race_guess,{
	guessCoin=[] :: ?int32
	,roleID=[] :: ?int32}).
-record(sc_race_guess,{
	result=[] :: ?int8}).
-record(p_family_member_info,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,family_contribution=[] :: ?int32
	,left_family_contribution=[] :: ?int32
	,use_gold_time=[] :: ?int32
	,title=[] :: ?int8
	,is_male=[] :: boolean()
	,online=[] :: boolean()
	,role_level=[] :: ?int16
	,fight_power=[] :: ?int64
	,family_title=[] :: ?int8}).
-record(p_family_info,{
	family_id=[] :: ?int32
	,family_name=[] :: ?string
	,level=[] :: ?int16
	,create_role_id=[] :: ?int32
	,create_role_name=[] :: ?string
	,owner_role_id=[] :: ?int32
	,owner_role_name=[] :: ?string
	,cur_members=[] :: ?int16
	,active_points=[] :: ?int32
	,notice=[] :: ?string
	,members=[] :: [#p_family_member_info{}]
	,rank=[] :: ?int32
	,create_time=[] :: ?int32}).
-record(p_family_request,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,level=[] :: ?int16
	,fight_power=[] :: ?int64
	,timestamp=[] :: ?int32
	,family_id=[] :: ?int32}).
-record(p_family_summary,{
	family_id=[] :: ?int32
	,family_name=[] :: ?string
	,owner_role_name=[] :: ?string
	,cur_members=[] :: ?int16
	,level=[] :: ?int16
	,rank=[] :: ?int32
	,notice=[] :: ?string
	,is_request=[] :: boolean()
	,owner_role_id=[] :: ?int32}).
-record(cs_family_get_list,{
	start=[] :: ?int16
	,num=[] :: ?int16}).
-record(sc_family_get_list,{
	result=[] :: ?int8
	,family_list=[] :: [#p_family_summary{}]}).
-record(cs_family_create,{
	family_name=[] :: ?string
	,is_gold_create=[] :: boolean()}).
-record(sc_family_create,{
	result=[] :: ?int8
	,family_info=[] :: #p_family_info{}
	,timestamp=[] :: ?int32}).
-record(cs_family_request_join,{
	family_id=[] :: ?int32}).
-record(sc_family_request_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,timestamp=[] :: ?int32}).
-record(cs_family_cancel_join,{
	family_id=[] :: ?int32}).
-record(sc_family_cancel_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()}).
-record(cs_family_agree_join,{
	role_id=[] :: ?int32}).
-record(sc_family_agree_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,family_info=[] :: #p_family_info{}}).
-record(cs_family_refuse_join,{
	role_id=[] :: ?int32}).
-record(sc_family_refuse_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()}).
-record(cs_family_get_info,{
	}).
-record(sc_family_get_info,{
	result=[] :: ?int8
	,family_info=[] :: #p_family_info{}}).
-record(cs_family_kick,{
	kick_role_id=[] :: ?int32}).
-record(sc_family_kick,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,family_info=[] :: #p_family_info{}}).
-record(cs_family_create_consume,{
	}).
-record(sc_family_create_consume,{
	need_coin=[] :: ?int32
	,need_gold=[] :: ?int32}).
-record(cs_family_leave,{
	}).
-record(sc_family_leave,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,family_info=[] :: #p_family_info{}}).
-record(cs_family_change_notice,{
	notice=[] :: ?string}).
-record(sc_family_change_notice,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,notice=[] :: ?string}).
-record(cs_family_request_list,{
	}).
-record(sc_family_request_list,{
	result=[] :: ?int8
	,request_list=[] :: [#p_family_request{}]}).
-record(sc_family_del_request,{
	role_id=[] :: ?int32}).
-record(cs_combine_do,{
	combineType=[] :: ?int8
	,combineTypeID=[] :: ?int16
	,combineOutType=[] :: ?int8
	,uIDList=[] :: [?int64]}).
-record(sc_combine_fail,{
	result=[] :: ?int8}).
-record(p_newGer,{
	gerTypeID=[] :: ?int16
	,gerLevel=[] :: ?int8
	,gerQuality=[] :: ?int8}).
-record(sc_combine_ger,{
	newGer=[] :: [#p_newGer{}]}).
-record(p_newEquip,{
	itemTypeID=[] :: ?int16
	,itemNum=[] :: ?int16
	,itemLevel=[] :: ?int8
	,itemRank=[] :: ?int8}).
-record(sc_combine_equip,{
	newEquip=[] :: [#p_newEquip{}]}).
-record(cs_combine_info,{
	}).
-record(sc_combine_info,{
	stopTime=[] :: ?int32
	,content=[] :: ?string
	,gerStarList=[] :: [?int8]
	,equipStarList=[] :: [?int8]}).
-record(cs_firecracker_open,{
	}).
-record(p_discount,{
	amount=[] :: ?int32
	,discount=[] :: ?int8}).
-record(sc_firecracker_open,{
	status=[] :: ?int8
	,name=[] :: ?string
	,description=[] :: ?string
	,icon=[] :: ?string
	,startTime=[] :: ?int32
	,rewardTime=[] :: ?int32
	,closeTime=[] :: ?int32
	,total=[] :: ?int32
	,markedPrice=[] :: ?int8
	,tradedPrice=[] :: ?int8
	,count=[] :: ?int32
	,rank=[] :: ?int8
	,canReward=[] :: ?int8
	,returnGold=[] :: ?int16
	,discounts=[] :: [#p_discount{}]}).
-record(sc_firecracker_info_sync,{
	total=[] :: ?int32
	,tradedPrice=[] :: ?int8}).
-record(cs_firecracker_close,{
	}).
-record(cs_firecracker_setoff,{
	type=[] :: ?int8}).
-record(p_item_view,{
	itemTypeID=[] :: ?int16
	,itemLevel=[] :: ?int8
	,itemRank=[] :: ?int8
	,itemNum=[] :: ?int16}).
-record(p_reward_info,{
	coin=[] :: ?int32
	,roleExp=[] :: ?int32
	,gerExp=[] :: ?int32
	,gold=[] :: ?int32
	,reputation=[] :: ?int32
	,itemList=[] :: [#p_item_view{}]
	,gerList=[] :: [#p_ger_view{}]}).
-record(sc_firecracker_setoff,{
	result=[] :: ?int8
	,count=[] :: ?int32
	,returnGold=[] :: ?int16
	,canReward=[] :: ?int8
	,rewardInfo=[] :: [#p_reward_info{}]}).
-record(cs_firecracker_rank,{
	}).
-record(p_firecracker_rank,{
	rank=[] :: ?int8
	,name=[] :: ?string
	,count=[] :: ?int32
	,rewardInfo=[] :: [#p_reward_info{}]}).
-record(sc_firecracker_rank,{
	rankList=[] :: [#p_firecracker_rank{}]}).
-record(cs_firecracker_get_reward,{
	}).
-record(sc_firecracker_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_treaHouse_get_list,{
	}).
-record(p_treaHouse_card,{
	pos=[] :: ?int8
	,posType=[] :: ?int8
	,cardType=[] :: ?int8
	,count=[] :: ?int8
	,value=[] :: ?int32}).
-record(p_baseBoxOpenInfo,{
	pos=[] :: ?int8
	,isOpen=[] :: ?int8}).
-record(sc_treaHouse_get_list,{
	isOpen=[] :: ?int8
	,freeTimes=[] :: ?int8
	,boxProcess=[] :: ?int8
	,boxOpenProcess=[] :: [#p_baseBoxOpenInfo{}]
	,endTime=[] :: ?int32
	,cardList=[] :: [#p_treaHouse_card{}]
	,oneTimeNeedGold=[] :: ?int16
	,refreshNeedCoin=[] :: ?int16
	,stopTime=[] :: ?int32}).
-record(cs_treaHouse_is_open,{
	}).
-record(sc_treaHouse_is_open,{
	type=[] :: ?int8}).
-record(cs_treaHouse_explore_one,{
	}).
-record(p_treaHouse_card_oneTime,{
	openCardList=[] :: [#p_treaHouse_card{}]
	,newCardList=[] :: [#p_treaHouse_card{}]}).
-record(sc_treaHouse_explore_one,{
	type=[] :: ?int8
	,boxProcess=[] :: ?int8
	,info=[] :: [#p_treaHouse_card_oneTime{}]}).
-record(cs_treaHouse_explore_ten,{
	}).
-record(sc_treaHouse_explore_ten,{
	type=[] :: ?int8
	,openTimes=[] :: ?int8
	,boxProcess=[] :: ?int8
	,infoList=[] :: [#p_treaHouse_card_oneTime{}]}).
-record(cs_treaHouse_refresh,{
	}).
-record(sc_treaHouse_refresh,{
	type=[] :: ?int8
	,cardList=[] :: [#p_treaHouse_card{}]}).
-record(cs_treaHouse_open_base_box,{
	pos=[] :: ?int8}).
-record(sc_treaHouse_open_base_box,{
	type=[] :: ?int8
	,boxOpenProcess=[] :: [#p_baseBoxOpenInfo{}]}).
-record(p_treaHouse_ranker,{
	type=[] :: ?int8
	,rankNum=[] :: ?int16
	,mark=[] :: ?int32
	,roleName=[] :: ?string
	,rewardInfo=[] :: #p_reward_info{}}).
-record(cs_treaHouse_get_rankInfo,{
	}).
-record(sc_treaHouse_get_rankInfo,{
	type=[] :: ?int8
	,isGetRankReward=[] :: ?int8
	,selfInfo=[] :: #p_treaHouse_ranker{}
	,rankInfoList=[] :: [#p_treaHouse_ranker{}]}).
-record(cs_treaHouse_get_rank_Reward,{
	}).
-record(sc_treaHouse_get_rank_Reward,{
	type=[] :: ?int8
	,rank=[] :: ?int8
	,rewardInfo=[] :: #p_reward_info{}}).
-record(cs_treaHouse_get_baseBoxRewardInfo,{
	}).
-record(p_treaHouse_BaseReward_Info,{
	pos=[] :: ?int8
	,needMark=[] :: ?int32
	,rewardInfo=[] :: #p_reward_info{}}).
-record(sc_treaHouse_get_baseBoxRewardInfo,{
	baseReaward_boxInfoList=[] :: [#p_treaHouse_BaseReward_Info{}]}).
-record(sc_treaHouse_change_state,{
	}).
-record(cs_challengeGod_info,{
	}).
-record(sc_challengeGod_info,{
	freeTimes=[] :: ?int16
	,buyTimes=[] :: ?int16
	,gerPos=[] :: ?int8
	,price=[] :: ?int8}).
-record(cs_challengeGod_select_ger,{
	pos=[] :: ?int8}).
-record(sc_challengeGod_select_ger,{
	result=[] :: ?int8}).
-record(cs_challengeGod_challenge_dungeon_one,{
	dungeonID=[] :: ?int16}).
-record(p_ger_add_exp,{
	gerPos=[] :: ?int8
	,addExp=[] :: ?int32
	,isUpgraded=[] :: boolean()}).
-record(p_reward,{
	coin=[] :: ?int32
	,roleExp=[] :: ?int32
	,gerExpList=[] :: [#p_ger_add_exp{}]
	,gold=[] :: ?int32
	,itemList=[] :: [#p_item_view{}]
	,gerList=[] :: [#p_ger_view{}]
	,reputation=[] :: ?int32}).
-record(sc_challengeGod_challenge_dungeon_one,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_reward{}]}).
-record(cs_challengeGod_challenge_dungeon_ten,{
	dungeonID=[] :: ?int16}).
-record(p_challengeGod_result,{
	result=[] :: ?int8
	,reward=[] :: #p_reward{}}).
-record(sc_challengeGod_challenge_ten,{
	result=[] :: ?int8
	,info=[] :: [#p_challengeGod_result{}]}).
-record(cs_talk_world,{
	channel=[] :: ?int8
	,talkMessage=[] :: ?string}).
-record(sc_talk_world,{
	result=[] :: ?int8
	,channel=[] :: ?int8}).
-record(sc_talk_message,{
	channel=[] :: ?int8
	,roleName=[] :: ?string
	,message=[] :: ?string
	,roleTitle=[] :: ?int8
	,timeStamp=[] :: ?int64
	,roleID=[] :: ?int32
	,familyTitle=[] :: ?int8
	,location=[] :: ?string
	,isMale=[] :: boolean()
	,head=[] :: ?int32}).
-record(cs_talk_gag_one,{
	roleName=[] :: ?string}).
-record(cs_talk_ungag_one,{
	roleName=[] :: ?string}).
-record(cs_talk_get_gag_list,{
	}).
-record(sc_talk_get_gag_list,{
	nameList=[] :: [?string]}).
-record(cs_talk_recent_list,{
	channel=[] :: ?int8}).
-record(sc_talk_recent_list,{
	channel=[] :: ?int8
	,list=[] :: [#sc_talk_message{}]}).
-record(cs_talk_person,{
	roleID=[] :: ?int32
	,talkMessage=[] :: ?string}).
-record(sc_talk_person,{
	result=[] :: ?int8}).
-record(cs_talk_person_offline,{
	}).
-record(sc_talk_person_offline,{
	list=[] :: [#sc_talk_message{}]}).
-record(cs_talk_gm,{
	tarRoleID=[] :: ?int32}).
-record(sc_talk_gm,{
	result=[] :: ?int8}).
-record(sc_push_highlight_Info,{
	value=[] :: ?int8
	,type=[] :: ?int8}).
-record(cs_nanm_open,{
	}).
-record(sc_nanm_open,{
	isOpen=[] :: boolean()
	,maxHp=[] :: ?int64
	,bossQuality=[] :: ?int16
	,isBuffed=[] :: boolean()
	,buffNum=[] :: ?int16
	,isOfflinePlay=[] :: boolean()
	,beginTime=[] :: ?int32}).
-record(sc_nanm_init_state,{
	curHp=[] :: ?int64
	,curHarm=[] :: ?int64
	,curRank=[] :: ?int32
	,rebornTime=[] :: ?int32}).
-record(cs_nanm_close,{
	}).
-record(cs_nanm_buff,{
	type=[] :: ?int8}).
-record(sc_nanm_buff,{
	type=[] :: ?int8
	,result=[] :: ?int8}).
-record(cs_nanm_last_info,{
	curSavedInfoID=[] :: ?int32}).
-record(sc_nanm_last_info_ignore,{
	}).
-record(p_nanm_info,{
	roleName=[] :: ?string
	,harmValue=[] :: ?int64}).
-record(sc_nanm_last_info_win,{
	bossQuality=[] :: ?int16
	,intervalSec=[] :: ?int32
	,bossMaxHp=[] :: ?int64
	,nanmInfolist=[] :: [#p_nanm_info{}]
	,luckyRoleList=[] :: [?string]}).
-record(sc_nanm_last_info_fail,{
	bossQuality=[] :: ?int16
	,intervalSec=[] :: ?int32}).
-record(cs_nanm_cur_info,{
	}).
-record(sc_nanm_cur_info_ignore,{
	}).
-record(sc_nanm_cur_info,{
	nanmInfoList=[] :: [#p_nanm_info{}]}).
-record(sc_nanm_hp_sync,{
	bossHp=[] :: ?int64}).
-record(p_nanm_harm,{
	name=[] :: ?string
	,harm=[] :: ?int64}).
-record(sc_nanm_harm_broadcast,{
	harmList=[] :: [#p_nanm_harm{}]}).
-record(sc_nanm_buff_sync,{
	buffNum=[] :: ?int16}).
-record(sc_nanm_stop,{
	type=[] :: ?int8
	,roleSta=[] :: #p_role_stastic{}}).
-record(cs_nanm_rank_sync,{
	}).
-record(sc_nanm_rank_sync,{
	curRank=[] :: ?int16}).
-record(cs_nanm_fight,{
	}).
-record(sc_nanm_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,rebornTime=[] :: ?int32
	,rewardCoin=[] :: ?int32
	,rewardReputation=[] :: ?int32}).
-record(cs_nanm_reborn,{
	}).
-record(sc_nanm_reborn,{
	result=[] :: ?int8}).
-record(cs_nanm_offline_play,{
	openFlag=[] :: boolean()}).
-record(sc_nanm_offline_play,{
	result=[] :: ?int8
	,newOpenFlag=[] :: boolean()}).
-record(cs_nanm_open_time,{
	}).
-record(sc_nanm_open_time,{
	beginTime=[] :: ?int32}).
-record(cs_version,{
	version=[] :: ?string}).
-record(sc_version,{
	result=[] :: ?int8}).
-record(cs_gift_request,{
	code=[] :: ?string}).
-record(sc_gift_request,{
	result=[] :: ?int8
	,rewardInfo=[] :: [#p_reward_info{}]}).
-record(cs_box_item,{
	itemTypeID=[] :: ?int16
	,num=[] :: ?int8}).
-record(sc_box_item,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]
	,itemTypeID=[] :: ?int16
	,num=[] :: ?int8}).
-record(cs_box_shop,{
	tab=[] :: ?int32
	,type=[] :: ?int8}).
-record(p_reward_view2,{
	type=[] :: ?int8
	,typeID=[] :: ?int16
	,num=[] :: ?int16}).
-record(sc_box_shop,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view2{}]}).
-record(cs_box_shop_info,{
	}).
-record(p_shop_box_info,{
	itemTypeID=[] :: ?int16
	,valueOne=[] :: ?int32
	,valueTen=[] :: ?int32
	,isOpenActivity=[] :: ?int8
	,discount=[] :: ?int32
	,endtime=[] :: ?int32}).
-record(sc_box_shop_info,{
	info=[] :: [#p_shop_box_info{}]
	,stopTime=[] :: ?int32}).
-record(p_reward_view3,{
	type=[] :: ?int8
	,typeID=[] :: ?int16
	,num=[] :: ?int32}).
-record(cs_box_get_spirit_equip_count,{
	}).
-record(sc_box_get_spirit_equip_count,{
	count1=[] :: ?int32
	,count2=[] :: ?int32
	,needItemTypeID1=[] :: ?int32
	,needNum1=[] :: ?int32
	,needItemTypeID2=[] :: ?int32
	,needNum2=[] :: ?int32}).
-record(cs_activity_get_list,{
	}).
-record(p_activity_icon,{
	activityID=[] :: ?int16
	,iconRrc=[] :: ?string
	,activityName=[] :: ?string}).
-record(sc_activity_get_list,{
	iconList=[] :: [#p_activity_icon{}]}).
-record(p_activity_ext,{
	activityID=[] :: ?int32
	,endTimestamp=[] :: ?int32
	,title=[] :: ?string
	,content=[] :: ?string}).
-record(cs_activity_info,{
	activityID=[] :: ?int16}).
-record(p_activity_draw,{
	drawID=[] :: ?int16
	,description=[] :: ?string
	,maxDrawTimes=[] :: ?int16
	,alreadyDrawTimes=[] :: ?int16
	,canDrawTimes=[] :: ?int16
	,rewardInfo=[] :: #p_reward_info{}
	,needMaterial=[] :: [#p_reward_view{}]}).
-record(sc_activity_info,{
	activityID=[] :: ?int16
	,type=[] :: ?int8
	,description=[] :: ?string
	,drawList=[] :: [#p_activity_draw{}]
	,startTime=[] :: ?int32
	,stopTime=[] :: ?int32
	,typeValue=[] :: ?int32
	,isForever=[] :: ?int8
	,isDailyRefresh=[] :: ?int8}).
-record(cs_activity_month,{
	}).
-record(sc_activity_month,{
	dayPayGold=[] :: ?int32
	,leftDays=[] :: ?int8
	,isDraw=[] :: boolean()
	,needPayGold=[] :: ?int16
	,monthPrice=[] :: ?int16
	,dayGetGold=[] :: ?int16}).
-record(cs_activity_month_buy,{
	}).
-record(sc_activity_month_buy,{
	result=[] :: ?int8}).
-record(cs_activity_month_draw,{
	}).
-record(sc_activity_month_draw,{
	result=[] :: ?int8}).
-record(cs_activity_draw,{
	activityID=[] :: ?int16
	,drawID=[] :: ?int16}).
-record(sc_activity_draw,{
	result=[] :: ?int8
	,activityID=[] :: ?int16
	,drawID=[] :: ?int16
	,alreadyDrawTimes=[] :: ?int16
	,canDrawTimes=[] :: ?int16}).
-record(sc_activity_update,{
	activityID=[] :: ?int16
	,drawID=[] :: ?int16
	,canDrawTimes=[] :: ?int16}).
-record(sc_activity_record_update,{
	activityID=[] :: ?int16
	,typeValue=[] :: ?int32}).
-record(p_energy_activity,{
	startTime=[] :: ?int32
	,endTime=[] :: ?int32
	,energyMax=[] :: ?int8
	,isGet=[] :: ?int8
	,energyMin=[] :: ?int8
	,oneClickGet=[] :: ?int8
	,clickSeconds=[] :: ?int8}).
-record(cs_activity_energy,{
	}).
-record(sc_activity_energy,{
	activityList=[] :: [#p_energy_activity{}]}).
-record(cs_activity_sign_emperor_info,{
	}).
-record(sc_activity_sign_emperor_info,{
	isSign=[] :: ?int8
	,signDays=[] :: ?int8
	,isGetBox=[] :: ?int8
	,isEmperor=[] :: ?int8
	,emperorName=[] :: ?string}).
-record(cs_activity_sign_get_reward,{
	}).
-record(sc_activity_sign_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_activity_sign_up,{
	}).
-record(sc_activity_sign_up,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_activity_rebate_info,{
	}).
-record(p_rebate_info,{
	type=[] :: ?int8
	,ratio=[] :: ?int8
	,amount=[] :: ?int32}).
-record(p_rebate_list,{
	rebateID=[] :: ?int8
	,name=[] :: ?string
	,status=[] :: ?int8
	,startTime=[] :: ?int32
	,closeTime=[] :: ?int32
	,rewardTime=[] :: ?int32
	,rebateInfo=[] :: [#p_rebate_info{}]}).
-record(sc_rebate_info,{
	status=[] :: ?int8
	,name=[] :: ?string
	,description=[] :: ?string
	,icon=[] :: ?string
	,startTime=[] :: ?int32
	,closeTime=[] :: ?int32
	,rebateList=[] :: [#p_rebate_list{}]}).
-record(cs_activity_rebate_get_reward,{
	rebateID=[] :: ?int8}).
-record(p_rebate_reward,{
	coin=[] :: ?int32
	,gold=[] :: ?int32
	,reputation=[] :: ?int32}).
-record(sc_rebate_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_rebate_reward{}]}).
-record(sc_rebate_update,{
	}).
-record(cs_activity_levelRank_open,{
	}).
-record(levelRank_rankerInfo,{
	roleExp=[] :: ?int64
	,rankNum=[] :: ?int8
	,roleName=[] :: ?string
	,rewardInfo=[] :: #p_reward_info{}}).
-record(sc_activity_levelRank_open,{
	stopTime=[] :: ?int32
	,endTime=[] :: ?int32
	,rankerInfoList=[] :: [#levelRank_rankerInfo{}]}).
-record(cs_activity_levelRank_refresh,{
	}).
-record(sc_activity_levelRank_refresh,{
	rankerInfoList=[] :: [#levelRank_rankerInfo{}]}).
-record(cs_activity_day_pay_mul,{
	}).
-record(sc_activity_day_pay_mul,{
	mul=[] :: ?int8}).
-record(cs_invite_info,{
	}).
-record(sc_invite_info,{
	isBindWeibo=[] :: boolean()
	,isInputInviteCode=[] :: boolean()
	,inviteNum=[] :: ?int16
	,whoInviteYou=[] :: ?string
	,getFirstPayRewardNum=[] :: ?int16}).
-record(cs_invite_bind_weibo,{
	}).
-record(sc_invite_bind_weibo,{
	result=[] :: ?int8}).
-record(cs_invite_weibo_share_levelup,{
	level=[] :: ?int16}).
-record(sc_invite_weibo_share_levelup,{
	result=[] :: ?int8}).
-record(cs_invite_input_invite_code,{
	inviteCode=[] :: ?string}).
-record(sc_invite_input_invite_code,{
	result=[] :: ?int8
	,inviterName=[] :: ?string}).
-record(cs_invite_list,{
	}).
-record(p_invite,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int8
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,isPay=[] :: boolean()}).
-record(sc_invite_list,{
	inviteList=[] :: [#p_invite{}]}).
-record(cs_friend_get_list,{
	type=[] :: ?int8}).
-record(p_friend,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int8
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,fightPower=[] :: ?int64
	,logoutTime=[] :: ?int32
	,location=[] :: ?string
	,head=0 :: ?int32
	,matingCoolSecond=0 :: ?int32
	,gerTypeID=0 :: ?int32
	,gerQuality=0 :: ?int16
	,canGive=0 :: ?int8
	,canSend=0 :: ?int8
	,sendS=0 :: ?int32
	,beginGold=0 :: ?int32
	,endGold=0 :: ?int32
	,beginBadge=0 :: ?int32
	,endBadge=0 :: ?int32
	,isFight=[] :: boolean()}).
-record(sc_friend_get_list,{
	type=[] :: ?int8
	,roleInfoList=[] :: [#p_friend{}]
	,giveTimes=0 :: ?int8
	,allTimes=20 :: ?int8
	,fightTimes=0 :: ?int8}).
-record(cs_friend_fight,{
	roleID=[] :: ?int32}).
-record(sc_friend_fight,{
	result=0 :: ?int8
	,addCoin=0 :: ?int32
	,addRepu=0 :: ?int32
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_friend_get_add_list,{
	}).
-record(p_stranger,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int8
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,fightPower=[] :: ?int64
	,logoutTime=[] :: ?int32
	,location=[] :: ?string
	,head=0 :: ?int32
	,canAdd=0 :: ?int8}).
-record(sc_friend_get_add_list,{
	roleList=[] :: [#p_stranger{}]}).
-record(cs_friend_add,{
	roleIDList=[] :: [?int32]}).
-record(sc_friend_add,{
	result=[] :: ?int8}).
-record(p_friend_add,{
	roleID=[] :: ?int32
	,timestamp=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int8
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,fightPower=[] :: ?int64
	,head=0 :: ?int32}).
-record(cs_friend_add_list,{
	}).
-record(sc_friend_add_list,{
	list=[] :: [#p_friend_add{}]}).
-record(sc_friend_new_add,{
	list=[] :: [#p_friend_add{}]}).
-record(cs_friend_agree,{
	roleIDList=[] :: [?int32]}).
-record(sc_friend_agree,{
	result=[] :: ?int8}).
-record(cs_friend_refuse,{
	roleIDList=[] :: [?int32]}).
-record(sc_friend_refuse,{
	result=[] :: ?int8}).
-record(cs_friend_explore,{
	name=[] :: ?string}).
-record(sc_friend_explore,{
	roleInfoList=[] :: [#p_stranger{}]}).
-record(cs_friend_delete,{
	type=[] :: ?int8
	,roleID=[] :: ?int32}).
-record(sc_friend_delete,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,roleID=[] :: ?int32}).
-record(sc_friend_notify_delete,{
	type=[] :: ?int8
	,roleID=[] :: ?int32}).
-record(sc_friend_new,{
	type=[] :: ?int8
	,newFriend=[] :: #p_friend{}}).
-record(cs_friend_send_enargy,{
	roleIDList=[] :: [?int32]}).
-record(sc_friend_send_enargy,{
	result=1 :: ?int8
	,roleID=0 :: ?int32}).
-record(sc_friend_send_enargy_me,{
	roleID=[] :: ?int32}).
-record(cs_friend_give_enargy,{
	roleIDList=[] :: [?int32]}).
-record(sc_friend_give_enargy,{
	result=[] :: ?int8
	,roleIDList=[] :: [?int32]
	,giveTimes=0 :: ?int8}).
-record(sc_frend_give_enargy_me,{
	roleID=[] :: ?int32
	,canSend=[] :: ?int8}).
-record(cs_friend_give_all_enargy,{
	}).
-record(sc_friend_remove_request,{
	}).
-record(cs_gather_get_list,{
	type=[] :: ?int8}).
-record(sc_gather_get_list,{
	type=[] :: ?int8
	,idList=[] :: [?int32]}).
-record(sc_gather_new,{
	type=[] :: ?int8
	,newIDList=[] :: [?int32]}).
-record(cs_hist_get_list,{
	type=[] :: ?int8}).
-record(sc_hist_get_list,{
	type=[] :: ?int8
	,isDiscardData=[] :: boolean()
	,historyList=[] :: [#p_hist{}]
	,unreadNum=[] :: ?int16}).
-record(sc_hist_new,{
	list=[] :: [#p_hist{}]}).
-record(cs_hist_replay,{
	histUID=[] :: ?int64
	,type=[] :: ?int8}).
-record(sc_hist_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(sc_hist_unreadNum,{
	type=[] :: ?int8
	,unreadNum=[] :: ?int16}).
-record(cs_mail_info,{
	type=[] :: ?int8
	,clientTopMailUID=[] :: ?int64}).
-record(p_mail,{
	mailUID=[] :: ?int64
	,mailType=[] :: ?int8
	,senderID=[] :: ?int32
	,senderName=[] :: ?string
	,content=[] :: ?string
	,time=[] :: ?int32
	,mailTemplateID=[] :: ?int16
	,paramList=[] :: [any()]
	,mailReward=[] :: [#p_mail_reward{}]}).
-record(sc_mail_info,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,isFuckData=[] :: boolean()
	,mailList=[] :: [#p_mail{}]
	,newMailNum=[] :: [?int8]}).
-record(cs_mail_draw_reward,{
	mailUID=[] :: ?int64}).
-record(sc_mail_draw_reward,{
	result=[] :: ?int8}).
-record(cs_mail_delete,{
	mailUID=[] :: ?int64
	,type=[] :: ?int8}).
-record(sc_mail_delete,{
	result=[] :: ?int8}).
-record(cs_mail_new,{
	targetRoleID=[] :: ?int32
	,targetRoleName=[] :: ?string
	,content=[] :: ?string}).
-record(sc_mail_new,{
	result=[] :: ?int8}).
-record(cs_mail_unread_num,{
	}).
-record(sc_mail_unread_num,{
	newMailNum=[] :: [?int8]}).
-record(cs_mail_more,{
	type=[] :: ?int8
	,startMailUID=[] :: ?int64}).
-record(sc_mail_more,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,mailList=[] :: [#p_mail{}]
	,newMailNum=[] :: [?int8]}).
-record(cs_mail_agree_friend,{
	mailUID=[] :: ?int64}).
-record(sc_mail_agree_friend,{
	result=[] :: ?int8
	,mailUID=[] :: ?int64}).
-record(cs_mail_del_spec_mail,{
	senderID=[] :: ?int32}).
-record(sc_mail_del_spec_mail,{
	mailUIDList=[] :: [?int64]}).
-record(cs_hron_info,{
	}).
-record(sc_hron_info,{
	curDungeonNum=[] :: ?int16
	,attackAdd=[] :: ?int16
	,hpAdd=[] :: ?int16
	,dungeonID=[] :: ?int16
	,challengeTimes=[] :: ?int8
	,isOpen=[] :: boolean()
	,nextTime=[] :: ?int32
	,coinBuyTimes=[] :: ?int8
	,goldBuyTimes=[] :: ?int8
	,maxDungeonNum=[] :: ?int16}).
-record(cs_hron_buy,{
	type=[] :: ?int8}).
-record(sc_hron_buy,{
	result=[] :: ?int8
	,attackAdd=[] :: ?int16
	,hpAdd=[] :: ?int16}).
-record(cs_hron_fight,{
	}).
-record(sc_hron_fight,{
	result=[] :: ?int8
	,dungeonID=[] :: ?int16
	,curDungeonNum=[] :: ?int16
	,challengeTimes=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,rewardInfo=[] :: [#p_reward{}]}).
-record(cs_hron_raids,{
	}).
-record(sc_hron_raids,{
	result=[] :: ?int8
	,dungeonID=[] :: ?int16
	,reward=[] :: [#p_reward{}]}).
-record(cs_card_get_list,{
	}).
-record(p_card,{
	type=[] :: ?int8
	,value=[] :: ?int32}).
-record(p_opened_card,{
	pos=[] :: ?int8
	,type=[] :: ?int8
	,value=[] :: ?int32}).
-record(sc_card_get_list,{
	isOpen=[] :: boolean()
	,openedCardList=[] :: [#p_opened_card{}]
	,cardList=[] :: [#p_card{}]
	,needNumList=[] :: [?int8]
	,goldPrice=[] :: ?int32}).
-record(cs_card_draw,{
	pos=[] :: ?int8}).
-record(sc_card_draw,{
	result=[] :: ?int8
	,pos=[] :: ?int8
	,card=[] :: [#p_card{}]}).
-record(cs_card_refresh,{
	}).
-record(sc_card_refresh,{
	result=[] :: ?int8
	,cardList=[] :: [#p_card{}]}).
-record(cs_card_onekey,{
	}).
-record(sc_card_onekey,{
	result=[] :: ?int8
	,card=[] :: [#p_opened_card{}]}).
-record(cs_daily_get_list,{
	}).
-record(p_daily,{
	type=[] :: ?int8
	,value=[] :: ?int8
	,isDrawed=[] :: boolean()}).
-record(sc_daily_get_list,{
	dailyList=[] :: [#p_daily{}]
	,dailyDrawList=[] :: [?int8]}).
-record(cs_daily_draw,{
	type=[] :: ?int8}).
-record(sc_daily_draw,{
	result=[] :: ?int8
	,newDaily=[] :: #p_daily{}
	,dailyDrawList=[] :: [?int8]}).
-record(cs_daily_reward_list,{
	}).
-record(p_daily_reward,{
	needValue=[] :: ?int32
	,isGet=[] :: boolean()
	,reward=[] :: #p_mail_reward{}}).
-record(p_daily_reward_info,{
	type=[] :: ?int8
	,nowValue=[] :: ?int32
	,list=[] :: [#p_daily_reward{}]}).
-record(sc_daily_reward_list,{
	list=[] :: [#p_daily_reward_info{}]}).
-record(cs_daily_reward_get,{
	type=[] :: ?int8
	,needValue=[] :: ?int32}).
-record(sc_daily_reward_get,{
	type=[] :: ?int8
	,needValue=[] :: ?int32
	,result=[] :: ?int8}).
-record(cs_daily_vip_info,{
	}).
-record(sc_daily_vip_info,{
	isDraw=[] :: boolean()
	,reward=[] :: #p_mail_reward{}}).
-record(cs_daily_vip_draw,{
	}).
-record(sc_daily_vip_draw,{
	result=[] :: ?int8}).
-record(cs_pvp_get_list,{
	}).
-record(p_pvp,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int8
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,fightPower=[] :: ?int64
	,rank=[] :: ?int16
	,head=0 :: ?int32}).
-record(sc_pvp_get_list,{
	rank=[] :: ?int16
	,pvpList=[] :: [#p_pvp{}]}).
-record(cs_pvp_fight,{
	roleID=[] :: ?int32
	,rank=[] :: ?int16}).
-record(sc_pvp_fight,{
	result=[] :: ?int8
	,newRank=[] :: ?int16
	,addRepu=[] :: ?int32
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_pvp_get_first_eight_replays,{
	}).
-record(p_pvp_replay_info,{
	attackerName=[] :: ?string
	,defenderName=[] :: ?string
	,attackerNewRank=[] :: ?int16
	,defenderNewRank=[] :: ?int16
	,replayUID=[] :: ?int64
	,time=[] :: ?int32}).
-record(sc_pvp_get_first_eight_replays,{
	infoList=[] :: [#p_pvp_replay_info{}]}).
-record(cs_pvp_eight_replay,{
	replayUID=[] :: ?int64}).
-record(sc_pvp_eight_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: #sc_fight_request{}}).
-record(cs_shop_buy_num,{
	}).
-record(p_shop_num,{
	shopID=[] :: ?int16
	,sellID=[] :: ?int16
	,buyNum=[] :: ?int16}).
-record(sc_shop_buy_num,{
	shopNumList=[] :: [#p_shop_num{}]}).
-record(cs_shop_buy,{
	shopID=[] :: ?int16
	,sellID=[] :: ?int16
	,num=[] :: ?int8}).
-record(sc_shop_buy,{
	result=[] :: ?int8}).
-record(cs_shop_encounter,{
	}).
-record(p_shop_random,{
	shopID=[] :: ?int16
	,refreshSec=[] :: ?int32
	,sellIDList=[] :: [?int16]}).
-record(sc_shop_encounter,{
	shopList=[] :: [#p_shop_random{}]}).
-record(sc_shop_new,{
	newShop=[] :: #p_shop_random{}}).
-record(cs_shop_refresh,{
	shopID=[] :: ?int16}).
-record(sc_shop_refresh,{
	result=[] :: ?int8
	,newShop=[] :: [#p_shop_random{}]}).
-record(sc_shop_auto_refresh,{
	updateShop=[] :: #p_shop_random{}}).
-record(cs_shop_treasure_info,{
	}).
-record(p_treasure,{
	type=[] :: ?int8
	,typeID=[] :: ?int16
	,num=[] :: ?int32
	,costType=[] :: ?int8
	,costVal=[] :: ?int32
	,isBuy=[] :: boolean()
	,index=[] :: ?int8}).
-record(sc_shop_treasure_info,{
	activityName=[] :: ?string
	,activityEndTime=[] :: ?int32
	,mul=[] :: ?int8
	,discounts=[] :: ?int16
	,nextRefreshTime=[] :: ?int32
	,list=[] :: [#p_treasure{}]}).
-record(cs_shop_treasure_buy,{
	index=[] :: ?int8}).
-record(sc_shop_treasure_buy,{
	result=[] :: ?int8
	,index=[] :: ?int8}).
-record(sc_shop_treasure_new_activity,{
	activityName=[] :: ?string
	,activityEndTime=[] :: ?int32
	,mul=[] :: ?int8
	,discounts=[] :: ?int16}).
-record(sc_shop_treasure_new_shop,{
	nextRefreshTime=[] :: ?int32
	,list=[] :: [#p_treasure{}]}).
-record(cs_shop_refresh2,{
	}).
-record(sc_shop_refresh2,{
	result=[] :: ?int8
	,list=[] :: [#p_treasure{}]}).
-record(cs_item_bag,{
	}).
-record(p_item,{
	itemUID=[] :: ?int64
	,itemTypeID=[] :: ?int16
	,itemLevel=[] :: ?int8
	,itemRank=[] :: ?int8
	,itemNum=[] :: ?int16
	,itemDecay=[] :: ?int32
	,itemExp=[] :: ?int16}).
-record(sc_item_bag,{
	allItem=[] :: [#p_item{}]}).
-record(cs_item_equip,{
	}).
-record(sc_item_equip,{
	allEquip=[] :: [#p_equip{}]}).
-record(cs_item_sell,{
	itemUIDList=[] :: [?int64]}).
-record(sc_item_sell,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view3{}]
	,gold=[] :: ?int32}).
-record(cs_item_down_equip,{
	gerID=[] :: ?int64
	,itemPos=[] :: ?int8}).
-record(sc_item_down_equip,{
	result=[] :: ?int8
	,gerID=[] :: ?int64
	,itemPos=[] :: ?int8}).
-record(cs_item_up_equip,{
	gerID=[] :: ?int64
	,itemPos=[] :: ?int8
	,itemUID=[] :: ?int64
	,itemGerID=[] :: ?int64}).
-record(sc_item_up_equip,{
	result=[] :: ?int8
	,gerID=[] :: ?int64
	,itemPos=[] :: ?int8
	,itemUID=[] :: ?int64}).
-record(sc_item_new,{
	newItemList=[] :: [#p_item{}]}).
-record(p_item_num_update,{
	itemUID=[] :: ?int64
	,itemNum=[] :: ?int16}).
-record(sc_item_update,{
	updateList=[] :: [#p_item_num_update{}]}).
-record(cs_item_use,{
	itemUID=[] :: ?int64
	,itemNum=[] :: ?int8}).
-record(sc_item_use,{
	result=[] :: ?int8
	,itemUID=[] :: ?int64
	,itemNum=[] :: ?int8}).
-record(sc_item_delete_notify,{
	itemUIDList=[] :: [?int64]}).
-record(cs_item_reinforce,{
	itemUID=[] :: ?int64
	,gerID=[] :: ?int64}).
-record(sc_item_reinforce,{
	result=[] :: ?int8
	,itemUID=[] :: ?int64
	,newLevel=[] :: ?int16}).
-record(cs_item_max_reinforce,{
	itemUID=[] :: ?int64
	,gerID=[] :: ?int64}).
-record(sc_item_max_reinforce,{
	result=[] :: ?int8
	,itemUID=[] :: ?int64
	,tempLevelList=[] :: [?int16]}).
-record(sc_item_update_rank,{
	itemUID=[] :: ?int64
	,newItemRank=[] :: ?int8
	,newItemDecay=[] :: ?int32}).
-record(cs_item_up_rank,{
	srcItemUID=[] :: ?int64
	,foodItemUID=[] :: ?int64
	,srcItemGerID=[] :: ?int64
	,foodItemGerID=[] :: ?int64}).
-record(sc_item_up_rank,{
	result=[] :: ?int8
	,srcItemUID=[] :: ?int64
	,foodItemUID=[] :: ?int64
	,newItemLevel=[] :: ?int8
	,newItemRank=[] :: ?int8}).
-record(sc_item_more,{
	list=[] :: [#p_item{}]}).
-record(cs_item_compound,{
	typeID=[] :: ?int16}).
-record(sc_item_compound,{
	result=[] :: ?int8
	,typeID=[] :: ?int16}).
-record(cs_item_eat,{
	itemID=[] :: ?int64
	,itemGerID=[] :: ?int64
	,foodItemIDList=[] :: [?int64]}).
-record(sc_item_eat,{
	result=[] :: ?int8
	,itemID=[] :: ?int64
	,newItemRank=[] :: ?int8
	,itemExp=[] :: ?int16}).
-record(p_all_equipment,{
	all_equipment_id=[] :: ?int32
	,all_equipment_list=[] :: [?int32]}).
-record(sc_item_all_equipment,{
	gerID=[] :: [?int64]
	,all_equipment_info_list=[] :: [#p_all_equipment{}]}).
-record(cs_item_use_info,{
	}).
-record(p_item_use_info,{
	type_id=[] :: ?int16
	,left_times=[] :: ?int8}).
-record(sc_item_use_info,{
	use_info_list=[] :: [#p_item_use_info{}]}).
-record(cs_item_up_all_equip,{
	gerID=[] :: ?int64}).
-record(sc_item_up_all_equip,{
	result=[] :: ?int8}).
-record(cs_explore_one,{
	}).
-record(sc_explore_one,{
	result=[] :: ?int8
	,itemList=[] :: [#p_id_num{}]
	,roleExp=[] :: ?int32
	,gerExp=[] :: ?int32
	,coin=[] :: ?int32
	,crit=[] :: ?int8}).
-record(p_echapter,{
	chapterID=[] :: ?int16
	,endTime=[] :: ?int32
	,value=[] :: ?int32
	,isCollected=[] :: boolean()}).
-record(cs_explore_dungeon_list,{
	chapterID=[] :: ?int16}).
-record(p_edungeon,{
	dungeonID=[] :: ?int16
	,isPassed=[] :: boolean()}).
-record(sc_explore_dungeon_list,{
	chapterID=[] :: ?int16
	,dungeonList=[] :: [#p_edungeon{}]
	,state=[] :: ?int8}).
-record(cs_explore_challenge_encounter,{
	dungeonID=[] :: ?int16}).
-record(sc_explore_challenge_encounter,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_reward{}]
	,score=[] :: ?int8
	,state=[] :: ?int8}).
-record(sc_explore_delete_encounter,{
	chapterID=[] :: ?int16}).
-record(cs_explore_giveup_encounter,{
	chapterID=[] :: ?int16}).
-record(sc_explore_giveup_encounter,{
	result=[] :: ?int8}).
-record(cs_explore_list,{
	}).
-record(sc_explore_list,{
	chapterList=[] :: [#p_echapter{}]}).
-record(cs_explore_collect,{
	chapterID=[] :: ?int16}).
-record(sc_explore_collect,{
	chapterID=[] :: ?int16
	,result=[] :: ?int8}).
-record(cs_explore_force_collect,{
	chapterID=[] :: ?int16
	,type=[] :: ?int8}).
-record(sc_explore_force_collect,{
	chapterID=[] :: ?int16
	,type=[] :: ?int8
	,result=[] :: ?int8}).
-record(cs_explore_auto_explore_check,{
	}).
-record(sc_explore_auto_explore_check,{
	result=[] :: ?int8}).
-record(cs_explore_encounter_pass_reward,{
	chapterID=[] :: ?int16}).
-record(sc_explore_encounter_pass_reward,{
	result=[] :: ?int8}).
-record(cs_explore_encounter_dungeon_state,{
	chapterID=[] :: ?int16}).
-record(sc_explore_encounter_dungeon_state,{
	result=[] :: ?int8
	,chapterID=[] :: ?int16
	,dungeonID=[] :: ?int16
	,state=[] :: ?int8}).
-record(cs_explore_free,{
	}).
-record(sc_explore_free,{
	result=[] :: ?int8}).
-record(cs_ger_info,{
	}).
-record(sc_ger_info,{
	gerList=[] :: [#p_ger{}]}).
-record(p_ger_pos_info,{
	gerID=[] :: ?int64
	,gerPos=[] :: ?int8
	,itemUIDList=[] :: [?int64]}).
-record(sc_ger_update,{
	gerID=[] :: ?int64
	,gerQuality=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerAttack=[] :: ?int32
	,gerHpMax=[] :: ?int32
	,gerFightPower=[] :: ?int64
	,gerExp=[] :: ?int64}).
-record(sc_ger_new,{
	newGer=[] :: #p_ger{}}).
-record(cs_ger_standup,{
	gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(sc_ger_standup,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(cs_ger_move_pos,{
	gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(sc_ger_move_pos,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(cs_ger_pos_list,{
	}).
-record(sc_ger_pos_list,{
	gerPosInfoList=[] :: [#p_ger_pos_info{}]}).
-record(cs_ger_sell,{
	gerIDList=[] :: [?int64]}).
-record(sc_ger_sell,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view3{}]}).
-record(cs_ger_detail,{
	gerID=[] :: ?int64}).
-record(sc_ger_detail,{
	gerID=[] :: ?int64
	,gerSpInit=[] :: ?int16
	,gerSpMax=[] :: ?int16
	,gerCritic=[] :: ?int16
	,gerCriticReduce=[] :: ?int16
	,gerDoom=[] :: ?int16
	,gerMiss=[] :: ?int16
	,gerAbsorb=[] :: ?int16
	,gerDamageBack=[] :: ?int16
	,gerReel=[] :: ?int16
	,gerReelReduce=[] :: ?int16
	,gerPhyDefBite=[] :: ?int16
	,gerPhyDef=[] :: ?int16
	,gerMagDefBite=[] :: ?int16
	,gerMagDef=[] :: ?int16}).
-record(cs_ger_view_other,{
	tarRoleID=[] :: ?int32
	,serverID=[] :: ?int16}).
-record(sc_ger_view_other,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger_view{}]}).
-record(sc_ger_update_exp,{
	gerID=[] :: ?int64
	,gerExp=[] :: ?int64}).
-record(cs_ger_eat,{
	gerID=[] :: ?int64
	,foodIDList=[] :: [?int64]}).
-record(sc_ger_eat,{
	result=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(cs_ger_up_rank,{
	srcGerID=[] :: ?int64
	,foodGerID=[] :: ?int64}).
-record(sc_ger_up_rank,{
	result=[] :: ?int8
	,srcGerID=[] :: ?int64
	,foodGerID=[] :: ?int64}).
-record(sc_ger_update_standlist,{
	posList=[] :: [#p_ger_pos{}]}).
-record(cs_ger_down,{
	gerID1=[] :: ?int64
	,gerID2=[] :: ?int64
	,gerID3=[] :: ?int64
	,gerID4=[] :: ?int64
	,gerID5=[] :: ?int64
	,gerID6=[] :: ?int64
	,gerEquip1=[] :: ?int64
	,gerEquip2=[] :: ?int64
	,gerEquip3=[] :: ?int64
	,gerEquip4=[] :: ?int64
	,gerEquip5=[] :: ?int64
	,gerEquip6=[] :: ?int64
	}).
-record(sc_ger_down,{
	result=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(sc_ger_more,{
	list=[] :: [#p_ger{}]}).
-record(sc_ger_del,{
	gerIDList=[] :: [?int64]}).
-record(p_ger_power,{
	pos=[] :: ?int8
	,fightPower=[] :: ?int64}).
-record(sc_ger_refresh_power,{
	gerPowerList=[] :: [#p_ger_power{}]}).
-record(cs_ger_view_other_dtl,{
	tarRoleID=[] :: ?int32
	,serverID=[] :: ?int16}).
-record(sc_ger_view_other_dtl,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger{}]
	,equipList=[] :: [#p_equip{}]
	,gerPosList=[] :: [#p_ger_pos{}]
	,atkAdd=[] :: ?int16
	,hpAdd=[] :: ?int16
	,lieuViewList=[] :: [#p_lieu_view{}]}).
-record(cs_ger_guard_info,{
	}).
-record(p_ger_guard_attr,{
	attrType=[] :: ?int8
	,addValue=[] :: ?int32}).
-record(p_ger_guard,{
	gerPos=[] :: ?int8
	,gerID=[] :: ?int64
	,gerTypeID=[] :: ?int16
	,gerQuality=[] :: ?int8
	,baseList=[] :: [#p_ger_guard_attr{}]
	,highList=[] :: [#p_ger_guard_attr{}]}).
-record(sc_ger_guard_info,{
	list=[] :: [#p_ger_guard{}]}).
-record(cs_ger_guard_set,{
	gerID=[] :: ?int64
	,gerPos=[] :: ?int8}).
-record(sc_ger_guard_set,{
	result=[] :: ?int8
	,gerID=[] :: ?int64
	,gerPos=[] :: ?int8
	,baseList=[] :: [#p_ger_guard_attr{}]}).
-record(cs_ger_guard_refresh,{
	gerPos=[] :: ?int8
	,lockList=[] :: [boolean()]}).
-record(sc_ger_guard_refresh,{
	result=[] :: ?int8
	,highList=[] :: [#p_ger_guard_attr{}]}).
-record(cs_ger_lieu_pos_list,{
	}).
-record(sc_ger_lieu_pos_list,{
	gerPosInfoList=[] :: [#p_ger_pos_info{}]}).
-record(cs_ger_lieu_standup,{
	gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(sc_ger_lieu_standup,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(cs_ger_lieu_untie,{
	gerPos=[] :: ?int8}).
-record(p_ger_lieu_info,{
	gerPos=[] :: ?int8
	,specialID=[] :: ?int16
	,isLock1=[] :: ?int8
	,attAddID=[] :: ?int16
	,isLock2=[] :: ?int8
	,hpAddID=[] :: ?int16
	,isLock3=[] :: ?int8}).
-record(sc_ger_lieu_untie,{
	result=[] :: ?int8
	,info=[] :: [#p_ger_lieu_info{}]}).
-record(cs_ger_lieu_info_list,{
	}).
-record(sc_ger_lieu_info_list,{
	info=[] :: [#p_ger_lieu_info{}]}).
-record(cs_ger_lieu_move_pos,{
	gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(sc_ger_lieu_move_pos,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(cs_ger_lieu_lock_clo,{
	gerPos=[] :: ?int8
	,num=[] :: ?int8}).
-record(sc_ger_lieu_lock_clo,{
	result=[] :: ?int8}).
-record(cs_ger_lieu_unlock_clo,{
	gerPos=[] :: ?int8
	,num=[] :: ?int8}).
-record(sc_ger_lieu_unlock_clo,{
	result=[] :: ?int8}).
-record(cs_ger_lieu_refresh_clo,{
	gerPos=[] :: ?int8}).
-record(sc_ger_lieu_refresh_clo,{
	result=[] :: ?int8
	,info=[] :: #p_ger_lieu_info{}}).
-record(cs_ger_lieu_tie_info,{
	}).
-record(sc_ger_lieu_tie_info,{
	posList=[] :: [?int8]}).
-record(cs_ger_lieu_refresh_freeTimes,{
	}).
-record(sc_ger_lieu_refresh_freeTimes,{
	times=[] :: ?int16}).
-record(sc_ger_new_list,{
	newGerList=[] :: [#p_ger{}]}).
-record(cs_ger_down_rank,{
	srcGerID=[] :: ?int64}).
-record(sc_ger_down_rank,{
	result=[] :: ?int8
	,add_ger_list=[] :: [#p_ger_view{}]}).
-record(cs_message_notice,{
	curMaxNoticeID=[] :: ?int32}).
-record(p_notice,{
	noticeID=[] :: ?int32
	,title=[] :: ?string
	,content=[] :: ?string}).
-record(sc_message_notice,{
	picID=[] :: ?int8
	,noticeIDList=[] :: [?int32]
	,noticeList=[] :: [#p_notice{}]
	,iconList=[] :: [#p_activity_icon{}]
	,infoList=[] :: [#sc_activity_info{}]
	,list=[] :: [#p_activity_ext{}]
	,activityIDList=[] :: [?int32]}).
-record(cs_message_certain_notice,{
	noticeIDList=[] :: ?int32}).
-record(sc_message_certain_notice,{
	noticeList=[] :: [#p_notice{}]}).
-record(sc_message_bc,{
	msg=[] :: ?string}).
-record(sc_message_bc_id,{
	msgID=[] :: ?int16}).
-record(sc_message_bc_id2,{
	msgID=[] :: ?int16
	,paramList=[] :: [any()]}).
-record(cs_message_test,{
	msg=[] :: ?string}).
-record(sc_message_test,{
	result=[] :: ?int8
	,errorMsg=[] :: ?string}).
-record(sc_message_best_card,{
	roleName=[] :: ?string
	,type=[] :: ?int8
	,value=[] :: ?int32}).
-record(sc_message_ger_upLevel,{
	roleName=[] :: ?string
	,gerInfo=[] :: #p_ger_view{}}).
-record(sc_message_item_uprank,{
	roleName=[] :: ?string
	,itemInfo=[] :: #p_item_view{}}).
-record(cs_battle_progress,{
	}).
-record(p_battle_progress,{
	type=[] :: ?int8
	,dungeonID=[] :: ?int16
	,chapterID=[] :: ?int16
	,dungeonCount=[] :: ?int16}).
-record(sc_battle_progress,{
	bpList=[] :: [#p_battle_progress{}]
	,bestPassChapterID=[] :: [?int16]}).
-record(cs_battle_info,{
	type=[] :: ?int8
	,chapterID=[] :: ?int16}).
-record(p_dungeon,{
	dungeonID=[] :: ?int16
	,restTimes=[] :: ?int16
	,bestScore=[] :: ?int8}).
-record(sc_battle_info,{
	type=[] :: ?int8
	,chapterID=[] :: ?int16
	,perfectRewarded=[] :: boolean()
	,dungeonInfo=[] :: [#p_dungeon{}]
	,dungeonCount=[] :: ?int16}).
-record(cs_battle_challenge,{
	type=[] :: ?int8
	,dungeonID=[] :: ?int16}).
-record(sc_battle_challenge,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_reward{}]
	,score=[] :: ?int8}).
-record(cs_battle_perfect_reward,{
	chapterID=[] :: ?int16}).
-record(sc_battle_perfect_reward,{
	result=[] :: ?int8}).
-record(sc_battle_broadcast_get_item,{
	roleName=[] :: ?string
	,itemTypeID=[] :: ?int32
	,num=[] :: ?int8
	,dungeonID=[] :: ?int16
	,chapterID=[] :: ?int16}).
-record(cs_battle_dungeon_raids,{
	dungeonID=[] :: ?int16}).
-record(sc_battle_dungeon_raids,{
	result=[] :: ?int8
	,raidsTimes=[] :: ?int8
	,reward=[] :: [#p_reward{}]}).
-record(cs_battle_coin_info,{
	}).
-record(sc_battle_coin_info,{
	times=[] :: ?int16
	,coolDown=[] :: ?int32}).
-record(cs_battle_coin_fight,{
	type=[] :: ?int8}).
-record(sc_battle_coin_fight,{
	result=[] :: ?int8
	,coolDown=[] :: ?int32
	,isWin=[] :: boolean()
	,coin=[] :: ?int32
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_role_info,{
	}).
-record(sc_role_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,description=[] :: ?string
	,familyID=[] :: ?int32
	,level=[] :: ?int16
	,exp=[] :: ?int64
	,coin=[] :: ?int32
	,reputation=[] :: ?int32
	,gold=[] :: ?int32
	,goldBonus=[] :: ?int32
	,goldUsed=[] :: ?int32
	,vipLevel=[] :: ?int8
	,goldTotalPaid=[] :: ?int32
	,energy=[] :: ?int16
	,energyBuyTimes=[] :: ?int16
	,nextEnergyTime=[] :: ?int32
	,discoveryTimes=[] :: ?int8
	,nextDscvTime=[] :: ?int32
	,pvpTimes=[] :: ?int8
	,ruleTimes=[] :: ?int8
	,randomPVPTimes=[] :: ?int8
	,singlePVPTimes=[] :: ?int8
	,title=[] :: ?int8
	,encounterFreeNum=[] :: ?int8
	,isPVPPushOpen=[] :: boolean()
	,isPushNightMute=[] :: boolean()
	,dscvBuyTimes=[] :: ?int16
	,pvpBuyTimes=[] :: ?int16
	,ruleBuyTimes=[] :: ?int16
	,coinBuyTimes=[] :: ?int16
	,weiboCount=[] :: ?int8
	,nextPvpTime=[] :: ?int32
	,nextRuleTime=[] :: ?int32
	,challengeGodEnergy=[] :: ?int16
	,challengeGodBuyTimes=[] :: ?int16
	,lastWeiXinShareSec=[] :: ?int32
	,head=[] :: ?int32
	,payExtReward=[] :: ?int32
	,isFailed=[] :: boolean()
	,alienTimes=[] :: ?int8
	,lastAlienTime=[] :: ?int32
	,payList=[] :: [?int16]}).
-record(sc_role_update_level,{
	level=[] :: ?int16}).
-record(sc_role_update_list,{
	updateAttrList=[] :: [#sc_role_update_level{}]}).
-record(cs_role_buy_energy,{
	type=[] :: ?int8}).
-record(sc_role_buy_energy,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,newEnergy=[] :: ?int16
	,newBuyTimes=[] :: ?int16
	,getCoin=[] :: ?int32
	,killCoin=[] :: ?int32
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(sc_role_update_exp,{
	exp=[] :: ?int64}).
-record(sc_role_update_coin,{
	coin=[] :: ?int32}).
-record(sc_role_update_reputation,{
	reputation=[] :: ?int32}).
-record(sc_role_update_gold,{
	gold=[] :: ?int32}).
-record(sc_role_update_goldBonus,{
	goldBonus=[] :: ?int32}).
-record(sc_role_update_vipLevel,{
	vipLevel=[] :: ?int8
	,challengeGodFree=[] :: ?int16
	,challengeGodBuy=[] :: ?int16}).
-record(sc_role_update_energy,{
	energy=[] :: ?int16
	,nextEnergyTime=[] :: ?int32}).
-record(sc_role_update_discoveryTimes,{
	discoveryTimes=[] :: ?int8
	,nextDscvTime=[] :: ?int32}).
-record(sc_role_update_pvpTimes,{
	pvpTimes=[] :: ?int8
	,nextPvpTime=[] :: ?int32}).
-record(sc_role_update_ruleTimes,{
	ruleTimes=[] :: ?int8
	,nextRuleTime=[] :: ?int32}).
-record(sc_role_update_randomPVPTimes,{
	randomPVPTimes=[] :: ?int8}).
-record(sc_role_update_singlePVPTimes,{
	singlePVPTimes=[] :: ?int8}).
-record(sc_role_update_goldUsed,{
	goldUsed=[] :: ?int32}).
-record(sc_role_update_title,{
	title=[] :: ?int8}).
-record(sc_role_update_encounterFreeNum,{
	encounterFreeNum=[] :: ?int8}).
-record(sc_role_update_weiboCount,{
	weiboCount=[] :: ?int8}).
-record(cs_role_setting,{
	}).
-record(sc_role_setting,{
	idList=[] :: [?int8]}).
-record(cs_role_get_energy,{
	click_times=[] :: ?int8}).
-record(sc_role_get_energy,{
	result=[] :: ?int8}).
-record(cs_role_buy_coin_value,{
	}).
-record(sc_role_buy_coin_value,{
	value=[] :: ?int32}).
-record(cs_role_weixin_share,{
	}).
-record(sc_role_update_pay_ext,{
	pay_ext=[] :: ?int32}).
-record(cs_role_suggest_open,{
	}).
-record(sc_role_suggest_open,{
	is_open=[] :: boolean()}).
-record(cs_role_suggest,{
	title=[] :: ?string
	,content=[] :: ?string}).
-record(sc_role_suggest,{
	result=[] :: ?int8}).
-record(cs_role_log_guide_state,{
	value=[] :: ?int16}).
-record(cs_role_pay_tencent,{
	value=[] :: ?int16}).
-record(sc_role_pay_tencent,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(cs_role_login_reward,{
	}).
-record(sc_role_login_reward,{
	list=[] :: [#p_mail_reward{}]}).
-record(cs_role_change_name,{
	newName=[] :: ?string}).
-record(sc_role_change_name,{
	result=[] :: ?int8}).
-record(cs_role_check_tencent,{
	}).
-record(sc_role_check_tencent,{
	result=[] :: ?int8}).
-record(cs_role_push_setting,{
	type=[] :: ?int8
	,value=[] :: ?int16}).
-record(sc_role_push_setting,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,value=[] :: ?int16}).
-record(cs_role_get_guide_state,{
	}).
-record(sc_role_get_guide_state,{
	value=[] :: ?int16}).
-record(cs_role_set_guide_state,{
	value=[] :: ?int16}).
-record(sc_role_set_guide_state,{
	result=[] :: ?int8}).
-record(cs_role_change_head,{
	head=[] :: ?int32}).
-record(sc_role_change_head,{
	result=1 :: ?int8
	,head=0 :: ?int32}).
-record(cs_role_change_location,{
	location=[] :: ?string}).
-record(cs_role_token,{
	token=[] :: ?string}).
-record(sc_role_token,{
	}).
-record(cs_role_select_ger,{
	gerTypeID=[] :: ?int16}).
-record(sc_role_select_ger,{
	result=[] :: ?int8}).
-record(cs_role_demo_fight,{
	type=[] :: ?int8}).
-record(sc_role_demo_fight,{
	type=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(sc_role_base_config,{
	energyMax=[] :: ?int8
	,dscvMax=[] :: ?int8}).
-record(cs_role_pay_ios,{
	receipt=[] :: ?string
	,payID=[] :: ?int32
	,deviceID=[] :: ?string
	,macAddr=[] :: ?string
	,type=[] :: ?int8}).
-record(sc_role_pay_ios,{
	result=[] :: ?int8
	,receipt=[] :: ?string
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(cs_role_pay_91,{
	receipt=[] :: ?string
	,payID=[] :: ?int32
	,deviceID=[] :: ?string
	,macAddr=[] :: ?string}).
-record(sc_role_pay_91,{
	result=[] :: ?int8
	,receipt=[] :: ?string
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_uc,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_dl,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_zz,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_360,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_wdj,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_dk,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_mi,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_az,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(cs_account_login,{
	userID=[] :: ?int32
	,unixTime=[] :: ?int32
	,accountName=[] :: ?string
	,ticket=[] :: ?string
	,macAddr=[] :: ?string
	,serverID=[] :: ?int16
	,deviceID=[] :: ?string
	,srcType=[] :: ?int16}).
-record(sc_account_login,{
	result=[] :: ?int16
	,isCreated=[] :: boolean()
	,isGerSelected=[] :: boolean()
	,guiCreateRole=[] :: ?int8}).
-record(cs_account_create,{
	roleName=[] :: ?string
	,sex=[] :: ?int8}).
-record(sc_account_create,{
	result=[] :: ?int8}).
-record(cs_account_enter_game,{
	}).
-record(sc_account_enter_game,{
	result=[] :: ?int8}).
-record(sc_account_kick,{
	reason=[] :: ?int8}).
-record(cs_account_heart,{
	}).
-record(sc_account_heart,{
	unixTime=[] :: ?int32}).
-record(cs_account_logout,{
	}).
-record(cs_account_check_rolename,{
	roleName=[] :: ?string}).
-record(sc_account_check_rolename,{
	result=[] :: ?int8}).
-record(cs_account_pay_arg,{
	accountID=[] :: ?int32
	,openID=[] :: ?string
	,payToken=[] :: ?string
	,openKey=[] :: ?string
	,pf=[] :: ?string
	,pfKey=[] :: ?string}).
-record(sc_account_pay_arg,{
	}).
-record(sc_account_update_pay_arg,{
	}).
