

-define(melee_fight_tar_won, melee_fight_tar_won).
-define(melee_fight_src_won, melee_fight_src_won).
 
-record(melee_fighter,{
                       roleID = 0
                       ,roleName = <<"">>
                       ,groundID = 0         %% 战场ID
                       ,score = 0            %% 积分
                       ,cur_win_times = 0    %% 当前连胜场次  
                       ,longest_win = 0      %% 连胜场次
                       ,camp                 %% 阵营      1:喵喵阵营； 2:皮卡丘阵营
                      }).


-record(role_melee, {
                     roleID=0
                    ,fightPower=0
                    , isMale=false
                    , title=0
                    , head=0,
                    level = 0
                    , roleName = <<"">>
                    , rank = 0
                    , score = 0
                    , serverID = 0
                    , win_streak=0,
                    fightTimes=0, tarRoleID=0,
                    winConTimes=0, winConMaxTimes=0,
                    lastIsWin=false
                     }).