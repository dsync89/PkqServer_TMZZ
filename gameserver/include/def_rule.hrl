

-record(role_rule, {roleID=0,fightPower=0, isMale=false, title=0, head=0,
                    level = 0, roleName = <<"">>, rank = 0, score = 0, serverID = 0, winTimes=0,
                    fightTimes=0, tarRoleID=0,
                    winConTimes=0, winConMaxTimes=0,
                    lastIsWin=false}).

-record(alien_fighter2,{
                        roleID=0
                        ,fightPower=0
                        ,isMale=false
                        ,title=0
                        ,head=0
                        ,level=0
                        ,roleName= <<"">>
                        ,rank=0
                        ,serverID=0
                        ,killNum=0
                        ,timestamp=0}).

-record(alien_fighter3,{
                        roleID=0
                        ,fightPower=0
                        ,isMale=false
                        ,title=0
                        ,head=0
                        ,level=0
                        ,roleName= <<"">>
                        ,rank=0
                        ,serverID=0
                        ,killContinuousNum=0
                        ,isInContinuous=false
                        ,timestamp=0}).

