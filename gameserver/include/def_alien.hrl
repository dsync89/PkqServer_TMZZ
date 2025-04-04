
-define(STATUS_CLOSE,          0).
-define(STATUS_SIGN,           1).
-define(STATUS_FIGHT,          2).

-record(role_alien, {roleID=0,is_sign=false, group_id=0, fighterList=[], itemList=[], fightPower=0, isMale=false, title=0, head=0,
                     level = 0, roleName = <<"">>, rank = 0, serverID = 0, hpPercent=100, killNum=0,
                     killContinuousNum=0, maxKillContinuousNum=0, isInContinuous=false,
                     guessCoin=0, guessType=false, timestamp=0,canBeAtkTime=0}).

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

