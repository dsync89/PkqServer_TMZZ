%% 服务器可识别的奖励格式
% [{?REWARD_GOLD, 100},
%  {?REWARD_COIN,100},
%  {?REWARD_REPU, 100},
%  {?REWARD_ITEM, 10010, Num},
%  {?REWARD_GER, 11010, Num},
%  #new_item{},
%  #new_ger{},
%  [#new_item{}],
%  [#new_ger{}]]
%
%
%
%

-define(REWARD_COIN, 1).% 银两
-define(REWARD_GOLD, 2).% 元宝
-define(REWARD_REPU, 3).% 声望
-define(REWARD_ROLE_EXP,4).% 主公经验
-define(REWARD_GER_EXP,5).% 武将经验
-define(REWARD_ITEM, 6).% 道具
-define(REWARD_GER, 7).% 武将