-define(CREATE_REPLAY_SQL,
        "CREATE TABLE IF NOT EXISTS  `gReplay~w` (
       `replayUID` bigint(20) unsigned NOT NULL,
       `replay` blob NOT NULL COMMENT '战斗录像具体内容。term_to_binary序列化。',
       `time` datetime NOT NULL COMMENT '战斗发生的时间，用来做定期的维护，过久的删除',
       PRIMARY KEY (`replayUID`),
       KEY `time` (`time`)
       ) ENGINE=InnoDB DEFAULT CHARSET=latin1;").

-define(CREATE_SQL_FORMAT_LIST,[

"CREATE TABLE IF NOT EXISTS `logBuyTimes_~w_~w` (
  `roleID` int(11) unsigned NOT NULL,
  `time` datetime NOT NULL,
  `vipLevel` smallint(6) unsigned NOT NULL COMMENT '购买时候的VIP等级',
  `todayBuyTimes` int(11) unsigned NOT NULL COMMENT '本日的第几次购买',
  `newValue` int(11) unsigned NOT NULL COMMENT '购买后的值',
  `add` int(11) unsigned NOT NULL COMMENT '本次增加值',
  `type` int(11) unsigned NOT NULL COMMENT '类型 1:体力次数 2:探索次数 3:pvp次数 4:秩序战场次数 5：兑换银两次数',
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='购买探索次数的LOG';",

"CREATE TABLE IF NOT EXISTS `logCreateRole_~w_~w` (
  `accid` bigint(11) unsigned NOT NULL COMMENT '用户ID',
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `devid` varchar(100) DEFAULT NULL COMMENT '设备id',
  `ip` varchar(20) NOT NULL COMMENT 'IP地址',
  `result` tinyint(1) unsigned NOT NULL COMMENT '创角结果：1=成功，2=重名，3=其他',
  `time` datetime NOT NULL,
  `sex` tinyint(1) unsigned NOT NULL COMMENT '性别',
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家创角日志';",

"CREATE TABLE IF NOT EXISTS `logSelectGer_~w_~w` (
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `time` datetime DEFAULT NULL,
  `gerID` smallint(6) unsigned NOT NULL COMMENT '武将的模版ID',
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='选择第一个武将的日志记录';",

"CREATE TABLE IF NOT EXISTS `t_coin_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` tinyint(4) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '获得的银两',
  `curCoin` int(11) NOT NULL COMMENT '获得前的银两数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='银两获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_coin_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` tinyint(4) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的银两',
  `curCoin` int(11) NOT NULL COMMENT '消费前的银两数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='银两消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_ger_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '新获得的武将UID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` tinyint(4) unsigned NOT NULL COMMENT '武将等级',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '武将品阶',
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '获得的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(20) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将获得记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` tinyint(4) unsigned NOT NULL COMMENT '武将等级',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '武将品阶',
  `time` datetime NOT NULL,
  `type` tinyint(4) unsigned NOT NULL COMMENT '武将消耗类型',
  `argID` int(11) unsigned NOT NULL COMMENT '附带参数',
  `desc` varchar(20) NOT NULL COMMENT '附带描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将消耗记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_uplevel_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` tinyint(4) unsigned NOT NULL COMMENT '升级前武将等级',
  `gerExp` bigint(20) unsigned NOT NULL COMMENT '升级前的武将经验',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '武将品阶',
  `newLevel` tinyint(4) unsigned NOT NULL COMMENT '新的武将等级',
  `newExp` bigint(20) unsigned NOT NULL COMMENT '新的武将等级',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将升级记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_uprank_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` tinyint(4) unsigned NOT NULL COMMENT '升品前武将等级',
  `gerExp` bigint(20) unsigned NOT NULL COMMENT '升品前的武将经验',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '升品前的武将品阶',
  `newLevel` tinyint(4) unsigned NOT NULL COMMENT '新的武将等级',
  `newExp` bigint(20) unsigned NOT NULL COMMENT '新的武将经验',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '新的武将品阶',
  `foodGerUID` bigint(20) unsigned NOT NULL COMMENT '作为升品材料的武将唯一ID',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将升品记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_downrank_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` tinyint(4) unsigned NOT NULL COMMENT '降品品前武将等级',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '降品品前的武将品阶',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '新的武将品阶',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将降品记录';",

"CREATE TABLE IF NOT EXISTS `t_gold_bonus_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` tinyint(4) unsigned NOT NULL COMMENT 'VIP等级',
  `goldBonus` int(11) unsigned NOT NULL COMMENT '获得的赠送元宝',
  `curGoldBonus` int(11) NOT NULL COMMENT '获得前的赠送元宝数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='赠送获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_gold_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` tinyint(4) unsigned NOT NULL COMMENT 'VIP等级',
  `gold` int(11) unsigned NOT NULL COMMENT '消耗的元宝',
  `goldBonus` int(11) unsigned NOT NULL COMMENT '消耗的赠送元宝',
  `curGold` int(11) unsigned NOT NULL COMMENT '消费前的元宝数量',
  `curGoldBonus` int(11) unsigned NOT NULL COMMENT '消费前的赠送元宝数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='元宝消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_item_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '新创建的道具UID、或者更新了数量的旧道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `addNum` smallint(6) unsigned NOT NULL COMMENT '获得的道具数量',
  `curItemNum` int(11) unsigned NOT NULL COMMENT '获得道具前的该道具数量',
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '获得的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(20) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='道具获得记录';",

"CREATE TABLE IF NOT EXISTS `t_item_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '消耗的道具UID、或者更新了数量的道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `decNum` smallint(6) unsigned NOT NULL COMMENT '消耗的道具数量',
  `curItemNum` int(11) unsigned NOT NULL COMMENT '消耗道具前的该道具数量',
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '消耗的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(20) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='道具消耗记录';",

"CREATE TABLE IF NOT EXISTS `t_item_uplevel_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '升级的道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `addLevel` tinyint(4) unsigned NOT NULL COMMENT '提升的装备等级',
  `newLevel` tinyint(4) unsigned NOT NULL COMMENT '升级后的装备等级',
  `addTimes` tinyint(4) unsigned NOT NULL COMMENT '连续升级的次数',
  `coin` int(11) DEFAULT NULL COMMENT '消耗的银两',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='装备升级记录';",

"CREATE TABLE IF NOT EXISTS `t_item_uprank_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '升品的道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `curLevel` tinyint(4) unsigned NOT NULL COMMENT '升品前的装备等级',
  `newLevel` tinyint(4) unsigned NOT NULL COMMENT '升品后的装备等级',
  `curRank` tinyint(4) unsigned NOT NULL COMMENT '升品前的装备品阶',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '升品后的装备品阶',
  `foodItemUID` bigint(20) unsigned NOT NULL COMMENT '消耗的装备UID',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='道具升品记录';",

"CREATE TABLE IF NOT EXISTS `t_repu_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` tinyint(4) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '获得的声望',
  `curCoin` int(11) NOT NULL COMMENT '获得前的声望数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='声望获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_repu_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` tinyint(4) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的声望',
  `curCoin` int(11) NOT NULL COMMENT '消费前的声望数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='声望消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_score_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` tinyint(4) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的积分',
  `curCoin` int(11) NOT NULL COMMENT '消费前的积分数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='积分消耗记录表';",

"CREATE TABLE IF NOT EXISTS `logLogin_~w_~w` (
  `accid` bigint(11) unsigned NOT NULL COMMENT '用户ID',
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `devid` varchar(100) DEFAULT NULL COMMENT '设备号',
  `ip` varchar(20) NOT NULL COMMENT 'IP地址',
  `datetime` datetime NOT NULL,
  `duration` int(8) unsigned NOT NULL COMMENT '在线时间',
  KEY `roleID` (`roleID`),
  KEY `datetime` (`datetime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家登录日志';",

"CREATE TABLE IF NOT EXISTS `logLogout_~w_~w` (
  `accid` int(11) unsigned NOT NULL COMMENT '用户ID',
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `devid` varchar(100) DEFAULT NULL COMMENT '设备号',
  `ip` varchar(20) NOT NULL COMMENT 'IP地址',
  `datetime` datetime NOT NULL,
  `duration` int(8) unsigned NOT NULL COMMENT '在线时间',
  `srctype` tinyint unsigned NOT NULL COMMENT '渠道ID',
  INDEX (`roleID`),
  INDEX (`accid`),
  INDEX (`datetime`),
  INDEX (`srctype`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家登出日志';",

"CREATE TABLE IF NOT EXISTS `logSuggest_~w_~w` (
  `accid` bigint(11) unsigned NOT NULL COMMENT '用户ID',
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `roleName` varchar(20) NOT NULL COMMENT '主公名字',
  `title` varchar(50) NOT NULL COMMENT '物理地址',
  `content` varchar(500) NOT NULL COMMENT 'IP地址',
  `datetime` datetime NOT NULL,
  KEY `accid` (`accid`),
  KEY `roleID` (`roleID`),
  KEY `datetime` (`datetime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家反馈日志';",

"CREATE TABLE IF NOT EXISTS `logHomestead_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='家园交配日志';",

"CREATE TABLE IF NOT EXISTS `logFriendEnargy_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
   `type` tinyint(1) unsigned NOT NULL COMMENT '1=领取，2=赠送',
  `DRoleID` int(11) unsigned NOT NULL COMMENT '主公ID',
 `value` int(11) unsigned NOT NULL COMMENT '体力值',
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='好友送和领取体力日志';",

"CREATE TABLE IF NOT EXISTS `logDungenFight_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
   `dungenID` int(11) unsigned NOT NULL COMMENT '关卡ID',
   `result` tinyint(1) unsigned NOT NULL COMMENT '结果：1=成功，0=失败',
   `type` tinyint(1) unsigned NOT NULL COMMENT '1=挑战，2=扫荡',
   `times` tinyint(4) unsigned NOT NULL COMMENT '扫荡的循环次数',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='关卡挑战日志';",

"CREATE TABLE IF NOT EXISTS `logWorldBoss_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
	`type` tinyint(1) unsigned NOT NULL COMMENT '1=南蛮，2=虎牢关',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='世界boss日志';",

"CREATE TABLE IF NOT EXISTS `logRaceSign_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='华丽大赛报名日志';",

"CREATE TABLE IF NOT EXISTS `logPvpFight_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
	`result` tinyint(1) unsigned NOT NULL COMMENT '结果：1=成功，0=失败',
	`rank` int(11) unsigned NOT NULL COMMENT '排名',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='好友送体力日志';",

"CREATE TABLE IF NOT EXISTS `t_tencent_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `time` datetime NOT NULL COMMENT '消费时间',
  `billno` varchar(20) NOT NULL COMMENT '流水号',
  `gold` int(11) unsigned NOT NULL COMMENT '消耗的元宝',
  `goldBonus` int(11) unsigned NOT NULL COMMENT '消耗的赠送元宝',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='腾讯游戏币消耗记录表';"
]).

