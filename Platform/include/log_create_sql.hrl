-define(CREATE_SQL_FORMAT_LIST,[
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
"CREATE TABLE IF NOT EXISTS `logLogin_~w_~w` (
  `accid` bigint(11) unsigned NOT NULL COMMENT '用户ID',
  `datetime` datetime NOT NULL COMMENT '登录的时间',
  `srctype` tinyint unsigned NOT NULL COMMENT '渠道ID',
  INDEX ( `accid` ),
  INDEX ( `datetime` ),
  INDEX ( `srctype` )
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家登录日志';"							
]).