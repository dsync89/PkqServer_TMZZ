alter table `gHron` add `maxDungeonNum` smallint(6) unsigned NOT NULL DEFAULT '0' COMMENT '最高第几关' after `curDungeonNum`;

DROP TABLE IF EXISTS `gMonthCard`;
CREATE TABLE `gMonthCard` (
  `roleID`      int(11) unsigned NOT NULL,
  `endTime`     int(11) unsigned NOT NULL,
  `drawTime`    int(11) unsigned NOT NULL,
  `dayPayGold`  int(11) unsigned NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gGuardInfo`;
CREATE TABLE `gGuardInfo`(
  `roleID` int(11) unsigned NOT NULL ,
  `count` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '累计次数',
  `guardInfo` blob NOT NULL COMMENT '玩家的守护武将信息',
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gAutoMail`;
CREATE TABLE `gAutoMail` (
  `configID` int(11) unsigned NOT NULL COMMENT '配置邮件ID',
  PRIMARY KEY (`configID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='系统自动发送邮件ID记录';

DROP TABLE IF EXISTS `gCoin`;
CREATE TABLE `gCoin` (
  `roleID`      int(11) unsigned NOT NULL,
  `date`        date    NOT NULL,
  `coolDown`    int(11) unsigned NOT NULL,
  `times`       smallint(6) unsigned NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

alter table `gRoleExtra` add `meleeSignTimes` smallint(5) unsigned NOT NULL COMMENT '大乱斗签到次数' after `dscvBuyTimes`;
alter table `gRoleExtra` add `lastMeleeSignDate` date NOT NULL COMMENT '上次大乱斗签到日期' after `meleeSignTimes`;

DROP TABLE IF EXISTS `gFriendFightTimes`;
CREATE TABLE `gFriendFightTimes` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `refreshDate`  date NOT NULL COMMENT '上次次数刷新日期',
  `fightTimes`  int(11) NOT NULL COMMENT '剩余挑战次数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gFriendFight`;
CREATE TABLE `gFriendFight` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
  KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

update `gRoleExtra` set `battleProgressHard` = 40001;