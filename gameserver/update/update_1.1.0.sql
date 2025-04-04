alter table `gRoleExtra` change `plunderBuyTimes` `ruleBuyTimes` smallint(5) unsigned NOT NULL COMMENT '秩序战场已购买次数';
alter table `gRoleExtra` change `plunderTimes` `ruleTimes` smallint(5) unsigned NOT NULL COMMENT '秩序战场次数';
alter table `gRoleExtra` change `lastplunderTime` `lastRuleTime` int(11) unsigned NOT NULL COMMENT '秩序战场上次恢复时间';

delete from `gETC` where `key` = 3;

DROP TABLE IF EXISTS `gRoad`;
CREATE TABLE `gRoad` (
  `roleID`      int(11) unsigned NOT NULL,
  `timestamp`   int(11) unsigned NOT NULL,
  `nowID`       tinyint(4) unsigned NOT NULL,
  `extID`       tinyint(4) unsigned NOT NULL,
  `status`      tinyint(4) unsigned NOT NULL,
  `resetTimes`  tinyint(4) unsigned NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

alter table `logGuide` add `datetime` datetime NOT NULL after `guideState`;
ALTER TABLE `logGuide` ADD index `datetime` (`datetime`);