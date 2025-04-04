ALTER TABLE `user_online` ADD `srcType` smallint unsigned NOT NULL DEFAULT '0' COMMENT '渠道ID' AFTER `online`;
ALTER TABLE `user_online` ADD index `srcType` (`srcType`);
ALTER TABLE `gRole` ADD `timeVipDraw` int NOT NULL DEFAULT '0' COMMENT '领取每日vip礼包时间戳' AFTER `isFailed`;
ALTER TABLE `gRole` ADD `lastPayTime` int NOT NULL DEFAULT '0' COMMENT '每日首冲翻倍重置时间' AFTER `timeVipDraw`;
ALTER TABLE `gRole` ADD index `fightPower` (`fightPower`);
ALTER TABLE `gRole` ADD index `srcType` (`srcType`);
DROP TABLE IF EXISTS `gReward`;
CREATE TABLE `gReward` (
    `roleID` int(11) unsigned NOT NULL,
    `onlineSecs` int unsigned NOT NULL DEFAULT 0 COMMENT '累计在线时间',
    `days` int(11) unsigned NOT NULL DEFAULT 1 COMMENT '当前可领取天数',
    `lastDays` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '上次领取天数',
    `getList` varbinary(1000) NOT NULL COMMENT '已领取列表',
    PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;