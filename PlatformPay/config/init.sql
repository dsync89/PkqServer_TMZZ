CREATE TABLE IF NOT EXISTS `account` (
  `accountID` int(11) NOT NULL auto_increment,
  `type` int(4),
  `accountName` varchar(40),
  `password` varchar(35),
  `phoneNumber` varchar(20),
  KEY `accountID` (`accountID`),
  KEY `accountName` (`accountName`),
  UNIQUE (`type`,`accountName`)
) ENGINE=InnoDB  AUTO_INCREMENT=1000000 DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `login_history` (
  `accountID` int(11) NOT NULL,
  `serverId` int(5) unsigned,
  `time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  KEY `accountID` (`accountID`),
  UNIQUE (`accountID`,`serverId`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `pay` (
  `orderSerial` varchar(40) NOT NULL,
  `serverID` int(5) NOT NULL,
  `roleID` int(11) NOT NULL,
  `deviceID` varchar(40) NOT NULL,
  `macAddr` varchar(20) NOT NULL,
  `ip` varchar(15) NOT NULL,
  `goodsID` int(11)	NOT NULL,
  `goodsCount` int(4) NOT NULL,
  `uin` int(11) NOT NULL,
  `createTime` timestamp,
  PRIMARY KEY `orderSerial` (`orderSerial`),
  KEY `serverID` (`serverID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;