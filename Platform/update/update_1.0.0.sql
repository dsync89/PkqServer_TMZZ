ALTER TABLE `account` ADD `ip` varchar(20) NOT NULL COMMENT 'IP地址' AFTER `devid`;
DROP TABLE IF EXISTS `reward`;
CREATE TABLE `reward` (
  `accountID` int(11) unsigned NOT NULL,
  `typeVal` int unsigned NOT NULL,
  `isDraw` tinyint(1) NOT NULL DEFAULT '0',
  UNIQUE KEY `accountTypeVal` (`accountID`,`typeVal`),
  KEY `accountID` (`accountID`),
  KEY `typeVal` (`typeVal`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;