DROP TABLE IF EXISTS `chatGM`;
CREATE TABLE `chatGM` (
  `srcAccountID` int(11) NOT NULL,
  `srcRoleID` int(11) NOT NULL,
  `srcRoleName` varchar(20) DEFAULT NULL,
  `tarAccountID` int(11) NOT NULL,
  `tarRoleID` int(11) NOT NULL,
  `tarRoleName` varchar(20) DEFAULT NULL,
  `dateTime` datetime NOT NULL COMMENT '举报时间',
  INDEX (`dateTime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;