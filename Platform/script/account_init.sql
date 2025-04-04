-- MySQL dump 10.13  Distrib 5.1.69, for redhat-linux-gnu (x86_64)
--
-- Host: localhost    Database: Platform
-- ------------------------------------------------------
-- Server version	5.1.69

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `account`
--

DROP TABLE IF EXISTS `account`;
CREATE TABLE `account` (
  `accountID` int(11) NOT NULL AUTO_INCREMENT,
  `type` int(4) DEFAULT NULL,
  `accountName` varchar(40) DEFAULT NULL,
  `password` varchar(35) DEFAULT NULL, 
  `phoneNumber` varchar(20) DEFAULT NULL,
  `signTime` datetime NOT NULL COMMENT '帐号创建时间',
  `devid` varchar(64) DEFAULT NULL COMMENT '设备唯一标识ID',
  `ip` varchar(20) NOT NULL COMMENT 'IP地址',
  UNIQUE KEY `type` (`type`,`accountName`),
  KEY `accountID` (`accountID`),
  KEY `accountName` (`accountName`),
  KEY `signTime` (`signTime`)
) ENGINE=InnoDB AUTO_INCREMENT=1000000 DEFAULT CHARSET=utf8;

--
-- Table structure for table `giftcode`
--

DROP TABLE IF EXISTS `giftcode`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `giftcode` (
  `type` char(2) NOT NULL COMMENT '礼品码header',
  `code` char(8) NOT NULL COMMENT '礼品码body',
  `accountID` bigint(11) unsigned NOT NULL COMMENT '账号ID',
  `genTime` datetime NOT NULL COMMENT '生成时间',
  `getTime` datetime NOT NULL COMMENT '领取时间',
  PRIMARY KEY (`type`,`code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `login_history`
--

DROP TABLE IF EXISTS `login_history`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `login_history` (
  `accountID` int(11) NOT NULL,
  `serverID1` int(5) unsigned NOT NULL DEFAULT '0',
  `serverID2` int(5) unsigned NOT NULL DEFAULT '0',
  `serverID3` int(5) unsigned NOT NULL DEFAULT '0',
  `serverID4` int(5) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY `accountID` (`accountID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `create_process_log`;
CREATE TABLE `create_process_log`(
    `accid`  int(11) unsigned NOT NULL COMMENT '玩家的帐号ID',
    `roleID` int(11) unsigned NOT NULL COMMENT '玩家的角色ID',
    `createTime` datetime COMMENT '创建角色时间',
    `selectTime` datetime COMMENT '选择武将时间',
    `enterTime` datetime COMMENT '进入游戏时间',
    KEY `accid` (`accid`),
    KEY `roleID` (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT '角色创建进度';


DROP TABLE IF EXISTS `reward`;
CREATE TABLE `reward` (
  `accountID` int(11) unsigned NOT NULL,
  `typeVal` int unsigned NOT NULL,
  `isDraw` tinyint(1) NOT NULL DEFAULT '0',
  UNIQUE KEY `accountTypeVal` (`accountID`,`typeVal`),
  KEY `accountID` (`accountID`),
  KEY `typeVal` (`typeVal`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

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
