-- MySQL dump 10.13  Distrib 5.1.67, for redhat-linux-gnu (x86_64)
--
-- Host: localhost    Database: game
-- ------------------------------------------------------
-- Server version	5.1.67-log

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
-- Table structure for table `gActivity`
--

DROP TABLE IF EXISTS `gActivity`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gActivity` (
  `roleID` int(11) unsigned NOT NULL,
  `actID` int(5) unsigned NOT NULL,
  `value` varbinary(200) NOT NULL,
  `list` varbinary(1000) NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`actID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gAlien`;
CREATE TABLE `gAlien` (
  `roleID` int(11) unsigned NOT NULL,
  `alienTimes` int unsigned NOT NULL COMMENT '异星战场相关数据',
  `lastRecoverTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
  `resetTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `gBagItem`
--

DROP TABLE IF EXISTS `gBagItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gBagItem` (
  `itemUID` bigint(20) unsigned NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `itemTypeID` smallint(5) unsigned NOT NULL,
  `itemNum` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`itemUID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gBanAccount`
--

DROP TABLE IF EXISTS `gBanAccount`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gBanAccount` (
  `accountID` bigint(11) unsigned NOT NULL COMMENT '玩家帐号ID',
  PRIMARY KEY (`accountID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='封禁帐号表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gBestPassChapter`
--

DROP TABLE IF EXISTS `gBestPassChapter`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gBestPassChapter` (
  `roleID` int(11) unsigned NOT NULL,
  `chapterID` smallint(6) unsigned NOT NULL,
  UNIQUE KEY `roleID` (`roleID`,`chapterID`),
  KEY `roleID_2` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gCard`
--

DROP TABLE IF EXISTS `gCard`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gCard` (
  `roleID` int(11) unsigned NOT NULL,
  `openedCardList` varbinary(1000) NOT NULL COMMENT '已抽取的卡牌,term_to_binary存储',
  `cardList` varbinary(1000) NOT NULL COMMENT '等待抽取的列表,term_to_binary存储',
  `drawCount` int(11) NOT NULL COMMENT '当前的总点击次数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gChapter`
--

DROP TABLE IF EXISTS `gChapter`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gChapter` (
  `roleID` int(11) unsigned NOT NULL,
  `chapterID` int(11) unsigned NOT NULL COMMENT '章节ID',
  `bestRewarded` tinyint(1) NOT NULL DEFAULT '0' COMMENT '是否领取过完美通关奖励',
  `curDate` date NOT NULL,
  PRIMARY KEY (`roleID`,`chapterID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gDungeon`
--

DROP TABLE IF EXISTS `gDungeon`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gDungeon` (
  `roleID` int(11) unsigned NOT NULL,
  `chapterID` smallint(5) unsigned NOT NULL,
  `dungeonID` smallint(5) unsigned NOT NULL,
  `restTimes` smallint(6) unsigned NOT NULL,
  `bestScore` smallint(2) unsigned NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`dungeonID`),
  KEY `roleID` (`roleID`,`chapterID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gETC`
--

DROP TABLE IF EXISTS `gETC`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gETC` (
  `key` tinyint(4) unsigned NOT NULL,
  `value` longblob NOT NULL,
  PRIMARY KEY (`key`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gEquip`
--

DROP TABLE IF EXISTS `gEquip`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gEquip` (
  `itemUID` bigint(20) unsigned NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `itemTypeID` smallint(5) unsigned NOT NULL,
  `itemPos` tinyint(2) unsigned NOT NULL,
  `itemLevel` smallint(4) unsigned NOT NULL,
  `itemRank` tinyint(4) unsigned NOT NULL,
  `itemGerID` bigint(20) unsigned NOT NULL,
  `itemDecay` int(11) unsigned NOT NULL COMMENT '下次的道具品阶衰减时间',
  `itemExp` smallint(6) DEFAULT '0',
  PRIMARY KEY (`itemUID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gFamily`;
CREATE TABLE `gFamily` (
    `familyID`            int(11) unsigned NOT NULL       COMMENT '联盟ID',
    `familyName`          varchar(20) NOT NULL            COMMENT '联盟名称',
    `familyLevel`         smallint(6) unsigned NOT NULL   COMMENT '联盟等级',
    `createRoleID`        int(11) unsigned NOT NULL       COMMENT '创建者roleID',
    `createRoleName`      varchar(20) NOT NULL            COMMENT '创建者名称',
    `ownerRoleID`         int(11) unsigned NOT NULL       COMMENT '盟主roleID',
    `ownerRoleName`       varchar(20) NOT NULL            COMMENT '盟主名称',
    `curMembers`          smallint(4) unsigned NOT NULL   COMMENT '当前成员数量',
    `activePoints`        int(11) unsigned NOT NULL       COMMENT '联盟建设值',
    `notice`              varchar(200) NOT NULL           COMMENT '公告',
    `rank`                int(11) unsigned NOT NULL       COMMENT '联盟排名',
    `createTime`          int(11) unsigned NOT NULL       COMMENT '创建联盟的时间戳',
    `talkData`            blob    NOT NULL                COMMENT '联盟成员最近的聊天记录',
    PRIMARY KEY (`familyID`),
    UNIQUE KEY `familyName` (`familyName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
    
DROP TABLE IF EXISTS `gFamilyMember`;
CREATE TABLE `gFamilyMember` (
    `roleID`            int(11) unsigned NOT NULL           COMMENT '角色ID',
    `roleName`          varchar(20) NOT NULL                COMMENT '角色名称',
    `familyID`          int(11) unsigned NOT NULL           COMMENT '联盟ID',
    `familyCon`         int(11) unsigned NOT NULL           COMMENT '联盟贡献',
    `leftFamilyCon`     int(11) unsigned NOT NULL           COMMENT '剩余贡献',
    `useGoldTime`       int(11) unsigned NOT NULL          COMMENT '元宝贡献时间戳',
    `title`             tinyint(2) unsigned NOT NULL        COMMENT '玩家称号',
    `isMale`            tinyint(1) unsigned NOT NULL        COMMENT   '是否为男性',
    `roleLevel`         smallint(6) unsigned NOT NULL       COMMENT   '角色等级',
    `fightPower`        bigint(20) unsigned NOT NULL        COMMENT  '角色战斗力',
    `familyTitle`       tinyint(2) unsigned NOT NULL        COMMENT '联盟官职称号',
    `JoinTime`          int(11) unsigned NOT NULL          COMMENT '加入联盟时间',
    PRIMARY KEY (`roleID`),
    KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gFamilyRequest`;
CREATE TABLE `gFamilyRequest` (
    `roleID`        int(11) unsigned NOT NULL           COMMENT '角色ID',
    `roleName`      varchar(20) NOT NULL                COMMENT '角色名字',
    `roleLevel`     smallint(6) unsigned NOT NULL       COMMENT '角色等级',
    `fightPower`    bigint(20) unsigned NOT NULL        COMMENT '战斗力',
    `timestamp`     int(11) unsigned NOT NULL           COMMENT '申请时间戳',
    `familyID`      int(11) unsigned NOT NULL           COMMENT '申请联盟ID',
    UNIQUE KEY `roleID_2` (`roleID`,`familyID`),
    KEY (`roleID`),
    KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Table structure for table `gFighterList`
--

DROP TABLE IF EXISTS `gFighterList`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFighterList` (
  `roleID` int(11) unsigned NOT NULL,
  `fighterList` varbinary(1000) NOT NULL COMMENT '上阵武将列表',
  `lieuInfoList` varbinary(1000) NOT NULL COMMENT '副将数据信息,查看他人出战时使用',
  `lieuAtkAdd` smallint(6) unsigned NOT NULL COMMENT '副将的攻击加成',
  `lieuHpAdd` smallint(6) unsigned NOT NULL COMMENT '副将的血量加成',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gFriend`
--

DROP TABLE IF EXISTS `gFriend`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFriend` (
  `roleID` int(11) unsigned NOT NULL,
  `type` tinyint(1) unsigned NOT NULL,
  `friendID` int(11) unsigned NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`type`,`friendID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGather`
--

DROP TABLE IF EXISTS `gGather`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGather` (
  `roleID` int(11) unsigned NOT NULL,
  `type` tinyint(1) unsigned NOT NULL,
  `typeID` int(11) unsigned NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`type`,`typeID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGer`
--

DROP TABLE IF EXISTS `gGer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGer` (
  `gerID` bigint(20) unsigned NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `gerTypeID` smallint(5) unsigned NOT NULL,
  `gerLevel` int(3) unsigned NOT NULL,
  `gerExp` bigint(16) unsigned NOT NULL,
  `gerRank` tinyint(3) unsigned NOT NULL,
  `gerPos` tinyint(2) unsigned NOT NULL,
  PRIMARY KEY (`gerID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGift`
--

DROP TABLE IF EXISTS `gGift`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGift` (
  `roleID` int(11) unsigned NOT NULL,
  `type` char(2) NOT NULL COMMENT 'ç¤¼å“ç±»åž‹',
  PRIMARY KEY (`roleID`,`type`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGuide`
--

DROP TABLE IF EXISTS `gGuide`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGuide` (
  `roleID` int(11) unsigned NOT NULL,
  `guideState` smallint(6) unsigned NOT NULL COMMENT '新手引导的状态',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gHist`
--

DROP TABLE IF EXISTS `gHist`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gHist` (
  `histUID` bigint(20) unsigned NOT NULL,
  `histType` tinyint(4) unsigned NOT NULL COMMENT '?±???????4=pvp?????￥???5=?¤o????????￥',
  `name` varchar(15) NOT NULL COMMENT 'å¯¹æ‰‹åå­—',
  `enemyID` int(11) NOT NULL COMMENT 'å¯¹æ‰‹çš„çŽ©å®¶ID',
  `time` int(11) NOT NULL COMMENT 'Unixæ—¶é—´æˆ³',
  `arg` mediumint(9) unsigned NOT NULL COMMENT 'é™„å¸¦å‚æ•°',
  `isRead` tinyint(1) unsigned NOT NULL COMMENT 'æ˜¯å¦è¯»è¿‡è¿™å°æˆ˜æŠ¥',
  `fightInfo` varbinary(2000) NOT NULL COMMENT '????–—??????',
  `roleID` int(11) unsigned NOT NULL,
  `type` tinyint(1) unsigned NOT NULL COMMENT '战报大类型：4=争霸，5=秩序战场',
  `addRepu` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '得到声望',
  PRIMARY KEY (`histUID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='çŽ©å®¶æˆ˜æŠ¥';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gHron`
--

DROP TABLE IF EXISTS `gHron`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gHron` (
  `roleID` int(11) unsigned NOT NULL,
  `date` date NOT NULL COMMENT '数据日期，活动每天开一次，每天的数据对应一个日期,验证使用',
  `curDungeonNum` smallint(6) unsigned NOT NULL COMMENT '当前第几关，0=当前未开始挑战',
  `maxDungeonNum` smallint(6) unsigned NOT NULL DEFAULT '0' COMMENT '最高第几关',
  `attackAdd` smallint(6) unsigned NOT NULL COMMENT '当前攻击增加百分比',
  `hpAdd` smallint(6) unsigned NOT NULL COMMENT '当前血量增加百分比',
  `dungeonID` smallint(6) NOT NULL COMMENT '随机出来的关卡',
  `challengeTimes` tinyint(4) unsigned NOT NULL COMMENT '已挑战次数',
  `coinBuyTimes` tinyint(4) unsigned NOT NULL COMMENT '金币购买次数',
  `goldBuyTimes` tinyint(4) unsigned NOT NULL COMMENT '钻石购买次数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gInvite`
--

DROP TABLE IF EXISTS `gInvite`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gInvite` (
  `roleID` int(11) unsigned NOT NULL,
  `rewardNum` smallint(6) unsigned NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gInviteRoleList`
--

DROP TABLE IF EXISTS `gInviteRoleList`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gInviteRoleList` (
  `roleID` int(11) unsigned NOT NULL,
  `inviteRoleID` int(11) unsigned NOT NULL,
  UNIQUE KEY `inviteRoleID` (`inviteRoleID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gLimit`
--

DROP TABLE IF EXISTS `gLimit`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gLimit` (
  `roleID` int(11) unsigned NOT NULL,
  `encounterNum` tinyint(2) unsigned NOT NULL COMMENT '奇遇解锁数量',
  `isBindWeibo` tinyint(1) NOT NULL COMMENT '是否绑定了微博',
  `inviteRoleID` int(11) unsigned NOT NULL COMMENT '邀请你的玩家ID',
  `inviteRoleName` varchar(20) CHARACTER SET utf8 DEFAULT NULL COMMENT '要请你的角色名称',
  `lastShareLevel` smallint(4) unsigned NOT NULL COMMENT '上次微博分享的主公等级',
  `spiritGoldBoxCount` int(11) unsigned NOT NULL COMMENT '付费元宝召唤精灵单抽宝箱计数',
  `spiritGoldBonusBoxCount` int(11) unsigned NOT NULL COMMENT '赠送元宝召唤精灵单抽宝箱计数',
  `spiritItemBonusBoxCount` int(11) unsigned NOT NULL COMMENT '道具召唤精灵抽取宝箱计数',
  `equipGoldBoxCount` int(11) unsigned NOT NULL COMMENT '付费元宝祈祷装备单抽宝箱计数',
  `equipGoldBonusBoxCount` int(11) unsigned NOT NULL COMMENT '赠送元宝宝祈祷装备单抽宝箱计数',
  `equipItemBonusBoxCount` int(11) unsigned NOT NULL COMMENT '道具祈祷装备抽宝箱计数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gMail`
--

DROP TABLE IF EXISTS `gMail`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gMail` (
  `mailUID` bigint(20) unsigned NOT NULL COMMENT 'é‚®ä»¶å”¯ä¸€ID',
  `recvID` int(11) unsigned NOT NULL COMMENT 'é‚®ä»¶æŽ¥å—è€…çš„çŽ©å®¶ID',
  `mailType` tinyint(1) unsigned NOT NULL COMMENT 'é‚®ä»¶ç±»åž‹',
  `senderID` int(11) unsigned NOT NULL COMMENT 'é‚®ä»¶å‘é€è€…çš„UIDï¼Œ0=ç³»ç»Ÿå‘é€çš„',
  `senderName` varchar(20) NOT NULL COMMENT 'å‘é€è€…åå­—,å¦‚æžœæœ¬é‚®ä»¶æ˜¯ç³»ç»Ÿé‚®ä»¶ï¼Œåˆ™æ­¤å­—æ®µä¸ºç©º',
  `content` varchar(300) NOT NULL COMMENT 'å†…å®¹',
  `time` int(11) unsigned NOT NULL COMMENT 'å‘é€æ—¶é—´(Unixæ—¶é—´æˆ³)',
  `mailTemplateID` smallint(6) unsigned NOT NULL COMMENT 'é‚®ä»¶æ¨¡ç‰ˆID',
  `paramList` varbinary(300) NOT NULL COMMENT '??¨?€??±?????????°??—è?¨',
  `mailReward` varbinary(300) NOT NULL COMMENT '??ˉé￠???–?￥–??±',
  `isRead` tinyint(1) unsigned NOT NULL COMMENT 'æ˜¯å¦è¢«é˜…è¯»è¿‡',
  PRIMARY KEY (`mailUID`),
  KEY `recvID` (`recvID`),
  KEY `senderID` (`senderID`),
  KEY `mailType` (`mailType`),
  KEY `isRead` (`isRead`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='é‚®ä»¶ä¿¡æ¯';
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gAutoMail`;
CREATE TABLE `gAutoMail` (
  `configID` int(11) unsigned NOT NULL COMMENT '配置邮件ID',
  PRIMARY KEY (`configID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='系统自动发送邮件ID记录';

--
-- Table structure for table `gOfflineDeductGold`
--

DROP TABLE IF EXISTS `gOfflineDeductGold`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gOfflineDeductGold` (
  `roleID` int(11) unsigned NOT NULL,
  `deductGold` int(11) unsigned NOT NULL COMMENT '离线时已扣取的元宝',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gOfflinePayLog`
--

DROP TABLE IF EXISTS `gOfflinePayLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gOfflinePayLog` (
  `roleID` int(11) unsigned NOT NULL,
  `payItemID` int(11) unsigned NOT NULL COMMENT 'å……å€¼å•†å“ID',
  `receipt` varchar(3000) NOT NULL,
  `receiptMd5` varchar(40) NOT NULL,
  `SrcType` tinyint(4) NOT NULL,
  UNIQUE KEY `receiptMd5` (`receiptMd5`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gPay`
--

DROP TABLE IF EXISTS `gPay`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gPay` (
  `receiptMd5` varchar(40) NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `receipt` varchar(3000) NOT NULL,
  `srcType` tinyint(4) NOT NULL,
  `time` datetime NOT NULL,
  `payGold` int(11) unsigned NOT NULL,
  PRIMARY KEY (`receiptMd5`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gPush`
--

DROP TABLE IF EXISTS `gPush`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gPush` (
  `roleID` int(11) unsigned NOT NULL,
  `token` varchar(1000) NOT NULL,
  `isPVPPushOpen` tinyint(1) NOT NULL,
  `isPushNightMute` tinyint(1) NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;


--
-- Table structure for table `gRole`
--

DROP TABLE IF EXISTS `gRole`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gRole` (
  `roleID` int(11) unsigned NOT NULL COMMENT '玩家ID',
  `accid` bigint(11) unsigned NOT NULL COMMENT '帐号ID',
  `roleName` varchar(20) NOT NULL COMMENT '主公名字',
  `isMale` tinyint(1) NOT NULL COMMENT '性别',
  `level` smallint(6) unsigned NOT NULL DEFAULT '1' COMMENT '主公等级',
  `exp` bigint(16) unsigned NOT NULL DEFAULT '0' COMMENT '主公经验',
  `coin` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '银两',
  `reputation` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '声望',
  `gold` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '元宝',
  `goldBonus` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '官方赠送元宝',
  `goldUsed` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '花费元宝获得的积分',
  `vipLevel` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'VIP等级',
  `goldTotalPaid` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '总充值元宝',
  `title` tinyint(2) unsigned NOT NULL DEFAULT '0' COMMENT '官爵',
  `fightPower` int(14) unsigned NOT NULL DEFAULT '0' COMMENT '总战斗力',
  `lastLogoutTime` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '上次下线时间',
  `familyID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '玩家的联盟ID',
  `lastJoinFamily` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '最近一次加入联盟的时间',
  `head` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '选取的头像，0：默认头像',
  `payExtReward` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '充值额外奖励记录',
  `location` varchar(30) NOT NULL DEFAULT '' COMMENT '地理位置',
  `isFailed` tinyint(1) NOT NULL DEFAULT '0' COMMENT '是否关卡战败过',
  `timeVipDraw` int NOT NULL DEFAULT '0' COMMENT '领取每日vip礼包时间戳',
  `lastPayTime` int NOT NULL DEFAULT '0' COMMENT '每日首冲翻倍重置时间',
  `devid` varchar(100) NOT NULL COMMENT 'push token',
  `srcType` smallint unsigned NOT NULL DEFAULT '0' COMMENT '渠道ID',
  PRIMARY KEY (`roleID`),
  UNIQUE KEY `accid` (`accid`),
  UNIQUE KEY `roleName` (`roleName`),
  KEY `vipLevel` (`vipLevel`),
  KEY `level` (`level`),
  KEY `fightPower` (`fightPower`),
  KEY `srcType` (`srcType`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gReward`;
CREATE TABLE `gReward` (
    `roleID` int(11) unsigned NOT NULL,
    `onlineSecs` int unsigned NOT NULL DEFAULT 0 COMMENT '累计在线时间',
    `days` int(11) unsigned NOT NULL DEFAULT 1 COMMENT '当前可领取天数',
    `lastDays` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '上次领取天数',
    `getList` varbinary(1000) NOT NULL COMMENT '已领取列表',
    PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `gRoleExtra`
--

DROP TABLE IF EXISTS `gRoleExtra`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gRoleExtra` (
  `roleID` int(11) unsigned NOT NULL,
  `battleProgress` smallint(5) unsigned NOT NULL,
  `battleProgressHard` smallint(5) unsigned NOT NULL COMMENT '炼狱难度的战役推图',
  `battleProgressFastHard` smallint(5) unsigned NOT NULL COMMENT '最难难度的战役推图',
  `energy` smallint(6) unsigned NOT NULL,
  `energyBuyTimes` smallint(5) unsigned NOT NULL COMMENT '体力已购买次数',
  `challengeGodEnergy` smallint(6) unsigned NOT NULL COMMENT '神将录挑战次数',
  `challengeGodBuyTimes` smallint(5) unsigned NOT NULL COMMENT '购买神将录挑战的次数',
  `lastChallengeGodDate` date NOT NULL COMMENT '上次获得挑战神将录次数的日期',
  `refreshLieuTimes` smallint(5) unsigned NOT NULL COMMENT '副将免费刷新次数',
  `alreadyPayRefreshLieuTimes` mediumint(9) unsigned NOT NULL DEFAULT 0  COMMENT '元宝刷新副将次数累计',
  `dscvBuyTimes` smallint(5) unsigned NOT NULL COMMENT '探索已购买次数',
  `meleeSignTimes` smallint(5) unsigned NOT NULL COMMENT '大乱斗签到次数',
  `lastMeleeSignDate` date NOT NULL COMMENT '上次大乱斗签到日期',
  `pvpBuyTimes` smallint(5) unsigned NOT NULL COMMENT '争霸已购买次数',
  `ruleBuyTimes` smallint(5) unsigned NOT NULL COMMENT '秩序战场已购买次数',
  `coinBuyTimes` smallint(6) unsigned NOT NULL COMMENT '银两购买次数',
  `coinBossHP` bigint unsigned NOT NULL DEFAULT 0 COMMENT '招财boss血量',
  `fireTimes` INT UNSIGNED NOT NULL COMMENT '放鞭炮次数',
  `lastBuyTimesRefreshDate` date NOT NULL,
  `lastEnergyTime` int(11) unsigned NOT NULL,
  `discoveryTimes` smallint(5) unsigned NOT NULL,
  `lastDscvTime` int(11) unsigned NOT NULL,
  `dscvCount` int(11) unsigned NOT NULL,
  `pvpTimes` smallint(5) unsigned NOT NULL,
  `lastPvpTime` int(11) unsigned NOT NULL,
  `ruleTimes` smallint(5) unsigned NOT NULL COMMENT '秩序战场次数',
  `lastRuleTime` int(11) unsigned NOT NULL COMMENT '秩序战场上次恢复时间',
  `weiboCount` smallint(5) unsigned NOT NULL,
  `nextWeiboCountRefreshSec` int(11) unsigned NOT NULL,
  `lastWeiXinShareSec` int(11) unsigned not null COMMENT '上次微信分享的时间',
  `encounterList` varbinary(3000) NOT NULL COMMENT '遭遇战列表,term_to_binary存储',
  `lastTitleRewardDate` date NOT NULL COMMENT '上次领取官爵奖励的时间',
  `lastDrawTitle` tinyint(4) unsigned NOT NULL COMMENT '上次领取官爵奖励时的官爵',
  `lastLoggedLoginDate` date NOT NULL COMMENT '上次记录下来的登录日期',
  `lastDrawLoginRewardDays` int(11) unsigned NOT NULL COMMENT '上次领取连续登录奖励的连续登录日数',
  `loginDays` int(11) unsigned NOT NULL COMMENT '当前的连续登录日数',
  `lastDrawLevelUpLevel` smallint(6) unsigned NOT NULL COMMENT '上次领取的升级礼包的等级',
  `randomShopList` varbinary(1000) NOT NULL COMMENT '奇遇商店列表',
  `dailyDrawList` varbinary(100) NOT NULL COMMENT '本月签到列表',  
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gItemUse`;
CREATE TABLE `gItemUse` (
    `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
    `itemTypeID`  smallint(5) unsigned NOT NULL  COMMENT '道具模版ID',
    `useDate`     date NOT NULL COMMENT '最近一次使用日期',
    `useTimes`    tinyint unsigned NOT NULL  COMMENT '当日累积使用次数',
    UNIQUE KEY `roleID_2` (`roleID`,`itemTypeID`),
    KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `gShopNum`
--

DROP TABLE IF EXISTS `gShopNum`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gShopNum` (
  `roleID` int(11) unsigned NOT NULL,
  `shopID` smallint(6) unsigned NOT NULL,
  `sellID` smallint(6) unsigned NOT NULL,
  `buyNum` smallint(6) unsigned NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`shopID`,`sellID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gShopTreasure`;
CREATE TABLE `gShopTreasure` (
  `roleID` int(11) unsigned NOT NULL,
  `nextRefreshTime` int unsigned NOT NULL,
  `itemList` varbinary(1000)  NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gTeamPk`;
CREATE TABLE `gTeamPk` (
  `roleID` int(11) unsigned NOT NULL,
  `teamPkData` varbinary(20000) NOT NULL COMMENT '3v3相关数据',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `user_online`
--

DROP TABLE IF EXISTS `user_online`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_online` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `time` char(10) CHARACTER SET latin1 NOT NULL,
  `num` mediumint(9) NOT NULL,
  `online` date NOT NULL,
  `srcType` smallint unsigned NOT NULL DEFAULT '0' COMMENT '渠道ID',
  PRIMARY KEY (`id`),
  KEY `srcType` (`srcType`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 CHECKSUM=1 DELAY_KEY_WRITE=1;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;


DROP TABLE IF EXISTS `gEncounter`;
CREATE TABLE `gEncounter` (
	`roleID` int(11) unsigned NOT NULL,
	`monsterRank` tinyint(8) unsigned NOT NULL,
	`chapterInfoList` longblob NOT NULL,
	PRIMARY KEY(`roleID`),
	KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


DROP TABLE IF EXISTS `gLieuInfo`;
CREATE TABLE `gLieuInfo`(
  `roleID` int(11) unsigned NOT NULL ,
  `lieuInfo` blob NOT NULL COMMENT '玩家的参军信息',
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gGuardInfo`;
CREATE TABLE `gGuardInfo`(
  `roleID` int(11) unsigned NOT NULL ,
  `count` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '累计次数',
  `guardInfo` blob NOT NULL COMMENT '玩家的守护武将信息',
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gTalk`;
CREATE TABLE `gTalk`(
	`roleID` int(11) unsigned NOT NULL,
	`gag_list` blob NOT NULL COMMENT '玩家屏蔽的世界聊天发言人',
	PRIMARY KEY(`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gOtherRecord`;
CREATE TABLE `gOtherRecord`(
	`roleID` int(11) unsigned NOT NULL,
	`lastSignDate` date NOT NULL COMMENT '上次签到的日期',
	`signedDays` tinyint(4) unsigned NOT NULL COMMENT '连续签到的天数',
	`isEmperor` tinyint(4) unsigned NOT NULL COMMENT '是否是帝王',
	`isGetBox` tinyint(4) unsigned NOT NULL COMMENT '是否已经领取连续签到的宝箱',
	PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gFailedPayLog`;
CREATE TABLE `gFailedPayLog` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `receipt` varchar(3000) NOT NULL,
  `srcType` tinyint(4) NOT NULL,
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='充值失败单号记录表';


DROP TABLE IF EXISTS `gTreasureHouse`;
CREATE TABLE `gTreasureHouse`(
	`roleID` int(11) unsigned NOT NULL COMMENT '玩家的角色ID',
	`value_info` tinyint(4) unsigned NOT NULL COMMENT '玩家当前的倍率值',
	`card_list` varbinary(3000)  NOT NULL COMMENT '玩家获当前的显示道具列表',
	`free_count` int(9) unsigned NOT NULL COMMENT '玩家免费探索的次数',
	`buy_count` int(9) unsigned NOT NULL COMMENT '玩家付费探索的次数',
	`free_times` tinyint(5) unsigned NOT NULL COMMENT '玩家当日免费探索次数',
	`mark` int(11) unsigned NOT NULL COMMENT '玩家探索得到的分数',
	`baseBoxGetProcess` varbinary(1000) NOT NULL COMMENT '玩家获取的保底宝箱列表',
	`isGetRankReward` tinyint(4) unsigned NOT NULL COMMENT '玩家是否领取了排行榜奖励',
	`lastExploreDate` date COMMENT '玩家上次探索的日期',
	`activityID` mediumint(8) unsigned	NOT NULL COMMENT '活动ID',
	PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gTask`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gTask` (
  `roleID` int(11) unsigned NOT NULL  COMMENT '角色ID',
  `taskID` int(11) unsigned NOT NULL COMMENT '任务ID',
  `status`  int(3) unsigned NOT NULL COMMENT '任务状态',
  `triggerNum`  int(11) unsigned NOT NULL COMMENT '触发进度',
  `triggerNotes`  blob NOT NULL COMMENT '任务触发条件记录',
  UNIQUE KEY `roleID_2` (`roleID`,`taskID`),
  KEY `roleID` (`roleID`),
  KEY `taskID` (`taskID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gHomestead`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gHomestead` (
  `roleID` 		int(11) unsigned NOT NULL  COMMENT '角色ID',
  `roleName`   varchar(20) NOT NULL    COMMENT '角色名称',
  `addEnergyTimes`  int(3) unsigned NOT NULL COMMENT '充能剩余次数',
  `matingTimes`  int(3) unsigned NOT NULL COMMENT '交配剩余次数',
  `matingCoolSecond`  int(11) unsigned NOT NULL COMMENT '交配冷却时间',
  `add4mating`  int(3) unsigned NOT NULL COMMENT '交配加成',
  `gerID`  bigint(20) unsigned NOT NULL COMMENT '守护宠物UID',
  `gerTypeID` smallint(5) unsigned NOT NULL COMMENT '守护宠物类型',
  `quality` tinyint(3) unsigned NOT NULL COMMENT '守护宠物品阶',
  `level` tinyint(3) unsigned NOT NULL COMMENT '守护宠物等级',
  `refreshMatingSecond` int(11) unsigned NOT NULL COMMENT '上一次刷新交配次数时间',
  `machineList`  blob NOT NULL COMMENT '机器列表',
  `logList`  blob NOT NULL COMMENT '日志列表',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gFriendEnargy`;
CREATE TABLE `gFriendEnargy` (
  `roleID` 		int(11) unsigned NOT NULL  COMMENT '角色ID',
  `refreshDate`  date NOT NULL COMMENT '上次次数刷新日期',
  `giveTimes`  int(11) NOT NULL COMMENT '剩余领取体力次数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

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

DROP TABLE IF EXISTS `gToFriend`;
CREATE TABLE `gToFriend` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  `isGive`      tinyint(1) unsigned NOT NULL  COMMENT '是否赠送',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
  KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gToMe`;
CREATE TABLE `gToMe` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
  KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gAddFriend`;
CREATE TABLE `gAddFriend` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
  KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gBeAddFriend`;
CREATE TABLE `gBeAddFriend` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
  KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `t_gold_pay_add`;
CREATE TABLE IF NOT EXISTS `t_gold_pay_add` (
    `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
    `roleID` int(11) unsigned NOT NULL,
    `vipLevel` tinyint(4) unsigned  NOT NULL,
    `gold` int(11) unsigned NOT NULL,
    `curGold` int(11) unsigned NOT NULL,
    `time` datetime NOT NULL,
    `appItemID` int(8) unsigned NOT NULL,
    `desc` varchar(20) CHARACTER SET latin1 NOT NULL,
    `receiptMd5` varchar(40) NOT NULL,
     `accid` bigint(11) unsigned NOT NULL COMMENT '帐号ID',
     `devid` varchar(100) NOT NULL COMMENT '设备号',
     `srcType` smallint NOT NULL COMMENT '渠道id',
    PRIMARY KEY(`id`),
    KEY `roleID` (`roleID`),
    KEY `time` (`time`),
    KEY `appItemID` (`appItemID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `logGuide`;
CREATE TABLE `logGuide` (
  `roleID` int(11) unsigned NOT NULL,
  `guideState` smallint(6) unsigned NOT NULL COMMENT '新手引导的状态',
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`roleID`),
  KEY `datetime` (`datetime`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gOfflinePayAmountLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gOfflinePayAmountLog` (
  `roleID` int(11) unsigned NOT NULL,
  `amount` int(11) unsigned NOT NULL COMMENT '充值额度',
  `receipt` varchar(3000) NOT NULL,
  `receiptMd5` varchar(40) NOT NULL,
  `SrcType` tinyint(4) NOT NULL,
  UNIQUE KEY `receiptMd5` (`receiptMd5`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

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

DROP TABLE IF EXISTS `gCoin`;
CREATE TABLE `gCoin` (
  `roleID`      int(11) unsigned NOT NULL,
  `date`        date    NOT NULL,
  `coolDown`    int(11) unsigned NOT NULL,
  `times`       smallint(6) unsigned NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gMonthCard`;
CREATE TABLE `gMonthCard` (
  `roleID`      int(11) unsigned NOT NULL,
  `endTime`     int(11) unsigned NOT NULL,
  `drawTime`    int(11) unsigned NOT NULL,
  `dayPayGold`  int(11) unsigned NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;