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


CREATE TABLE `all_pay_log`(
  `id` bigint(11) unsigned NOT NULL AUTO_INCREMENT,
  `is_succ` tinyint(4) unsigned NOT NULL COMMENT '是否成功,1=>成功,2=>数据验证失败,3=>游戏服返回失败,4=>游戏服无响应,5=>其他失败',
  `roleID` int(11) unsigned NOT NULL COMMENT '玩家的角色ID',
  `accid` int(11) unsigned NOT NULL COMMENT '玩家的帐号ID',
  `amount` int(11)  unsigned NOT NULL COMMENT '玩家充值金额',
  `pay_time` datetime COMMENT '玩家充值时间',
  `pay_status` tinyint(4) unsigned NOT NULL COMMENT '充值状态',
  `srcType` tinyint(4) unsigned NOT NULL COMMENT '充值渠道',
  `receiptMd5` varchar(40) NOT NULL COMMENT '数据md5',
  `receipt` varchar(6000) NOT NULL COMMENT '订单原始数据,未必可读',
  PRIMARY KEY (`id`),
  KEY `is_succ` (`is_succ`),
  KEY `roleID` (`roleID`),
  KEY `accid` (`accID`),
  KEY `amount` (`amount`),
  KEY `srcType` (`srcType`),
  KEY `pay_status` (`pay_status`),
  KEY `pay_time` (`pay_time`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT '玩家充值记录';

