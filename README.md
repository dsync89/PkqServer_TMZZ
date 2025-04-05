# PkqServer_TMZZ

This is a complete working Erlang server for Destiny Child mobile game, for self learning purpose mostly about Erlang.

Check my website https://dsync89.com for a complete guide on setting this up.

## Build Environment

You need

- CentOS 7
- mySQL (running as docker for simplicity)
- ...


## Run the server

Go into each folder then run the following.

gameserver
```
cd gameserver
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name pm1@192.168.1.37 -setcookie moongame -pa ebin deps/ebin config config/app setting -s tk start
```

Platform
```
cd Platform
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name pm_platform@192.168.1.37 -setcookie moongame -pa ebin deps/ebin config config/app setting -s inets start -s user_default s
```
PlatformPay
```
cd PlatformPay
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name pm_pay@192.168.1.37 -setcookie moongame -pa ebin deps/ebin config config/app setting -s inets start -s user_default s
```
 
Logs if running successfully

gameserver
```
...
** deps/ebin/emysql_tracer.beam hides ebin/emysql_tracer.beam
** deps/ebin/emysql_tcp.beam hides ebin/emysql_tcp.beam
** deps/ebin/emysql_sup.beam hides ebin/emysql_sup.beam
** deps/ebin/emysql_statements.beam hides ebin/emysql_statements.beam 
** deps/ebin/emysql_conn_mgr.beam hides ebin/emysql_conn_mgr.beam
** deps/ebin/emysql_conn.beam hides ebin/emysql_conn.beam
** deps/ebin/emysql_auth.beam hides ebin/emysql_auth.beam
** deps/ebin/emysql.beam hides ebin/emysql.beam
** Found 55 name clashes in code paths 
starting TCP listeners            ...done
starting Init forbid list         ...done
```

Platform
```
...
                       {mfargs,{ssl_manager,start_link,[[]]}},
                       {restart_type,permanent},
                       {shutdown,4000},
                       {child_type,worker}]

=PROGRESS REPORT==== 5-Apr-2025::04:43:40 ===
          supervisor: {local,ssl_sup}
             started: [{pid,<0.131.0>},
                       {name,ssl_connection},
                       {mfargs,{ssl_connection_sup,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,4000},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 5-Apr-2025::04:43:40 ===
         application: ssl
          started_at: 'pm_platform@192.168.1.37'
```

PlatformPay
```
...
                       {mfargs,{ssl_manager,start_link,[[]]}},
                       {restart_type,permanent},
                       {shutdown,4000},
                       {child_type,worker}]

=PROGRESS REPORT==== 5-Apr-2025::04:43:20 === 
          supervisor: {local,ssl_sup}
             started: [{pid,<0.151.0>},
                       {name,ssl_connection}, 
                       {mfargs,{ssl_connection_sup,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,4000},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 5-Apr-2025::04:43:20 === 
         application: ssl
          started_at: 'pm_pay@192.168.1.37'
```

## Original README in Chinese
```
============================================================

后端流程：
	环境配置：
		1、操作系统：CentOs7
		2、系统软件：erlangR16B，mysql5.7（docker或者源码安装均可）
		3、docker安装mysql
			docker pull mysql:5.7.27 //拉取redis镜像
			docker run --name mysql -e MYSQL_ROOT_PASSWORD=数据库密码 -v /data/mysqldb_dir(映射目录):/var/lib/mysql -p 内网ip/外网ip:3306:3306 -d mysql:5.7.27
		4、编译安装erlang(确保shell脚本和安装包在同一目录下)
			#!/bin/bash
			yum -y install gcc gcc-c++ autoconf libjpeg libjpeg-devel libpng libpng-devel freetype freetype-devel libxml2 libxml2-devel glibc glibc-devel bzip2 bzip2-devel ncurses ncurses-devel curl curl-devel e2fsprogs e2fsprogs-devel  krb5-devel libidn libidn-devel openssl openssl-devel openldap openldap-devel  openldap-clients openldap-servers perl-CPAN mysql-devel unixODBC unixODBC-devel lrzsz ntp screen libtool-ltdl-devel cmake unzip epel-release wxWidgets-devel

			tar zxf openssl-1.0.1j.tar.gz
			cd openssl-1.0.1j
			./config --prefix=/usr  --openssldir=/etc/ssl --libdir=lib shared zlib-dynamic
			make && make install
			make clean
			./config --prefix=/usr/local/ssl
			sed -i 's/DGHASH_ASM/DGHASH_ASM -fPIC/' Makefile
			make && make install
			cd ../
			tar zxf otp_src_R16B.tar.gz
			cd otp_src_R16B

			CHOST="x86_64-pc-linux-gnu" CFLAGS="-march=nocona -O2 -pipe" CXXFLAGS="-march=nocona -O2 -pipe"
			./configure --enable-kernel-poll --enable-threads --enable-smp-support --enable-hipe --with-ssl=/usr/local/ssl/
			make && make install
	配置文件修改(进修改config文件不需要重新编译)
		1、gameserver
			1、config/setting/setting.config
				{cross_master_ip, '111.67.196.4'}.
				{cross_slave_server_list, [{4, '111.67.196.4'}, {22, '111.67.196.4'}, {28, '111.67.196.4'}]}.
				{account_check_addr,"111.67.196.4"}.
				{account_server_addr,"111.67.196.4"}.
				{push_url,"http://111.67.196.4:28082/recvpush"}.
				{database, {"localhost",3306,"laochen","laochenlao","cm_game3",10}}
				//将上述IP地址和数据库相关配置改成自己的
		2、Platform
			1、config/common.config
				{database, {"localhost",3306,"root","123456","cm_login",10}}.
				//修改数据库配置信息
			2、config/server_list.config
				{server, 1,  1, "测试1",  "111.67.196.4",10001, 9001, "12345678901234567890123456789012", 1}.
				将区对应的服务器ip修改掉
		3、PlatformPay
			1、config/common.config
				{database, {"localhost",3306,"root","123456","cm_login",10}}.
				{pay_port, {12831, "111.67.196.4"}}.
				//修改服务器ip地址和数据库配置信息
			2、config/server_list.config
				{server, 1,  1, "测试1",  "111.67.196.4",10001, 9001, "12345678901234567890123456789012", 1}.
				将区对应的服务器ip修改掉
	编译/运行
		1、修改程序后需要重新编译，进入各自目录，执行以下命令
			sh build.sh
		2、运行(进入上面各自根目录,执行以下命令)
			screen
			./start.sh > /dev/null &
		3、退出screen
			ctrl+a
			d	
前端流程(windows)：
	环境配置
		1、环境软件：jdk1.8，android-sdk(API LEVEL18-26),ndk(r10d)
		2、设置环境变量，JAVA_HOME,CLASS_PATH,ANDROD_SDK_ROOT,NDK_ROOT
		3、设置完成后开启dos窗口 ，分别执行以下命令确定环境配置正确
			java -version
			echo %JAVA_HOME%
			echo %CLASS_PATH%
			echo %ANDROID_SDK_ROOT%
			echo %NDK_ROOT%
	内容修改及编译
		1、项目采用cocos2d-x引擎，使用c++编写，所以每次改动c++代码都需要重新编译，编译步骤如下
		2、下载cgwin
		3、安装cgwin选择核心包，devel下的binutils，gcc-core,gcc-g++，gdb，mingwin64-i686-gcc-core，mingwin64-i686-gcc-g++,make（可参考该链接：https://www.cnblogs.com/xiaobai-cs/p/16830043.html）
		4、进入目录E:\TMZZ\ProjectTK(选择自己实际存放目录) ，classes下是游戏的C++代码，proj.android是android工程目录
		5、进入classes文件夹，修改SDS_NetWorkDefine.h和MB_UpdateSystem.cpp对应的服务器ip
		6、编辑E:\TMZZ\ProjectTK\Classes\Common\SDS_NetWorkDefine.h
			#define LOGIN_SERVER_URL "http://43.153.70.97:12380/"             //app
			#define NOTICE_SERVER_URL "http://43.153.70.97:11010/notice?type=1"
			将上述的ip换成自己的ip
		7、编辑E:\TMZZ\ProjectTK\Classes\Game\MB_UpdateSystem.cpp
			第32行 httpRequest->setUrl("http://43.153.70.97:8088/xjl_hotfix/serverVersion.json");
			第87行 pDetail->setUpdateUrl(CCString::createWithFormat("http://43.153.70.97:8088/xjl_hotfix/res%s.zip", _versionStr.c_str())->getCString());
			将此两处的ip换成自己ip
		8、打开cgwin，进入proj.android目录
		9、打开build_natvie.sh
			将第40行修改为 COCOS2DX_ROOT="$DIR/../libs"
		10、执行sh build_native.sh命令进行前端编译
	打包环境及流程
		1、上述修改及编译完成后，可以使用eclipse打开
		2、eclipse版本
			Version: 2019-09 R (4.13.0)
			Build id: 20190917-1200
		3、安装ADT23（必须要安装此插件才能设置Android SDK和NDK）
		4、设置jdk
			Windows->Preferences->Java->Installed JREs
			选择已经安装的jdk1.8
		5、设置Android SDK
			Windows->Preferences->Android->SDK Location
			选择已经安装的sdk
		6、设置NDK
			Windows->Preferences->Android->NDK->NDK Location
			选择已经安装的NDK
		7、打包
			File->Export->Android->Export Android Application->选择Project（默认会出来TMZZ_quick）->选择已有key或者新建key（都可在Eclipse中完成) ->选择apk导出目录	
			点击finish（完成）即可完成apk的打包工作
============================================================
```