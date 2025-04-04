%% @author admin
%% @doc 玩家网关进程
%% 负责：登录验证、创建角色
%% Created 2013-2-20

%% ~~~~~！！！！
%% 请保证此进程的进程字典只有一个socket值，因为role_lib:send_client的特殊实现。

-module(gw).
-compile(export_all).

-export([start_client/1, start_link/0, init/0]).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-define(TCP_TIMEOUT, -1). % 解析协议超时时间
-define(HEART_TIMEOUT, 300000). % 心跳包超时时间
-define(HEART_TIMEOUT_SEC, 300000). % 心跳包超时描述
-define(HEADER_LENGTH, 2). % 消息头长度
-define(TICK_INTERVAL, 5*1000).%TICK间隔
%% ======================================
%% dictionary key in this process
-define(roleInfo, roleInfo).
-define(socket, socket).

%% ======================================
-define(test_attacker_list, (ger_attr:test_ger_list())).
-define(test_all_ger, (ger_attr:test_all_ger())).
%% #ger{gerID=1,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=1,gerTypeID=12101,gerKingdom=1,gerSkill1=1,gerSkill2=2,gerPos=1,gerAntiWei=100,gerAntiQun=100},
%% 	  gerAttr=#gerAttr{gerID=1,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=2,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=2,gerTypeID=12102,gerKingdom=1,gerSkill1=1,gerSkill2=3,gerPos=2,gerAntiWei=100,gerAntiQun=100},
%% 	  gerAttr=#gerAttr{gerID=2,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=3,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=3,gerTypeID=12103,gerKingdom=1,gerSkill1=1,gerSkill2=4,gerPos=3,gerAntiWei=100,gerAntiQun=100},
%% 	  gerAttr=#gerAttr{gerID=3,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=4,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=4,gerTypeID=12104,gerKingdom=1,gerSkill1=1,gerSkill2=5,gerPos=4,gerAntiWei=100,gerAntiQun=100},
%% 	  gerAttr=#gerAttr{gerID=4,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=5,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=5,gerTypeID=12105,gerKingdom=1,gerSkill1=1,gerSkill2=6,gerPos=5,gerAntiWei=100,gerAntiQun=100},
%% 	  gerAttr=#gerAttr{gerID=5,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=6,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=6,gerTypeID=12106,gerKingdom=1,gerSkill1=1,gerSkill2=7,gerPos=6,gerAntiWei=100,gerAntiQun=100},
%% 	  gerAttr=#gerAttr{gerID=6,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40}]).


%%记录客户端进程
-record(client, {
				 socket = undefined,
				 roleServer= none,
				 roleID=0,
				 login  = 0,
				 accid  = 0,
				 accname = none,
				 timeout = 0, % 上次心跳时间
				macAddr = 0,
				 ip=0,
				 deviceID="",
                 srcType=0
				}).

%% 开启客户端服务
start_client(Sock) ->
	{ok, Child} = supervisor:start_child(gateway_tcp_client_sup, []),
	ok = gen_tcp:controlling_process(Sock, Child),
	Child ! {go, Sock}.

start_link() ->
	{ok, spawn_link(?MODULE, init, [])}.

%%gen_server init
%%Host:主机IP
%%Port:端口
init() ->
	process_flag(trap_exit, true),
	receive
		{go, Socket} ->
	Client = #client{
					 roleServer = none,
					 login  = 0,
					 accid  = 0,
					 accname = none,
					 timeout = util:now()
					},
			put(?socket, Socket),
			tick(),
			login_parse_packet(Socket, Client)
	end.

%%接收来自客户端的数据 - 先处理登陆
%%Socket：socket id
%%Client: client记录
login_parse_packet(Socket, Client) ->
    async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    login_parse_packet1(Socket, Client).

login_parse_packet1(Socket, Client) ->
    receive
        %%登陆处理
        {inet_async, Socket, _Ref, {ok, <<BodyLen:16>>}} ->
            case BodyLen > 0 of
                true ->
                    _Ref1 = async_recv(Socket, BodyLen-?HEADER_LENGTH, ?TCP_TIMEOUT),
                    login_parse_packet2(Socket, Client);
                false ->
                    login_lost(Socket, Client, 7, "WPE encounted！！")
            end;
        tick ->
            do_login_tick(Socket, Client);
        %%用户断开连接或出错
        Other ->
            login_lost(Socket, Client, 8, Other)
    end.

login_parse_packet2(Socket, Client) ->
    GuiCreateRole = data_common:get(guiCreateRole),
    receive
        {inet_async, _, _, {error, timeout}} ->
            login_parse_packet(Socket, Client);
        {inet_async, Socket, _, {ok, Binary}} ->
            ?DEBUG("receive packet=~w",[Binary]),
            case proto:decode(Binary) of
                %% 验证版本号
                #cs_version{version=Version}->
                    check_client_version(Version,Socket,Client);
                %%先验证登陆
                #cs_account_login{
                                  userID=UserID,
                                  accountName=Accname,
                                  macAddr=MacAddr,
                                  serverID=ServerID,
                                  deviceID=DeviceID,
                                  srcType=SrcType
                                 }=Data ->
                    Accid = (ServerID + 1) * ?AccidBase + UserID,
                    IsServerIDBad = not lists:member(ServerID, [data_setting:get(server_id) | data_setting:get(merge_server_id_list)]),
                    OnlineNum = ets:info(?ETS_ROLE_ONLINE,size),
                    MaxOnlineNum = data_setting:get(max_online_num),
                    if OnlineNum > MaxOnlineNum ->
                           Reply = #sc_account_login{result=5,isCreated=false,isGerSelected=false,guiCreateRole=GuiCreateRole},
                           tk_misc:send_sock(Socket, Reply),
                           login_lost(Socket, Client, 1, "login full");
                       IsServerIDBad ->
                           Reply = #sc_account_login{result=6,isCreated=false,isGerSelected=false},
                           tk_misc:send_sock(Socket, Reply),
                           login_lost(Socket, Client, 2, "login send error server id");
                       true ->
                           case check_auth(Data) of
                               true ->                                            
                                   Client1 = Client#client{
                                                           login = 1,
                                                           accid = Accid,
                                                           accname = Accname,
                                                           socket=Socket,
                                                           macAddr=MacAddr
                                                          },
                                   %% 判断该帐号是否已经创建了主公
                                   case check_created(Accid) of
                                       {true, RoleID, IsSelectedGer} ->
                                           Reply = #sc_account_login{result=1,isCreated=true,isGerSelected=IsSelectedGer,guiCreateRole=GuiCreateRole},
                                           tk_misc:send_sock(Socket, Reply),
                                           do_enter_game(Accid, RoleID, Client1#client{roleID=RoleID,deviceID=DeviceID,srcType=SrcType}, Socket);
                                       false ->
                                           Reply = #sc_account_login{result=1,isCreated=false,isGerSelected=false,guiCreateRole=GuiCreateRole},
                                           tk_misc:send_sock(Socket, Reply),
                                           login_parse_packet(Socket, Client1#client{deviceID=DeviceID,srcType=SrcType})
                                   end;
                               {false,Reason} ->
                                   Reply = #sc_account_login{result=Reason,isCreated=false,isGerSelected=false,guiCreateRole=GuiCreateRole},
                                   tk_misc:send_sock(Socket, Reply),
                                   login_lost(Socket, Client, 3, "login fail")
                           end
                    end;
                
                %%创建角色
                #cs_account_create{}=Data ->
                    case Client#client.login == 1 of
                        true ->
                            case do_create_role(Client, Data) of
                                {true, RoleID}->
                                    post_create_process_to_platform(Client#client.accid, RoleID, 0),
                                    mail_server:send_notemp_mail(RoleID, 1),
                                    do_enter_game(0, RoleID, Client#client{roleID=RoleID}, Socket);
                                false ->
                                    login_parse_packet(Socket, Client)
                            
                            end;
                        false ->
                            login_lost(Socket, Client, 4, "request cs_account_create before request cs_account_login")
                    end;
                #cs_account_check_rolename{roleName=RoleName}->
                    case check_roleName(RoleName) of
                        ok->
                            Reply = #sc_account_check_rolename{result=1},
                            tk_misc:send_sock(Socket, Reply),
                            login_parse_packet(Socket, Client);
                        {false,Reason} ->
                            Reply = #sc_account_check_rolename{result=Reason},
                            tk_misc:send_sock(Socket, Reply),
                            login_parse_packet(Socket, Client)
                    end;
                #cs_account_heart{} ->
                    NowSec = util:now(),
                    tk_misc:send_sock(Socket, #sc_account_heart{unixTime=NowSec}),
                    login_parse_packet(Socket, Client#client{timeout=NowSec});
                #cs_account_pay_arg{accountID=AccountID, openID=OpenID, payToken=PayToken, openKey=OpenKey, pf=PF, pfKey=PFKey} ->
                    Accid = (data_setting:get(server_id) + 1) * ?AccidBase + AccountID,
                    ets:insert(?ROLE_PAY_ARG, {Accid, OpenID, PayToken, OpenKey, PF, PFKey}),
                    tk_misc:send_sock(Socket, #sc_account_pay_arg{}),
                    login_parse_packet(Socket, Client);
                #cs_role_demo_fight{} ->
                    case data_common:get(is_open_fight_demo) of
                        true ->
                            FightInfo = [data_common:get(fight_demo)];
                        _ ->
                            FightInfo = []
                    end,
					?ERR("tk_misc:send_soc=================FightInfo====================~w",[FightInfo]),
                    tk_misc:send_sock(Socket, #sc_role_demo_fight{type=0,fightInfo=FightInfo}),
                    login_parse_packet(Socket, Client);
                Other ->
                    login_lost(Socket, Client, 5, Other)
            end;
        tick ->
            do_login_tick2(Socket, Client);
        Other ->
            login_lost(Socket, Client, 6, Other)
    end.

check_client_version(Version,Socket,Client)->
	{M,S,L} = data_setting:get(client_version),
	[CMT,CST,CLT|_] = re:split(Version, "[.]",[{return,list}]),
	CM = erlang:list_to_integer(CMT),
	CS = erlang:list_to_integer(CST),
	CL = erlang:list_to_integer(CLT),
	%Rst = compare_two_digit(CM,M) andalso compare_two_digit(CS,S) andalso compare_two_digit(CL,L),
	Rst = if CM < M ->
				 false;
			 CM =:= M ->
				 if CS < S ->
						false;
					CS =:= S ->
						if CL < L ->
							   false;
						   true ->
							   true
						end;
					true ->
						true
				 end;
			 true ->
				 true
		  end,

	case Rst of
		true->
			Reply = #sc_version{result=1},
			tk_misc:send_sock(Socket, Reply),
			login_parse_packet(Socket, Client);
		false ->
			Reply = #sc_version{result=2},
			tk_misc:send_sock(Socket, Reply),
			login_lost(Socket, Client, 9, "version error")
	end.

do_login_tick(Socket, Client) ->
	case check_timeout(Client) of
		true ->
			login_lost(Socket, Client, 10, timeout);
		false->
			tick(),
			login_parse_packet1(Socket, Client)
	end.	

do_login_tick2(Socket, Client) ->
    case check_timeout(Client) of
        true ->
            login_lost(Socket, Client, 10, timeout);
        false->
            tick(),
            login_parse_packet2(Socket, Client)
    end.

check_timeout(#client{timeout=LastSec}) ->
	NowSec = util:now(),
	% ?DEBUG("check_timeout = ~p:~p",[NowSec - LastSec, ?HEART_TIMEOUT_SEC]),
	NowSec - LastSec > ?HEART_TIMEOUT_SEC.
	


%%接收来自客户端的数据 - 登陆后进入游戏逻辑
%%Socket：socket id
%%Client: client记录
do_parse_packet(Socket, Client) ->
    async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    do_parse_packet1(Socket, Client).

do_parse_packet1(Socket, Client) ->
	receive
		{inet_async, Socket, _Ref, {ok, <<BodyLen:16>>}} ->
			case BodyLen > 0 of
				true ->
					async_recv(Socket, BodyLen-?HEADER_LENGTH, ?TCP_TIMEOUT),
					do_parse_packet2(Socket, Client);
				false ->
					do_lost(Socket, Client,  "WPE encounted！！")
			end;
		{send_client, Msg} ->
			tk_misc:send_sock(Socket, Msg),
			do_parse_packet1(Socket, Client);
		{emu, Msg} ->
			route(Client, Msg),
			?DEBUG("emu route=~100p",[Msg]),
			do_parse_packet1(Socket, Client);
		
		%% 帐号在别的地方登录
		login_again ->
			tk_misc:send_sock(Socket, #sc_account_kick{reason=1}),
			do_lost(Socket, Client, login_again);
		%% 延迟收到的进程退出错误
		{'DOWN', _, process, _, _} ->
			do_parse_packet1(Socket, Client);
		tick ->
			case check_timeout(Client) of
				true ->
					do_lost(Socket, Client, timeout);
				false ->
					tick(),
					do_parse_packet1(Socket, Client)
			end;
					
		%%用户断开连接或出错
		Other ->
			do_lost(Socket, Client,  Other)
	end.

do_parse_packet2(Socket, Client) ->
    receive
        {inet_async, Socket, _Ref1, {ok, Binary}} ->
            ?DEBUG("receive packet=~w",[Binary]),
            case proto:decode(Binary) of
                %%这里是处理游戏逻辑
                #cs_account_heart{} ->
                    NowSec = util:now(),
                    tk_misc:send_sock(Socket, #sc_account_heart{unixTime=NowSec}),
                    do_parse_packet(Socket, Client#client{timeout=NowSec});
                #cs_account_logout{}    ->
                    do_lost(Socket,Client, {'EXIT',client_exit,normal});
                #cs_account_pay_arg{openID=OpenID, payToken=PayToken, openKey=OpenKey, pf=PF, pfKey=PFKey} ->
                    Accid = Client#client.accid,
                    ets:insert(?ROLE_PAY_ARG, {Accid, OpenID, PayToken, OpenKey, PF, PFKey}),
                    tk_misc:send_sock(Socket, #sc_account_pay_arg{}),
                    do_parse_packet(Socket, Client);
                Data ->
                    %% 路由消息
                    ?DEBUG("recv msg=~100000p",[Data]),
                    case erlang:is_record(Data, cs_version) orelse erlang:is_record(Data, cs_account_login) of
                        true ->
                            next;
                        false ->
                            route(Client, Data)
                    end,
                    do_parse_packet(Socket, Client)
            end;
        {send_client, Msg} ->
            tk_misc:send_sock(Socket, Msg),
            do_parse_packet2(Socket, Client);
        {emu, Msg} ->
            route(Client, Msg),
            ?DEBUG("emu route=~100p",[Msg]),
            do_parse_packet2(Socket, Client);
        
        %% 帐号在别的地方登录
        login_again ->
            tk_misc:send_sock(Socket, #sc_account_kick{reason=1}),
            do_lost(Socket, Client, login_again);
        %% 延迟收到的进程退出错误
        {'DOWN', _, process, _, _} ->
            do_parse_packet2(Socket, Client);
        tick ->
            case check_timeout(Client) of
                true ->
                    do_lost(Socket, Client, timeout);
                false ->
                    tick(),
                    do_parse_packet2(Socket, Client)
            end;
        
        %%用户断开连接或出错
        Other ->
            do_lost(Socket, Client,  Other)
    end.

tick() ->
	erlang:send_after(?TICK_INTERVAL, self(), tick).
	
%%断开连接
login_lost(Socket, Client, Cmd, Reason) ->
	case Reason of
		{inet_async,_,_,{error,closed}} ->
			?DEBUG("socket closed...during login",[]);
		timeout ->
			?DEBUG("socket closed...during login ..timeout",[]);
		_ ->
			?ERR("login lost..reason=~100000p,Socket:~w, Client:~w, Cmd:~w",[Reason,Socket, Client, Cmd])
	end,
	%% 等写socket
	timer:sleep(200),
	erlang:exit({unexpected_message, Reason}).

%%退出游戏
do_lost(_Socket, #client{roleID=RoleID}, login_again) ->
    %% 等写socket,重新登录时,不关闭逻辑进程
    erlang:unlink(role_lib:pid(RoleID)),
    erlang:unregister(role_lib:gatewayRegName(RoleID)),
    timer:sleep(200),
    erlang:exit(login_again);
do_lost(_Socket, Client,  Reason) ->
	case Reason of
		{_,_,_,{error,closed}} ->
			?DEBUG("socket closed...",[]);
		{'EXIT',_,normal} ->
			?DEBUG("socket closed...role_server shutdown",[]);
		timeout ->
			?DEBUG("socket closed...timeout",[]);
		_ ->
			?ERR("do lost...~100000p,roleServer=~w",[Reason,Client#client.roleServer])
	end,
	role_server:stop(Client#client.roleServer),
	%% 等写socket
	timer:sleep(200).

%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
	prim_inet:async_recv(Sock, Length, Timeout).
%% 		{error, Reason} -> throw({Reason});
%% 		{ok, Res}       -> Res;
%% 		Res             -> Res
%% 	end.

%% 检查密钥是否正确
%% Accid=1, UnixTime=1361000000, accountName="crimoon", ticket="1e5eaff16f066b8b97cb0296cf46aeee"
%% tk_misc:send_sock(S, #cs_account_login{userID=1,unixTime=1361000000,accountName="crimoon",ticket="1e5eaff16f066b8b97cb0296cf46aeee"}).
check_auth(#cs_account_login{userID=Accid,
							 unixTime=UnixTime,
							 accountName=_Accname,
							 ticket=Ticket,
                             serverID=ServerID
							}) ->
    case data_setting:get(is_release) of
        true ->
            NowSec = util:now(),
            %% UnixTime 是帐号服务器发给客户端的过期时间，此处判断ticket是否过期
            if NowSec > UnixTime ->
                   {false,3};
               true ->
                   Md5 = integer_to_list(Accid) ++ integer_to_list(UnixTime) ++ ?TICKET,
                   Hex = util:md5(Md5),		   
                   if Hex =:= Ticket ->
                          case gateway_ban:check_ban((ServerID + 1) * ?AccidBase + Accid) of
                              true ->
                                  {false, 4};
                              false ->
                                  true
                          end;
                      true ->
                          {false, 2}
                   end
            end;
        false ->
            true
    end.

%% 判断是否已经注册了主公信息
check_created(Accid) ->
	case db_sql:check_roleCreated(Accid) of
		{true, RoleID} ->
			case db_sql:check_ger_select(RoleID) of
				false ->
					{true,RoleID, false};
				true ->			
					{true, RoleID, true}
			end;
		false ->
			false 
	end.


%% 判断名字是否合法
check_roleName(RoleName) ->
	DecodeList = gen_utf8_decode_list(erlang:list_to_binary(RoleName), []),
	case check_blankName(DecodeList) of
		true ->
			Length = calc_name_length(DecodeList),
			if Length > 24 orelse Length < 1 ->
				   ?DEBUG("RoleName length:~w,name:~w,DecodeList:~w",[Length,RoleName,DecodeList]),
				   {false,5};
			   true ->
				   case db_sql:search_roleName(util:latin1(RoleName)) of
					   RoleID when is_integer(RoleID) ->
						   {false, 3};
					   _ ->
%% 						   ?ERR("RoleName:~s, DecodeList:~w, Length:~w", [RoleName, DecodeList, Length]),
%% 						   {false, 3}
						   %% 玩家不存在
						   ok
				   end
			end;
		_ ->
			{false, 4}
	end.

calc_name_length(DecodeList) ->
	lists:foldr(fun(Val, Acc) ->
						case Val >= 16#4e00 andalso Val =< 16#9fa5 of
							true ->
								Acc + 2;
							false ->
								Acc + 1
						end
				end, 0, DecodeList).

gen_utf8_decode_list(<<>>, AccDecodeList) ->
	lists:reverse(AccDecodeList);
gen_utf8_decode_list(<<0:1,X1:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,X5:1,
					   1:1,0:1,X6:1,X7:1,X8:1,X9:1,X10:1,X11:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,
					   1:1,0:1,X5:1,X6:1,X7:1,X8:1,X9:1,X10:1,
					   1:1,0:1,X11:1,X12:1,X13:1,X14:1,X15:1,X16:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1, 
					   1:1,0:1,X4:1,X5:1,X6:1,X7:1,X8:1,X9:1, 
					   1:1,0:1,X10:1,X11:1,X12:1,X13:1,X14:1,X15:1, 
					   1:1,0:1,X16:1,X17:1,X18:1,X19:1,X20:1,X21:1, 
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,0:1,X1:1,X2:1, 
					   1:1,0:1,X3:1,X4:1,X5:1,X6:1,X7:1,X8:1, 
					   1:1,0:1,X9:1,X10:1,X11:1,X12:1,X13:1,X14:1, 
					   1:1,0:1,X15:1,X16:1,X17:1,X18:1,X19:1,X20:1, 
					   1:1,0:1,X21:1,X22:1,X23:1,X24:1,X25:1,X26:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,1:1,0:1,X1:1, 
					   1:1,0:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1, 
					   1:1,0:1,X8:1,X9:1,X10:1,X11:1,X12:1,X13:1, 
					   1:1,0:1,X14:1,X15:1,X16:1,X17:1,X18:1,X19:1, 
					   1:1,0:1,X20:1,X21:1,X22:1,X23:1,X24:1,X25:1, 
					   1:1,0:1,X26:1,X27:1,X28:1,X29:1,X30:1,X31:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(Binary, AccDecodeList) ->
	?ERR("非法的utf8编码，Binary：~w, AccDecodeList：~w", [Binary, AccDecodeList]),
	[].

get_val(List) ->
	{_, Val} = 
	lists:foldr(fun(Elem, {Count, AccVal}) ->
						{Count + 1, AccVal + math:pow(2, Count) * Elem}
	            end, {0, 0}, List),
	erlang:trunc(Val).
	
check_blankName([])->
	true;
check_blankName([H|T])->
	case (H >=48 andalso H =< 57) orelse (H >=65 andalso H =< 90) orelse (H >=97 andalso H =< 122) orelse (H >= 16#4e00 andalso H =< 16#9fa5) of
		false ->
			false;
		true ->
			check_blankName(T)
	end.

%% 创建角色流程
do_create_role(Client, Data) ->
	#client{accid=Accid, socket=Socket, deviceID=DeviceID, srcType=SrcType} = Client,
	#cs_account_create{roleName=RoleName, sex=Sex} = Data,

	case check_roleName(RoleName) of
		ok ->
			do_create_role2(RoleName,Sex,Socket,Accid,DeviceID,SrcType);
		{false, Type} ->
			?ERR("Invalid RoleName, Can't cteate,roleName=~1000p,type=~w, accid=~w",[RoleName, Type,Accid]),
			Reply = #sc_account_create{result=Type},
			tk_misc:send_sock(Socket, Reply),
			false
	end.

sex_to_bool(1) ->
	true;
sex_to_bool(2) ->
	false.

gen_roleID() ->
	%% 存下来是为了减小 角色名重复等允许的创角错误带来的roleID不连续情况
	case get(genRoleID) of
		RoleID when is_integer(RoleID) ->
			RoleID;
		_ ->
			RoleID = tk_id:gen_roleID(),
			put(genRoleID, RoleID),
			RoleID
	end.

do_create_role2(RoleName,Sex,Socket,Accid,DeviceID,SrcType) ->			
	%GerList = ?test_attacker_list,
	%OtherGer = ?test_all_ger,
	
	RoleID = gen_roleID(),
	tk_id:gen_roleID(),
	RoleInfo = #role{roleID= RoleID,
					 accid=Accid,
					 roleName=RoleName,
					 isMale=sex_to_bool(Sex),
					 gold=0,
					 coin=data_common:get(account_init_coin),
					 fightPower=0,
					 title=0,
					 familyID=0,
                     lastJoinFamily=0,
					 description="",
					 exp=0,
					 goldBonus=data_common:get(account_init_gold),
					 goldTotalPaid=0,
					 goldUsed=0,
					 lastLogoutTime=0,
					 level=1,
					 reputation=0,
					 vipLevel=data_common:get(account_init_vipLevel),
                     payExtReward=0,
					 deviceID=DeviceID,
                     srcType=SrcType
					},
	case db_sql:create_roleInfo(RoleInfo) of
		{ok, _} ->
			%?ERR("atomic"),
			Reply = #sc_account_create{result=1},
			tk_misc:send_sock(Socket, Reply),
			erase(genRoleID),
			behavior_create_role:log(Accid, RoleID, DeviceID, get_ip(Socket), Sex, 1),
			{true, RoleID};
		{error, Error} ->
			?ERR("create role error=~1000p\n with roleInfo=~1000p",[Error, RoleInfo]),
			Reply = #sc_account_create{result=3},
			tk_misc:send_sock(Socket, Reply),
			behavior_create_role:log(Accid, RoleID, DeviceID, get_ip(Socket), Sex, 3),
			false
	end.

update_login_history(Accid) ->
    catch global:send(util:get_platform_server(), {update_login_history, Accid rem ?AccidBase, data_setting:get(server_id)}).

%% 进入游戏
do_enter_game(Accid, RoleID, Client, Socket) ->
	#client{macAddr=MacAddr,deviceID=DeviceID,srcType=SrcType} = Client,
	behavior_login_log:log(Accid, RoleID, DeviceID, get_ip(Socket)),
	{ok, RoleServerPid} = do_enter_game2(RoleID, Socket, MacAddr, Accid, DeviceID, SrcType),
    update_login_history(Accid),
	do_parse_packet(Socket, Client#client{roleServer = RoleServerPid}).

do_enter_game2(RoleID, Socket, MacAddr, Accid, DeviceID, SrcType) ->
    Ip = get_ip(Socket),
    Bool = check_login_again(RoleID),
    case ets:lookup(?REG_JUDG_TABLE, RoleID) of
        [] ->
            case role_server:start([RoleID, self(), Socket, MacAddr, Ip, DeviceID, SrcType]) of
                {ok, Pid} ->
                    next;
                {error,not_exit} ->
                    ?ERR("RoleID:~w not_exit", [RoleID]),
                    Pid = ?undefined,
                    erlang:exit(not_exit)
            end;
        _ ->
            case role_lib:pid(RoleID) of
                ?undefined ->
                    timer:sleep(1000),
                    case role_server:start([RoleID, self(), Socket, MacAddr, Ip, DeviceID, SrcType]) of
                        {ok, Pid} ->
                            next;
                        {error, not_exit} ->
                            ?ERR("RoleID:~w not_exit", [RoleID]),
                            timer:sleep(5000),
                            case role_server:start([RoleID, self(), Socket, MacAddr, Ip, DeviceID, SrcType]) of
                                {ok, Pid} ->
                                    next;
                                {error, not_exit} ->
                                    Pid = ?undefined,
                                    ?ERR("sleep 5000ms start role_server return not_exit"),
                                    erlang:exit(not_exit)
                            end
                    end;
                OldPid ->
                    case Bool of
                        true ->
                            Pid = OldPid,
                            post_login_info_to_platform(Accid, SrcType),
                            case catch gen_server:call(Pid, {login_again, self(), Socket, MacAddr, Ip, DeviceID}, 10000) of
                                ok ->
                                    next;
                                Err ->
                                    ?ERR("Err:~w", [Err]),
                                    erlang:exit(Err)
                            end;
                        false ->
                            Ref = monitor_pid(OldPid),
                            role_server:stop(RoleID),
                            receive
                                {'DOWN', Ref, process, OldPid, _} ->
                                    case role_server:start([RoleID, self(), Socket, MacAddr, Ip ,DeviceID, SrcType]) of
                                        {ok, Pid} ->
                                            next;
                                        {error,not_exit} ->
                                            ?ERR("RoleID:~w not_exit", [RoleID]),
                                            Pid = ?undefined,
                                            erlang:exit(not_exit)
                                    end
                            after 5000 ->
                                Pid = ?undefined,
                                ?ERR("stop_role_server_timeout:RoleID:~w,Pid:~w", [RoleID,OldPid]),
                                erlang:exit(stop_role_server_timeout)
                            end
                    end
            end
    end,
    {ok, Pid}.

post_login_info_to_platform(Accid, SrcType) ->
	catch global:send(util:get_platform_server(), {login_again, Accid rem ?AccidBase, SrcType}).

post_create_process_to_platform(Accid, RoleID, Process)
  when erlang:is_integer(Accid), erlang:is_integer(RoleID), erlang:is_integer(Process), Process >=0, Process =< 2 ->
    case catch gen_server:call({global, util:get_platform_server()}, {process_create, Accid rem ?AccidBase, RoleID, Process}) of
        ok ->
            ok;
        Error ->
            ?ERR("Error:~w", [Error])
    end.

%% 清理进程字典中的缓存
%clear_dict() ->
%	erlang:erase(?roleInfo).

%% 消息路由
route(_Client, #cs_account_login{}) ->
	ignore;
route(Client, Data) ->
	MsgTag = element(1, Data),
	case proto_route:route(MsgTag) of
		{role, HandleModule} ->
			?DEBUG("handle module=~w",[HandleModule]),
			erlang:send(Client#client.roleServer, {client_msg, HandleModule, Data});
		{Server, _HandleModule} ->
			catch erlang:send(Server, {client_msg, Client#client.roleID, Data})
	end.


%% 检查重复登录
check_login_again(RoleID) ->
    RegName = role_lib:gatewayRegName(RoleID),
    case whereis(RegName) of
        ?undefined ->
            true = erlang:register(RegName, self()),
            false;
        Pid ->
            Ref = monitor_pid(Pid), 
            erlang:send(Pid, login_again),
            receive
                {'DOWN', Ref, process, Pid, _} ->
                    case catch erlang:register(RegName, self()) of
                        true ->
                            next;
                        _Err ->
%%                             ?ERR("Err:~w", [Err]),
                            ?CATCH(erlang:unregister(RegName)),
                            timer:sleep(5000),
                            case catch erlang:register(RegName, self()) of
                                true ->
                                    next;
                                Err2 ->
                                    ?ERR("Err2:~w", [Err2]),
                                    erlang:exit(Err2)
                            end
                    end,
                    true
            after 5000 ->
                erlang:exit(exit_gw_timeout)
            end
    end.

monitor_pid(Pid) ->
    case catch erlang:monitor(process, Pid) of
        Ref when erlang:is_reference(Ref) ->
            Ref;
        Other ->
            ?ERR("Other:~w", [Other]),
            erlang:exit(no_ref)
    end.

get_ip(Socket) ->
	case inet:peername(Socket) of
		{ok, {Ip,_Port}} ->
			Ip;
		_ ->
			""
	end.
