%% @author admin
%% @doc 机器人
%% Created 2013-5-17


-module(robot).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-define(sec(E), E*1000).
-define(IP, "192.168.0.124").
-define(PORT, 12315).
-define(VERSION, "1.0.0").
-define(SERVER_ID, 1).
-define(FIX_ADD_SECONDS, 700).

-define(SEC_LOGIN_NUM, 3).
-define(ONLINE_INTERVAL, 120).

-define(PVP_DEBUG,false).
-define(FRIEND_DEBUG,false).

c_login()->
	c_login(121417).
c_login(ID)->
	X = spawn(fun()->login2(ID) end),
	put(client_pid, X).

c_send(Info)->
	X = get(client_pid),
	erlang:send(X, {send_client, Info}).



s(Num0, Num) ->
	util:foldl(fun(_,Acc) ->
						Acc2 = Acc+?SEC_LOGIN_NUM,
						timer:sleep(50),
						if Acc2 >= Num ->
							   spawn(fun() -> start(Acc+1,Num) end),
							   {return, ok};
						   true ->
							   spawn(fun() ->start(Acc+1,Acc2) end),
							   Acc2
						end
			   end, Num0-1, lists:seq(1,10000)).

start(RankLow, RankHigh) ->
	NowSec = util:now(),
	PidList = [{E, login(E), NowSec+random:uniform(?ONLINE_INTERVAL)} || E<- lists:seq(RankLow,RankHigh)],

	manage_loop(PidList).

manage_loop(PidList) ->
	sleep(1),
	NowSec = util:now(),
	List2 =
	[begin
		 if NowSec =< EndSec ->
				M;
			true ->
				exit(Pid,normal),
				{E, login_again(E), NowSec+random:uniform(?ONLINE_INTERVAL)}
		 end
	 end || {E,Pid, EndSec}=M <- PidList],
	manage_loop(List2).
	

sleep(Sec) ->
	timer:sleep(Sec*1000).


%% ====================================================================
%% Internal functions
%% ====================================================================
-define(TCP_TIMEOUT, -1). % 解析协议超时时间
-define(HEART_TIMEOUT, -1). % 心跳包超时时间
-define(HEART_TIMEOUT_TIME, 100). % 心跳包超时次数
-define(HEADER_LENGTH, 2). % 消息头长度
%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.
	
%% 连接服务器
connect() ->
	{ok, S} = gen_tcp:connect(?IP,?PORT, ?TCP_OPTIONS),
    ?ERR("~w", [S]),
	put(socket,S),
	{ok, S}.

send_p(Record) ->
	Socket = get(socket),
	case tk_misc:send_sock(Socket, Record) of
		ok ->
			ok;
		_ ->
			exit(normal)
	end.

%% 创建服务器消息处理程序
spawn_receiver() ->
	Socket = get(socket),
	Father = self(),
    Rank = get(rank),
	ReceiverPid= spawn(fun() ->
							   put(father, Father),
                               put(rank, Rank),
				  receive {go,S} ->recv_loop(S)  end
		  end),
	ok = gen_tcp:controlling_process(Socket, ReceiverPid),
	ReceiverPid ! {go, Socket},
	put(receiver_pid, ReceiverPid).

%% 接收服务器消息
recv_loop(Socket) ->
	Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	receive
		{inet_async, Socket, Ref, {ok, <<BodyLen:16>>}} ->
			Ref1 = async_recv(Socket, BodyLen-2, ?TCP_TIMEOUT),
			receive
				{inet_async, Socket, Ref1, {ok, Binary}} ->
					case catch proto:decode(Binary) of
						{'EXIT',_Info} ->
                            ?ERR("decode err", []),
							ignore;
						Msg ->
                            ?DEBUG("msg=~10000p",[Msg]),
							erlang:send(get(father), Msg)
					end,
					recv_loop(Socket);
				Msg ->
					?ERR("unexpected msg=~w",[Msg])
			end;
		{inet_async,_,_,{error,closed}} ->
            ?ERR("{~w error,closed}", [get(rank)]),
			ignore;
		Msg ->
			?ERR("unexpected msg=~w",[Msg])
	end.


%% 模拟登录
login_again(Rank) ->
%%     sleep(5),
    login(Rank).

login(Rank) ->
	spawn(fun() ->login2(Rank) end).

login2(Rank) ->
	Accid = gen_account:accid(Rank),
	put(rank, Rank),
	put(accountName, Accid),
	connect(),
	spawn_receiver(),
	auth(),
%% 	enter_game(),
	request_roleInfo(),
	case ?PVP_DEBUG of
		true -> send_p(#cs_pvp_get_list{});
		_->ignore
	end,
	case ?FRIEND_DEBUG of
		true -> erlang:send_after(random:uniform(10)*1000, self(), {send_client,#cs_friend_get_list{type=1}});
		_->ignore
	end,
	loop().

loop() ->
	receive 
		{send_client, Msg} ->
			send_p(Msg);
		Info ->
			test(Info)
	after 5000->
			send_p(#cs_account_heart{})
	end,
	loop().

%% 登录认证
auth() ->	
	send_p(#cs_version{version=?VERSION}),
	Accid = get(accountName),
	NowSec=util:now() + ?FIX_ADD_SECONDS,
	Ticket = util:md5(integer_to_list(Accid) ++ integer_to_list(NowSec) ++ ?TICKET),
    ServerID = ?SERVER_ID,
	Record = #cs_account_login{accountName="",
							   unixTime=NowSec,
							   userID=Accid,
							   ticket=Ticket,
                               serverID=ServerID,deviceID=Ticket,srcType=1},
	send_p(Record),
	receive 
		#sc_account_login{result=Result, isCreated = IsCreated} ->
			if Result =/= 1 ->
				   ?ERR("~w login auth fail, result=~w, exit\n",[get(rank), Result]),
				   exit(fail);
			   true ->
				   ?ERR("~w auth success\n", [get(rank)]),
				   ensure_role_exist(IsCreated)
			end
	end.

%% 获取角色列表（并创建角色）
ensure_role_exist(true)->
	next;
ensure_role_exist(false)->
	create_role().
					
roleName() ->
	Accid = get(accountName),
	"robot"++integer_to_list(Accid).

%% 创建角色
create_role() ->
	send_p(#cs_account_create{sex=1,roleName=roleName()}),
	send_p(#cs_role_select_ger{gerTypeID=lists:nth(random:uniform(3), [5020, 5120, 5100])}),
	receive
		#sc_account_create{result=Result} ->
			if Result =:= 1 ->
				   ?DEBUG("create sucess, name:~s\n", [roleName()]),
				   next;
			   true ->
				   ?ERR("~s, create_role fail, result=~w, exit\n",[roleName(), Result]),
				   exit(fail)
			end
	end.

%% 进入游戏
%% enter_game() ->
%% 	send_p(#cs_account_enter_game{}),
%% 	receive
%% 		#sc_account_enter_game{result=Result} ->
%% 			if Result =:= 1 ->
%% 				   next;
%% 			   true ->
%% 				   ?DEBUG("enter_game fail, result=~w\n",[Result]),
%% 				   exit(fail)
%% 			end
%% 	end.

%% 获取角色基本信息
request_roleInfo() ->
	send_p(#cs_explore_auto_explore_check{}),
%exit(normal),
	send_p(#cs_role_info{}),
	send_p(#cs_invite_info{}),
	send_p(#cs_ger_info{}),
	send_p(#cs_item_bag{}),
	send_p(#cs_item_equip{}),
	send_p(#cs_ger_pos_list{}),
	send_p(#cs_battle_progress{}),
	send_p(#cs_explore_list{}),
	send_p(#cs_shop_buy_num{}),
	send_p(#cs_pvp_get_list{}),
	send_p(#cs_card_get_list{}),
	send_p(#cs_daily_get_list{}),
	send_p(#cs_message_notice{curMaxNoticeID=0}),
	send_p(#cs_mail_unread_num{}),
	send_p(#cs_gather_get_list{type=1}),
	send_p(#cs_gather_get_list{type=2}),
	send_p(#cs_friend_get_list{type=1}),
	send_p(#cs_friend_get_list{type=2}),
	receive
		#sc_role_info{
					  roleID=RoleID}=Msg ->
			?DEBUG("msg=~10000p",[Msg]),
			put(roleID,RoleID),
			next
		
	end,
	fight_loop().

fight_loop() ->
	send_p(#cs_role_demo_fight{type=1}),
%%     send_p(#cs_hula_fight{}),
%%     send_p(#cs_nanm_fight{}),
    send_p(#cs_account_heart{}),
%%     send_p(#cs_alien_info{}),
%%     ?ERR("cs_alien_info"),
%% 	flush_receive(),
    lib:flush_receive(),
 	sleep(20),
	fight_loop().

flush_receive() ->
    receive
        Any ->
            case Any of
                #sc_alien_info{fighterList=FighterList} ->
                    catch send_alien_fight(lists:keydelete(get(roleID), #p_alien_fighter.roleID, FighterList));
                #sc_alien_fight{result=Result} ->
                    ?ERR("Result:~w", [Result]),
                    next;
                _ ->
                    next
            end,
            flush_receive()
    after
            10000 ->
            ok
    end.

send_alien_fight(FighterList) ->
    Now = util:now(),
    case lists:filter(fun(#p_alien_fighter{canBeAtkTime=CanBeAtkTime}) ->
                              CanBeAtkTime < Now
                      end, FighterList) of
        [#p_alien_fighter{roleID=RoleID,rank=Rank}|_] ->
            ?ERR("RoleID:~w, Rank:~w", [RoleID, Rank]),
            send_p(#cs_alien_fight{tarRoleID=RoleID,tarRank=Rank});
        _ ->
            next
    end.

	
t() ->
	user_default:app([sasl,logger]),
	case file:consult("d:/robot.config") of
		{ok, [RankLow,RankHigh]} ->
			s(RankLow,RankHigh)
	end.



%% -define(BATTLE_PROGRESS,battle_progress).
-define(PVP_LIST,pvp_list).
-define(FRIEND_LIST,friend_list).

%% request_battle_progress()->
%% 	send_p(#cs_battle_progress{}),
%% 	receive
%% 		#sc_battle_progress{}=Info->
%% 			put(?BATTLE_PROGRESS,Info)
%% 	end.

pvp_fight([])->
	ignore;
pvp_fight(PvpList)->
	[#p_pvp{rank=Rank,roleID=RoleID}] = util:random_list(PvpList, 1),
	Send = #cs_pvp_fight{rank=Rank,roleID=RoleID},
	send_p(Send).

test(#sc_pvp_get_list{pvpList=PvpList}) ->
	case ?PVP_DEBUG of
		true  -> pvp_fight(PvpList);
		_->ignore
	end;
test(#sc_pvp_fight{})->
	case ?PVP_DEBUG of
		true  -> erlang:send_after(2000, self(), {send_client,#cs_pvp_get_list{}});
		_->ignore
	end;
test(#sc_friend_get_list{roleInfoList=FriendList})->
	case ?FRIEND_DEBUG of
		true ->
			Len = length(FriendList),
			case Len>=50 of
				true->
					erlang:send_after(10000, self(), {send_client,#cs_friend_get_list{type=1}});
				false->
					erlang:send_after(10000, self(), {send_client,#cs_friend_get_list{type=1}}),
					send_p(#cs_friend_get_add_list{})
			end,
			lists:foreach(fun(#p_friend{roleID=RoleID,canSend=CanSend})->
								  case CanSend of
									  1->
										  send_p(#cs_friend_send_enargy{roleIDList=[RoleID]});
									  _->
										  ignore
								  end
						  end, FriendList),
			send_p(#cs_friend_give_all_enargy{});
		_->ignore
	end;
test(#sc_friend_get_add_list{roleList=L})->
	case ?FRIEND_DEBUG andalso L=/=[] of
		true ->
			Len = length(L),
			Num = random:uniform(Len),
			List = util:random_list(L, Num),
			lists:foreach(fun(#p_stranger{roleID=RoleID,canAdd=CanAdd})->
								  case CanAdd of
									  1->
										  send_p(#cs_friend_add{roleIDList=[RoleID]});
									  _->
										  ignore
								  end
						  end, List);
		_->ignore
	end;
test(#sc_mail_unread_num{newMailNum=NewMailNum})->
	[N,_,_] = NewMailNum,
	case N>0 of
		true->
			send_p(#cs_mail_info{type=1,clientTopMailUID=0});
		_->
			ignore
	end;
test(#sc_mail_info{mailList=MailList})->
	lists:foreach(fun(#p_mail{mailType=MailType,mailUID=MailUID})->
						  case MailType of
							  3->
								  send_p(#cs_mail_agree_friend{mailUID=MailUID});
							  _->
								  ignore
						  end
				  end, MailList);
test(_)->
	ignore.

send(S, Data, OptList) when is_port(S), is_list(OptList) ->
    ?ERR("prim_inet:send(~p, ~p)~n", [S,Data]),
    try erlang:port_command(S, Data, OptList) of
    false -> % Port busy and nosuspend option passed
        ?ERR("prim_inet:send() -> {error,busy}~n", []),
        {error,busy};
    true ->
        receive
        {inet_reply,S,Status} ->
            ?ERR("prim_inet:send() -> ~p~n", [Status]),
            Status
        end
    catch
    error:_Error ->
        ?ERR("prim_inet:send() -> {error,einval}~n", []),
         {error,einval}
    end.

-define(url, "http://msdk.qq.com/mpay/get_balance_m?sig=CcVMQ%2BmfVN2np0bkAnpxSHq9ZGM%3D&appid=1103881795&openid=514B2A41EFDEF27054CA4A035A9BA544&openkey=811E2F1DC684A2FE3F0128611E6093FD&pay_token=37AFC58838DE7BD3620EA746A9FBD121&pf=desktop_m_qq-2002-android-2002-qq-1103881795-514B2A41EFDEF27054CA4A035A9BA544&pfkey=d293102762c1be2e3b553907a54d757c&ts=1428896891&zoneid=44").
-define(cookie, "session_id=openid;session_type=kp_actoken;org_loc=%2Fmpay%2Fget_balance_m").
-define(timeout, 20000).
-define(connect_timeout, 15000).
-define(interval_ms, (300 * 1000)).

req(URL, Cookie) ->
    R = httpc:request(get, {URL,[{"Cookie", Cookie}]},[{timeout, ?timeout}, {connect_timeout, ?connect_timeout}], []),
    io:format("~10000p~n", [R]).

req() ->
    case httpc:request(get, {?url,[{"Cookie", ?cookie}]},[{timeout, ?timeout}, {connect_timeout, ?connect_timeout}], []) of
        {ok, {_,_,R}} ->
            {RS} = ejson:decode(R),
            Ret = get_value(RS, <<"ret">>),
            case Ret of
                0 ->
                    next;
                1001 ->
                    next;
                _ ->
                    ?ERR("~w", [R])
            end;
        Err ->
            ?ERR("Err:~w", [Err])
    end.

tt(Num, Fix) ->
    tk_misc:start_applications([sasl, logger, inets]),
    lists:foreach(fun(X) ->
                          erlang:spawn(fun() ->
                                               start_loop(lists:concat([Fix, X]), 0)
                                       end)
                  end, lists:seq(1, Num)).

start_loop(ID, Count) ->
    req_loop(ID, Count, util:now_mili()).

req_loop(ID, Count, StartMS) ->
    T1 = util:now_mili(),
    req(),
    T2 = util:now_mili(),
    Interval = T2 - T1,
    case Interval >= ?timeout of
        true ->
            next;
%%             ?ERR("ID:~s, Interval:~w, Count:~w", [ID, Interval, Count]);
        false ->
            next
    end,
    timer:sleep(1000),
    case T2 - StartMS > ?interval_ms of
        true ->
            case erlang:get(is_print) of
                ?undefined ->
                    erlang:put(is_print, true),
                    ?ERR("ID:~s, Count:~w", [ID, Count + 1]);
                _ ->
                    next
            end;
        false ->
            next
    end,
    req_loop(ID, Count + 1, StartMS).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            ?undefined;
        {Key, Value} ->
            Value
    end.