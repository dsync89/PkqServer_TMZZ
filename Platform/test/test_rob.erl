-module(test_rob).

-export([s/1]).

%-include("common.hrl").

-define(IP, "192.168.1.9").
-define(PORT, "12342").

s(N)->
	M = N div 100 + 1,
	F1 = fun(Low, High)->lists:seq(trunc(Low), trunc(High)) end,
	[spawn(fun()->map2(E) end)||E<-F1(0,M div 2)],
	sleep(3),
	[spawn(fun()->map3(E,M div 2) end)||E<-F1(M div 2,M)],
	sleep(4),
	[spawn(fun()->map4(E) end)||E<-F1(0,M)],
	io:format("1\n"),
	sleep(5),
	[spawn(fun()->map5(E) end)||E<-F1(0,M)],
	sleep(5),
	[spawn(fun()->map6(E) end)||E<-F1(0,N)],
	io:format("2\n"),
	sleep(8),
	[spawn(fun()->map5(E) end)||E<-F1(0,N)],
	io:format("3\n"),
	ok.

map2(N)->
	F1 = fun(Low, High)->lists:seq(Low, High-1) end,
	[spawn(fun()->reg(E) end)||E<-F1(N*100, (N+1)*100)].

map3(N,M)->
	map2(N),
	F1 = fun(Low, High)->lists:seq(Low, High-1) end,
	[spawn(fun()->getkey(E) end)||E<-F1((N-M)*100, (N-M+1)*100)],
	ok.

map4(N)->
	F1 = fun(Low, High)->lists:seq(Low, High-1) end,
	[spawn(fun()->sct_svr(E) end)||E<-F1(N*100, (N+1)*100)].

map5(N)->
	F1 = fun(Low, High)->lists:seq(Low, High-1) end,
	[spawn(fun()->getkey(E) end)||E<-F1(N*100, (N+1)*100)].

map6(N)->
	map6(50, N).
map6(0,N)->
	map4(N);
map6(M,N)->
	map4(N),
	map6(M-1, N).

reg(Id)->
	sleep(2),
	Accid = Id,
	Passwd = integer_to_list(Accid),
	Name = "robot_"++Passwd,
	Arg = lists:flatten(io_lib:format("account=~s&password=~s&phoneNum=~w", [Name, Passwd, Accid])),
	URL = lists:flatten(io_lib:format("http://~s:~s/create",[?IP,?PORT])),
	Ans = httpc:request(post, {URL,[], "application/x-www-form-urlencoded",Arg}, [], []),
%	?DEBUG("get : ~w",[Ans]),
	ok.
	
getkey(Id)->
	sleep(2),
	CurTime = util:now(),
	Accid = Id,
	Passwd = integer_to_list(Accid),
	Name = "robot_"++Passwd,
	LoginKey = util:md5(Name++Passwd++integer_to_list(CurTime)++"tk_login_key"),
	URL = lists:flatten(io_lib:format("http://~s:~s/baseinfo?account=~s&loginkey=~s&curtime=~w", [?IP,?PORT,Name,LoginKey,CurTime])),
	Ans = httpc:request(get, {URL, []}, [], []),
%	?DEBUG("get : ~w",[Ans]),
	ok.

sct_svr(Id)->
	sleep(1),
	{_,_,S} = erlang:now(),
	SvrID = (Id + (S div 1000)) rem 100,
	URL = lists:flatten(io_lib:format("http://~s:~s/select?accid=~w&serverID=~w", [?IP,?PORT,Id,SvrID])),
	Ans = httpc:request(get, {URL, []}, [], []),
%	?DEBUG("get : ~w",[Ans]),
	ok.

sleep(M)->
	timer:sleep(M*1000).

