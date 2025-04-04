%% @author admin
%% @doc 测试账号服务器的功能
%% Created 2013-2-26


-module(test_request).

%% API functions
-export([]).

%% Internal functions
-export([]).

-define(IP, "42.62.40.33").
-define(PORT, 12380).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
start() ->
	inets:start(),
		httpc:request(post, {"http://42.62.40.33:12380/create",
							  [], "application/x-www-form-urlencoded",
                             lists:concat(["account=","test001","&password=","abcdefgh","&phoneNum=","11111111111","&devid=","1111111111111111"])}, [], []).
	
start2() ->
	inets:start(),
	CurTime = util:now(),
	Account = "test001",
	Password = "abcdefgh",
	LoginKey = util:md5(Account++Password++integer_to_list(CurTime)++"tk_login_key"),
	URL = lists:flatten(io_lib:format("http://42.62.40.33:12380/baseinfo?account=~s&loginkey=~s&curtime=~w", [Account,LoginKey,CurTime])),
		httpc:request(get, {URL, []}, [], []).

start3() ->
	inets:start(),
	URL = lists:flatten(io_lib:format("http://42.62.40.33:12380/login91?account=~s&uin=~s&sessionID=~s", ["test002","111111","dfwfwefsdfsf"])),
		httpc:request(get, {URL, []}, [], []).
	
start4() ->
	inets:start(),
	URL = lists:flatten(io_lib:format("http://42.62.40.33:12380/guest?macaddr=~s", ["12-12-12-12"])),
		httpc:request(get, {URL, []}, [], []).

start5() ->
	CurTime = util:now(),
	Account = "guestaBp5a2LfSovq",
	Password = "aqmBxMpV1hB",
	LoginKey = util:md5(Account++Password++integer_to_list(CurTime)++"tk_login_key"),
	URL = lists:flatten(io_lib:format("http://42.62.40.33:12380/bind?account=~s&loginkey=~s&curtime=~w&newaccount=adefde&newpassword=dwesdfsd", [Account,LoginKey,CurTime])),
		httpc:request(get, {URL, []}, [], []).

%% ====================================================================
%% Internal functions
%% ====================================================================
test_normal(Count) when Count > 0 ->
    application:start(sasl),
    application:start(logger),
    application:start(inets),
    test1(Count, random:seed0()).

test1(0, _RS) ->
    ok;
test1(Count, RS) ->
    {Integer, RS1} = random:uniform_s(999999999999, RS),
    Account = integer_to_list(Integer,36),
    Pass = Account,
    {DevInteger, RS2} = random:uniform_s(9999999999999999999999999999, RS1),
    DevID = integer_to_list(DevInteger,36),
    {_SleepTime, RS3} = random:uniform_s(1000 * 60 * 5, RS2),
    erlang:spawn(
      fun() -> 
%%               timer:sleep(SleepTime),
              do_test1(Account, Pass, DevID, true)
      end),
    test1(Count - 1, RS3).

do_test1(Account, Password, DevID, IsCreate) ->
    case IsCreate of
        false ->
            httpc:request(post, {"http://42.62.40.33:12380/create",
                                 [], "application/x-www-form-urlencoded",
                                 lists:concat(["account=",Account,"&password=",Password,"&phoneNum=","11111111111","&devid=",DevID])}, [], []);
        true ->
            next
    end,
    CurTime = util:now(),
    LoginKey = util:md5(Account++Password++integer_to_list(CurTime)++"tk_login_key"),
    URL = lists:flatten(io_lib:format("http://42.62.40.33:12380/baseinfo?account=~s&loginkey=~s&curtime=~w", [Account,LoginKey,CurTime])),
    httpc:request(get, {URL, []}, [], []),
    timer:sleep(300000),
    do_test1(Account, Password, DevID, true).
