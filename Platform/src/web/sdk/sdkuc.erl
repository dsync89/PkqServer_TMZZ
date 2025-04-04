%% @author admin
%% @doc @todo uc平台sdk
%% Created 2013-8-19


-module(sdkuc).

-include("common.hrl").

-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

check_auth(SessionID) ->
	%true.
	Id1 = util:now(),
	Id = integer_to_list(Id1),
	Cpid = "22703",
	GameId = "550934",
	ChannelId = "2",
	ServerId = "0",
	AppKey = "be48f1f254ad9373ebe808c81bf366a3",
	%URL = "http://sdk.test4.g.uc.cn/ss",
    URL = "http://sdk.g.uc.cn/ss",
	ContentType = "application/json",
	MD5 = sign([Cpid,"sid=",SessionID,AppKey]),
	Content = "{\"id\":"++Id++",\"service\":\"ucid.user.sidInfo\",\"data\":{\"sid\":\""++SessionID++"\"},\"game\":{\"cpId\":"++Cpid++",\"gameId\":"++GameId++",\"channelId\":\""++ChannelId++"\",\"serverId\":"++ServerId++"},\"sign\":\""++MD5++"\"}",
	%Content2 = ejson:encode(Content),
	%Content3 = http_uri:encode(binary:bin_to_list(Content2)),
    % io:format("check_auth: Content:~s\n",[Content]),
	case httpc:request(post, {URL, [],ContentType,Content}, [], []) of
		{ok,{_,_,R}} ->
			{RS} = ejson:decode(R),
			case get_value(RS, <<"id">>) of
				false ->
					?ERR("check_auth:~p~n",[RS]),
					false;
				Id1 ->
					{State} = get_value(RS,<<"state">>),
					case get_value(State,<<"code">>) of
						1 ->
							{Data} = get_value(RS,<<"data">>),
							Ucid = get_value(Data,<<"ucid">>),
							integer_to_list(Ucid);
						ErrCode ->
							ErrReason = get_value(State,<<"msg">>),
							?ERR("code err:~p,~p~n",[ErrCode,ErrReason]),
							false
					end;	 
				_ ->
					?ERR("httpc:request fail:~p~n",[RS]),
					false
			end;
		_ ->
			io:format("httpc:request fail..."),
			false
	end.

check_order_serial(AppID,AppKey,OrderSerial) ->
	Sign = md5(AppID++"1"++OrderSerial++AppKey),
	URL = "http://service.sj.91.com/usercenter/ap.aspx?AppId="++AppID
 			  ++"&Act=1&CooOrderSerial="++OrderSerial++"&Sign="++Sign,
	case httpc:request(get, {URL,[]},[], []) of
		{ok, {_,_,R}} ->
			{RS} = ejson:decode(R),
%% 			io:format("~100000p",[RS]),
			case (get_value(RS, <<"ErrorCode">>) =:= <<"1">>) of
				true ->
					{true, get_value(RS, <<"GoodsCount">>), get_value(RS,<<"GoodsId">>)};
				false ->
					{false, 2}
			end;
		_ ->
			{false, 4}
	end.
	

%% ====================================================================
%% Internal functions
%% ====================================================================


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

sign(StrList) ->
	md5(sign2(StrList)).

sign2([A]) ->
	A;
sign2([A,B]) ->
	A++B;
sign2([]) ->
	"";
sign2([A|T]) ->
	A++sign2(T).
		
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

test() ->
	check_order_serial("107061","ddee2c0a5fdd0b7b7d363c01dbe153292667de1d2dc68488","33033FA8-F62F-4534-97AD-9C685B164F21").