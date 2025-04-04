%% @author admin
%% @doc @todo Add description to sdkdl.


-module(sdkdl).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-export([]).

check_auth(Uin,SessionID) ->
	%true.
	AppIDStr = "2816",
	AppKey = "XMjSwFA5",
	
	Sign = sign([SessionID, "|", AppKey]),
	URL = "http://connect.d.cn/open/member/info/?app_id="++AppIDStr
			  ++"&mid="++Uin++"&token="++SessionID++"&sig="++Sign,
	%% io:format("uin:~w,sessionid:~w\nURL=~w",[Uin,SessionID,URL]),
	case httpc:request(get, {URL, []}, [], []) of
		{ok,{_,_,R}} ->	
			{RS} = ejson:decode(R),
			case get_value(RS, <<"error_code">>) of
				0 ->
%% 					io:format("success"),
					true;
				Error ->
					?ERR("check_auth fail, error:~w",[Error]),
					false
			end;
		Err ->
			?ERR("check_auth fail, err:~w",[Err]),
			false
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
