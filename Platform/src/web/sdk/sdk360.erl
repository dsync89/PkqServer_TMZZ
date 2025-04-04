%% @author admin
%% @doc @todo Add description to sdk360.


-module(sdk360).
-include("common.hrl").
-include("record.hrl").

-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-define(ACT_CHECK_AUTH, 6).

check_auth(SessionID) ->
    URL2="https://openapi.360.cn/user/me.json?access_token="++ SessionID ++ "&fields=id,name,avatar",
    case httpc:request(get, {URL2, []}, [], []) of
        {ok,{_,_,R2}} ->
            {RS2} = ejson:decode(R2),
            case get_value(RS2, <<"id">>) of
                UserID when is_binary(UserID) ->
                    {true,binary_to_list(UserID)};
                _ ->
                    {false,2}
            end;
        _ ->
            {false,4}
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
%% ====================================================================
%% Internal functions
%% ====================================================================


