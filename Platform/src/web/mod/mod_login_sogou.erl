%% @author admin
%% @doc @todo Add description to mod_login_sogou.


-module(mod_login_sogou).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 获取角色基本信息
handle(Req) ->
    QueryString = Req:parse_qs(),
	%?ERR("QS=~p\n",[QueryString]),
	%%SessionID = proplists:get_value("sessionID", QueryString),
	SessionKey = proplists:get_value("key", QueryString),
	Uid = proplists:get_value("uid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	if DevID =:= ?undefined orelse SessionKey =:= ?undefined orelse Uid =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(SessionKey, Uid) of
			   true ->
				   case db_func:get_sogou_accid(Uid, DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_SOGOU, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_SOGOU),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"uid">>,list_to_binary(Uid)},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false,Reason}  ->
				   ?ERR("sogoulogin check failed, Reason:~w",[Reason]),
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(SessionKey, Uid) ->
	AppSec = "&07b80c217570f136c2e2ed2de3e79f31d6b520350fb6e20b051b8699c1d3ed45",
	Str = "gid=268&session_key="++SessionKey++"&user_id="++Uid,
	Sign = sign([Str,AppSec]),
	URL = "http://api.app.wan.sogou.com/api/v1/login/verify?"
	%% test enviroment
	%URL = "http://dev.app.wan.sogou.com/api/v1/login/verify?"
			  ++Str++"&auth="++Sign,
	case httpc:request(post, {URL, [],"application/x-www-form-urlencoded",[]}, [], []) of
		{ok,{_,_,R}} ->
			%io:format("~w\n",[R]),
			{RS} = ejson:decode(R),
			case get_value(RS, <<"result">>) of
				true ->
					true;
				_ ->
					{false,R}
			end;
		Err ->
			{false,Err}
	end.
	
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

test()->
	[{"uid","8416967"},
                                       {"key",
                                        "ddc13d6960b89f57fa2541fed53aa080c223bfc597dd48f35d16fe7edf618bea"},
                                       {"devid",
                                        "50f95e79-25f2-3db2-8560-44d2be97990c"}].
