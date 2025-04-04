%% @author admin
%% @doc @todo Add description to mod_login_vivo.


-module(mod_login_vivo).
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
	Token = proplists:get_value("sessionID", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	if  Token =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(Token) of
			   {true, Uid} ->
				   case db_func:get_vivo_accid(Uid,DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_VIVO, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_VIVO),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false,Reason}  ->
				   ?ERR("vivologin check failed, Reason:~w",[Reason]),
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Token) ->
	URL = "https://usrsys.inner.bbk.com/auth/user/info",
	ContentType = "application/x-www-form-urlencoded",
	Body = "access_token="++Token,
	case httpc:request(post, {URL, [],ContentType,Body}, [], []) of
		{ok,{_,_,R}} ->
			%?ERR("R:~w",[R]),
			{RS} = ejson:decode(R),
			%?ERR("RS:~w",[RS]),
			case get_value(RS, <<"uid">>) of
				UID when is_binary(UID)->
					{true , erlang:binary_to_list(UID)};
				_ ->
					{false,"get vivo uid failed"}
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

test()->
	[{"sessionID","HHHXIZgBkxpJZ0dzytdtcePXpenDX7sF"},{"devid","50f95e79-25f2-3db2-8560-44d2be97990c"}].

