%% @author admin
%% @doc @todo Add description to mod_login_jl.


-module(mod_login_jl).
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
	DevID = proplists:get_value("devid", QueryString),
	% Token = proplists:get_value("sessionID", QueryString),
	Uid = proplists:get_value("uid", QueryString),
	if Uid =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth() of
			   {true}->
				   case db_func:get_jl_accid(Uid,DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_JL, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
						   pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_JL),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false, Reason}->
				   ?ERR("jl login check failed, Reason:~w",[Reason]),
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth()->
	{true}.

%% 金立测试帐号的playerID无法验证,不对登录进行验证.
check_auth(SessionID) ->
	Url="https://id.gionee.com/account/verify.do",
	TS = integer_to_list(util:now()),
	Nonce = integer_to_list(util:now(), 16),
	BaseString = TS ++ "\n" ++ Nonce ++ "\nPOST\n/account/verify.do\nid.gionee.com\n443\n\n",
	Mac1 = crypto:hmac('sha',list_to_binary("7798C224BB5345C09AA1CCE10BDC8718"), list_to_binary(BaseString)),
	Sign = binary_to_list(base64:encode(Mac1)),
	Header = "MAC id=\"D3F2878F7FAC422DAA42961A4356B505\",ts=\""++TS++"\",nonce="++Nonce++",mac=\"" ++ Sign ++"\"",
	case httpc:request(post,{Url,[{"Authorization",Header}],"application/json",SessionID},[],[]) of
		{ok,{_,_,R}}->
			{RS} = ejson:decode(R),
			case get_value(RS, <<"u">>) of
				false ->
					?ERR("jl login err:RS:~w",[RS]),
					{false,"return no such user"};
				ID ->
							{true, binary:bin_to_list(ID)}
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
	[{"sessionID","1407e846dffd853f62378ee804078038"},{"oauth_token_secret","c8cb73b210924ff966a7e2f593880d89"},
                                      {"devid","50f95e79-25f2-3db2-8560-44d2be97990c"}].


