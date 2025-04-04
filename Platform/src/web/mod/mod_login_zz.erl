%% @author admin
%% @doc @todo Add description to mod_login_zz.


-module(mod_login_zz).
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
	SessionID = proplists:get_value("sessionID", QueryString),
	DevID = proplists:get_value("devid", QueryString),
	ProductID = proplists:get_value("productID", QueryString),
	%?ERR("QS=~w,~w\n",[QueryString,SessionID]),
	case ProductID of
		?undefined ->
			do_old_login(SessionID, DevID,Req,QueryString);
		_ ->
			if SessionID =:= ?undefined orelse DevID =:= ?undefined ->
				   Reply = ejson:encode({[{<<"result">>,3}]}),
				   platform_tool:return(Req, Reply);
			   true ->
				   case check_auth(SessionID,ProductID) of
					   {true,UID,SdkUid, Token, MUserID} ->
						   case db_func:get_zz_accid(UID, DevID,util:get_ip_from_req(Req)) of
							   [] ->
								   Reply = ejson:encode({[{<<"result">>,1}]}),
								   platform_tool:return(Req, Reply);
							   [Accid] ->
								   Version = proplists:get_value("version", QueryString),
								   {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_ZZ, Version),
								   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
								   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
								   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
								   pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_ZZ),
								   %%io:format("~w",[LatestSLInfo]),
								   Reply = ejson:encode({[{<<"result">>,0},
														  {<<"accid">>,Accid},
														  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
														  {<<"valid_time">>,list_to_binary(ValidTime)},
														  {<<"login_history">>,list_to_binary(LatestSLInfo)},
														  {<<"server_list">>,ServerList},
														  {<<"user_name">>,list_to_binary(UID)},
														  {<<"sdkUid">>,SdkUid},
														  {<<"muserID">>,MUserID},
														  {<<"ac_token">>,Token}]}),
								   platform_tool:return(Req, Reply)
						   end;
					   {false,Reason}  ->
						   ?ERR("zz check failed, Reason:~w",[Reason]),
						   Reply = ejson:encode({[{<<"result">>,2}]}),
						   platform_tool:return(Req, Reply)
				   end
			end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(SessionID,ProductID)->
	%% 	AppKey = "",
	%% 	AppSecret = "",
	{AppKey, AppSecret} = get_client_info(ProductID),
	TokenUrl = "http://andrs.tisgame.com/andsrv/auth/getToken.lg",
	TokenParam = mochiweb_util:urlencode([{productId,ProductID},{redirect_uri,"1"},{client_id,AppKey},{client_secret,AppSecret},{grant_type,"authorization_code"},{code,SessionID}]),
	Response = httpc:request(post, {TokenUrl,[], "application/x-www-form-urlencoded",TokenParam}, [], []),
	case Response of
		{ok, {_,_,R}} ->
			{RS} = ejson:decode(R),
			%?ERR("~w",[RS]),
			case get_value(RS, <<"codes">>) of
				0 ->
					case get_value(RS, <<"access_token">>) of
						Token when is_binary(Token) ->
							InfoUrl = "http://andrs.tisgame.com/andsrv/auth/getUserInfo.lg",
							InfoParam = mochiweb_util:urlencode([{"access_token",Token}]),
							Response2 = httpc:request(post, {InfoUrl,[], "application/x-www-form-urlencoded",InfoParam}, [], []),
							case Response2 of
								{ok,{_,_,R2}} ->
									{RS2} = ejson:decode(R2),
									case get_value(RS2, <<"codes">>) of
										<<"0">> ->
											UserID = get_value(RS2, <<"username">>),
											SdkUid = get_value(RS2, <<"sdkuserid">>),
											MUserID = get_value(RS2, <<"id">>),
											{true,binary_to_list(UserID), SdkUid, Token,MUserID};
										Err4 ->
											?ERR("zz check err4:~p~n",[Err4]),
											{false,4}
									end;
								Err5 ->
									?ERR("zz check err5:~p~n",[Err5]),
									{false,5}
							end;
						Err6 ->
							?ERR("zz check err6: ~p~n",[Err6]),
							{false,6}
					end;
				ErrCode ->
					?ERR("zz check err:~p~n",[ErrCode]),
					{false,7}
			end;
		Err ->
			{false,Err}
	end.

get_client_info(ProductID)->
	case ProductID of
		"D10009A" ->
			{"dfd7549dda8447deab5dea448c514577","7ca635ee37f5bbac801d29e6fb9455f6"};
		"D10010" ->
			{"3e1e405ea677c32c436a4e03f6c19188","26599fbc5a95817142ca48c0ae000bd2"};
		_ ->
			{"0","0"}
	end.

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

do_old_login(SessionID, DevID,Req,QueryString) ->
	if SessionID =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,3}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case db_func:get_zz_accid(SessionID, DevID) of
			   [] ->
				   Reply = ejson:encode({[{<<"result">>,1}]}),
				   platform_tool:return(Req, Reply);
			   [Accid] ->
				   Version = proplists:get_value("version", QueryString),
				   {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_ZZ, Version),
				   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
				   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
				   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
				   pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_ZZ),
				   %%io:format("~w",[LatestSLInfo]),
				   Reply = ejson:encode({[{<<"result">>,0},
										  {<<"accid">>,Accid},
										  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
										  {<<"valid_time">>,list_to_binary(ValidTime)},
										  {<<"login_history">>,list_to_binary(LatestSLInfo)},
										  {<<"server_list">>,ServerList}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.
