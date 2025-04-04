%% @author admin
%% @doc 获取角色基本信息
%% Created 2013-2-26


-module(mod_loginuc).
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
	%io:format("QS=~p\n",[QueryString]),
    %AccountName = proplists:get_value("account", QueryString),
	%Uin = proplists:get_value("uin", QueryString),
	SessionID = proplists:get_value("sessionID", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	if SessionID =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,3}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case sdkuc:check_auth(SessionID) of
			   Ucid when is_list(Ucid) ->
				   case db_func:get_uc_accid(Ucid, DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_UC, Version),
						  ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
						  
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_UC),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												 {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   _  ->
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").

%% ====================================================================
%% Internal functions
%% ====================================================================


