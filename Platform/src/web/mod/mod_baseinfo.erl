%% @author admin
%% @doc 获取角色基本信息
%% Created 2013-2-26


-module(mod_baseinfo).
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
    AccountName = proplists:get_value("account", QueryString),
	LoginKey = proplists:get_value("loginkey", QueryString),
	CurTime = proplists:get_value("curtime", QueryString),
	if AccountName =:= ?undefined orelse AccountName =:= [] orelse LoginKey =:= ?undefined orelse CurTime =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,3}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case db_func:get_account_info(AccountName) of
			   [] ->
				   Reply = ejson:encode({[{<<"result">>,1}]}),
				   platform_tool:return(Req, Reply);
			   [Accid, Password] ->
				   CaclKey = util:md5(AccountName++binary_to_list(Password)++CurTime++"moon_game_login_key"),
				   if CaclKey =:= LoginKey ->
                          Version = proplists:get_value("version", QueryString),
                          SrcType =
                              case proplists:get_value("hd", QueryString) of
                                  ?undefined ->
                                      ?ACCOUNT_TYPE_NORMAL;
                                  _ ->
                                      ?ACCOUNT_TYPE_NORMAL_HD
                              end,
                          {DelServerIDList, ServerList} = tk_config:get_server_list(SrcType, Version),
						  ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						  ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						  LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
%% 						  io:format("~w",[LatestSLInfo]),
                          pm_platform_server:add_loglogin(Accid, SrcType),
						  Reply = ejson:encode({[{<<"result">>,0},
												 {<<"accid">>,Accid},
												 {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												 {<<"valid_time">>,list_to_binary(ValidTime)},
												 {<<"login_history">>,list_to_binary(LatestSLInfo)},
												 {<<"server_list">>,ServerList}]}),
						  platform_tool:return(Req, Reply);
					  true ->
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


