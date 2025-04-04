%% @author admin
%% @doc 修改密码
%% Created 2013-2-26


-module(mod_change_password).
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
    AccountName = proplists:get_value("account", QueryString),
	LoginKey = proplists:get_value("loginkey", QueryString),
	NewPassword = proplists:get_value("newpassword", QueryString),
	CurTime = proplists:get_value("curtime", QueryString),
	if AccountName =:= ?undefined orelse LoginKey =:= ?undefined orelse CurTime =:= ?undefined ->
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
						  ets:delete(?ETS_ACC_INFO,AccountName),
						  db_func:change_account_password(Accid, NewPassword),
						  Reply=ejson:encode({[{<<"result">>, 0}]}),
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


