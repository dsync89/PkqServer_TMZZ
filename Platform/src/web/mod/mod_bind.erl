%% @author admin
%% @doc 绑定游客帐号
%% Created 2013-6-20


-module(mod_bind).
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
	NewAccountName = proplists:get_value("newaccount", QueryString),
	NewPassword = proplists:get_value("newpassword", QueryString),
	PhoneNumber = case proplists:get_value("phoneNum", QueryString) of ?undefined -> ""; T -> T end,
	
	if AccountName =:= ?undefined 
		   orelse LoginKey =:= ?undefined 
		   orelse CurTime =:= ?undefined  
		   orelse NewAccountName =:= ?undefined  
		   orelse NewPassword =:= ?undefined 
		 ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   %% 判断是否是游客帐号
		   case AccountName of
			   "guest"++_ ->
				   %% 验证新的帐号名称合法
				   case mod_account_create:check_account_name(NewAccountName) of
					   false ->
						   Reply = ejson:encode({[{<<"result">>,3}]}),
						   platform_tool:return(Req, Reply);
					   true ->
						   %% 获取帐号信息
						   case db_func:get_account_info(AccountName) of
							   [] ->
								   Reply = ejson:encode({[{<<"result">>,1}]}),
								   platform_tool:return(Req, Reply);
							   [Accid, Password] ->
								   %% 验证密码
								   CaclKey = util:md5(AccountName++binary_to_list(Password)++CurTime++"moon_game_login_key"),
								   if CaclKey =:= LoginKey ->
										  %% 判断是否成功写入
										  case db_func:bind_account(Accid, NewAccountName,NewPassword,PhoneNumber) of
											  {ok, _} ->
												  Reply = ejson:encode({[{<<"result">>,0}]}),
												  platform_tool:return(Req, Reply);
											  {error,_} ->
												  %% 该帐号已存在
												  Reply = ejson:encode({[{<<"result">>,1}]}),
												  platform_tool:return(Req, Reply)
										  end;
									  
									  true ->
										  Reply = ejson:encode({[{<<"result">>,4}]}),
										  platform_tool:return(Req, Reply)
								   end
						   end;
					   _ ->
						   Reply = ejson:encode({[{<<"result">>,5}]}),
						   platform_tool:return(Req, Reply)
				   end
		   end
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


