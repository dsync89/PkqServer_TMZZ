%% @author admin
%% @doc 创建帐号
%% Created 2013-2-26


-module(mod_account_create).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).
-export([check_account_name/1]).
%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 创建角色逻辑
handle(Req) ->
    QueryString = Req:parse_post(),
	%io:format("qs=~100p\n",[QueryString]),
    AccountName = proplists:get_value("account", QueryString),
    Password = proplists:get_value("password", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	PhoneNumber = case proplists:get_value("phoneNum", QueryString) of ?undefined -> ""; T -> T end,
	if AccountName =:= ?undefined orelse Password =:= ?undefined orelse DevID =:= ?undefined ->
		   ?ERR("create account, result=2"),
		   
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_account_name(AccountName) of
			   true ->
                   case check_name_valid(AccountName) of
                       true ->
                           SrcType =
                               case proplists:get_value("hd", QueryString) of
                                   ?undefined ->
                                       ?ACCOUNT_TYPE_NORMAL;
                                   _ ->
                                       ?ACCOUNT_TYPE_NORMAL_HD
                               end,
				   			case db_func:create_account(SrcType,AccountName, Password, PhoneNumber, DevID, util:get_ip_from_req(Req)) of
					   			{ok,_} ->
								   %?DEBUG("create account result=0"),
								   Reply = ejson:encode({[{<<"result">>,0}]}),
								   platform_tool:return(Req, Reply);
					   			{error, _}->
							   		?ERR("create account result=1"),
							   		Reply = ejson:encode({[{<<"result">>,1}]}),
							   		platform_tool:return(Req, Reply)
				   			end;
				   		_ ->
				   			?ERR("create account result=3"),
				   			Reply = ejson:encode({[{<<"result">>,3}]}),
				   			platform_tool:return(Req, Reply)
				   	end;
			   false ->
				   ?ERR("create account result=3"),
				   Reply = ejson:encode({[{<<"result">>,3}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.
	
	
%% 检查用户名称是否允许
check_account_name(AccountName) ->
	case AccountName of
		"guest"++_ ->
			false;
		_ ->
			case erlang:length(AccountName) < 1 orelse erlang:length(AccountName) > 20 of
				true ->
					false;
				false ->
					true
			end
	end.

check_name_valid([])->
	true;
check_name_valid([H|T]=_AccountName)->
	if H < $0 ->
		false;
		H > $9 andalso H < $A ->
		false;
		H > $Z andalso H < $a ->
		false;
		H > $z ->
		false;
		true ->
		check_name_valid(T)
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================


