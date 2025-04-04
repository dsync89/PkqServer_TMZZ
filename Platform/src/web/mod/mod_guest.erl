%% @author admin
%% @doc 创建游客帐号
%% Created 2013-2-26


-module(mod_guest).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 创建游客帐号
handle(Req) ->
	QueryString = Req:parse_qs(),
	%io:format("qs=~100p\n",[QueryString]),
    DevID = proplists:get_value("devid", QueryString),
    case erlang:is_list(DevID) of
        true ->
            NewDevID = DevID;
        false ->
            NewDevID = ""
    end,
	random:seed(now()),
    SrcType =
        case proplists:get_value("hd", QueryString) of
            ?undefined ->
                ?ACCOUNT_TYPE_NORMAL;
            _ ->
                ?ACCOUNT_TYPE_NORMAL_HD
        end,
	case random_account(3, NewDevID, SrcType, Req) of
		{true,Account,Passworld} ->
			Reply = ejson:encode({[{<<"result">>,1},
								   {<<"account">>,list_to_binary(Account)},
								   {<<"password">>,list_to_binary(Passworld)}
								  ]}),
			platform_tool:return(Req, Reply);
		false ->
			Reply = ejson:encode({[{<<"result">>,2}]}),
			platform_tool:return(Req, Reply)
	end.
	
random_account(0, _NewDevID, _SrcType, _Req) ->
	false;
random_account(N, NewDevID, SrcType, Req) ->	
	AccountName = "guest"++util:random_str2(12),
	Password = util:random_str2(11),
	case db_func:create_account(SrcType, AccountName, Password, "", NewDevID,util:get_ip_from_req(Req)) of
		{ok,_} ->
			{true, AccountName, Password};
		{error,_} ->
			random_account(N-1, NewDevID, SrcType,Req)
	end.
		

	

