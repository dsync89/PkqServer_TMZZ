%% @author admin
%% @doc @todo Add description to mod_login_lenovo.


-module(mod_login_lenovo).
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
	SessionID = proplists:get_value("sessionID", QueryString),
	if SessionID =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(SessionID) of
			   {true,Uid}->
				   case db_func:get_lenovo_accid(Uid,DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_LENOVO, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
						   pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_LENOVO),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false, Reason}->
				   ?ERR("lenovo login check failed, Reason:~w",[Reason]),
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(SessionID) ->
	Realm = "&realm=10001009.realm.lenovoidapps.com",
	URL = "http://passport.lenovo.com/interserver/authen/1.2/getaccountid?lpsust="++SessionID++Realm,
	case httpc:request(get,{URL,[]},[],[]) of
		{ok,{_,_,R}}->
			%?ERR("info:~w",[R]),
			get_info(R);
		Err ->
			{false,Err}
	end.

get_info(Data)->
	{ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(Data),
	case xmerl_xpath:string("//AccountID/text()",ParsedDocumentRootElement) of
		[{_,_,_,_,AccountID,_}] ->
			{true, AccountID};
		_ ->
			[{_,_,_,_,ErrorCode,_}] = xmerl_xpath:string("//Code/text()",ParsedDocumentRootElement),
			{false, ErrorCode}
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
	[{"sessionID",
	  "ZAgAAAAAAAGE9MTAwMTg4NTUwMDgmYj0yJmM9MSZkPTExNTQyJmU9M0RDRUUxQUUzRUY4RkVFMzUyQ0E1NDRENkQ1NjFCODUxJmg9MTM5MDg3NTM3NjYzMyZpPTQzMjAwJmo9MCZvPTg2MDU1NjAwMDAzOTM1NCZwPWltZWkmcT0wJnVzZXJuYW1lPWtpbmV0aW5nJTQwc2luYS5jb231CY6c4GiOkq07O-xDtSiT"},
	 {"devid",
	  "50f95e79-25f2-3db2-8560-44d2be97990c"}].