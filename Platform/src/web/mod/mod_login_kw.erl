%% @author admin
%% @doc @todo Add description to mod_login_kw.


-module(mod_login_kw).
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
	SessionID = proplists:get_value("uid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	if SessionID =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(SessionID) of
			   {true, UID} ->
				   case db_func:get_kw_accid(UID, DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_KW, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_KW),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"kw_uid">>,list_to_binary(UID)},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false,Reason}  ->
				   ?ERR("kwlogin check failed, Reason:~w",[Reason]),
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
	URL = "http://user.dyn.mobi.kuaiwan.com/account/session/check/?sid=" ++ SessionID,
	case httpc:request(get, {URL, []}, [], []) of
		{ok,{_,_,R}} ->
			{RS} = ejson:decode(R),
			case get_value(RS, <<"ok">>) of
				true  ->
					{true, get_uid(RS)};
				_ ->
					{false,RS}
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

get_uid(RS) ->
	case get_value(RS, <<"data">>) of
		false ->
			{false,"data not found"};
		{Value} ->
			case get_value(Value,<<"user_id">>) of
				false->
					{false, "user_id not found"};
				Value2 ->
					binary_to_list(Value2)
			end
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

