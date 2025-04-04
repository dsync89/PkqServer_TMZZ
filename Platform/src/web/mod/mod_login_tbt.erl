%% @author admin
%% @doc @todo Add description to mod_login_tbt


-module(mod_login_tbt).
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
	SessionID = proplists:get_value("sessionid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	if SessionID =:= ?undefined orelse DevID =:= ?undefined ->
		   Reptbt = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reptbt);
	   true ->
		   case check_auth(SessionID) of
			   {true,Uid} ->
				   case db_func:get_tbt_accid(Uid,DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reptbt = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reptbt);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_TBT, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_TBT),
						   Reptbt = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reptbt)
				   end;
			   {false,Reason}  ->
				   ?ERR("tbtlogin check failed, Reason:~w",[Reason]),
				   Reptbt = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reptbt)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(SessionID) ->
	URL = "http://tgi.tongbu.com/checkv2.aspx?k="++SessionID,
	case httpc:request(get, {URL, []}, [], []) of
		{ok,{_,_,R}} ->
			{true,R};
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

