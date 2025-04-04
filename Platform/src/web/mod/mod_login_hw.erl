%% @author admin
%% @doc @todo Add description to mod_login_hw.


-module(mod_login_hw).
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
	SessionID = proplists:get_value("sessionID", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	if SessionID =:= ?undefined orelse DevID=:= ?undefined->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(SessionID) of
			   {true, Return} ->
				   %Data2 = ejson:decode(Data),
				   Uid = binary:bin_to_list(get_value(Return, <<"userID">>)),
				   %% 其他参数什么用?
				   case db_func:get_hw_accid(Uid,DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_HW, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_HW),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false,Reason}  ->
				   ?ERR("hwlogin check failed, Reason:~w",[Reason]),
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
	Sec = erlang:integer_to_list(util:now()),
	URL = "https://api.vmall.com/rest.php",
	Body = "nsp_svc=OpenUP.User.getInfo&nsp_ts=" ++ Sec ++ "&access_token=" ++ http_uri:encode(SessionID),
	case httpc:request(post, {URL, [{"urlencode, utf-8"}], "application/x-www-form-urlencoded",Body}, [], []) of
		{ok,{_,H,RS}} ->
			%io:format("~w",[RS]),
			{R} = ejson:decode(RS),
			case get_value(R, <<"error">>) of
				<<"nsp_ts error">> ->
					{false,H};
				<<"invalid session">> ->
					{false, R};
				_ ->
					{true,R}
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


