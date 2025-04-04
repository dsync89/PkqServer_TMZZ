%% @author admin
%% @doc @todo Add description to mod_login_az.


-module(mod_login_az).
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
	Uid = proplists:get_value("uid", QueryString),
	SessionID = proplists:get_value("sessionid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
		
	if SessionID =:= ?undefined orelse Uid =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(Uid, SessionID) of
			   true ->
				   case db_func:get_az_accid(Uid, DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_AZ, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_AZ),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false,Reason}  ->
				   ?ERR("az login check failed, Reason:~w",[Reason]),
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Uid,SessionID) ->
	AppKey = "14180408139tIBxX0x3avB4X6Hrjny",
	AppSec = "mL7b1LHPC4Hrtm5Dr800e6Si",
	OriStr = AppKey++Uid++SessionID++AppSec,
	%% ?ERR("ori str:~p\n",[OriStr]),
	Sign = base64:encode_to_string(OriStr),
	Time = get_now(),
	Url = "http://user.anzhi.com/web/api/sdk/third/1/queryislogin",
	Message = mochiweb_util:urlencode([{time,Time},{appkey,AppKey},{account,Uid},{sid,SessionID},{sign,Sign}]),
	%% ?ERR("message:~p",[Message]),
	Response = httpc:request(post, {Url,[], "application/x-www-form-urlencoded",Message}, [], []),
	case Response of
		{ok, {_,_,Content3}} ->
			%% ?ERR("response:~w\ncontent3:~w",[Response,Content3]),
			Content4 = str_to_term(Content3),
			Result = proplists:get_value(sc,Content4),
			if Result == '1' ->
					true;
			   true ->
					?ERR("az order fail3:~w",[Content4]),
				   	{false,Result}
			end;
		Err ->
			?ERR("az order fail2:~p~n",[Err]),
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

get_now() ->
	{{Year,Mon,Day},{Hour,Min,Sec}} = erlang:localtime(),
	Year2 = integer_to_list(Year),
	if Mon < 10 ->
			Mon2 = "0"++integer_to_list(Mon);
		true ->
			Mon2 = integer_to_list(Mon)
	end,
	if Day < 10 ->
			Day2 = "0"++integer_to_list(Day);
		true ->
			Day2 = integer_to_list(Day)
	end,
	if Hour < 10 ->
			Hour2 = "0"++integer_to_list(Hour);
		true ->
			Hour2 = integer_to_list(Hour)
	end,
	if Min < 10 ->
			Min2 = "0"++integer_to_list(Min);
		true ->
			Min2 = integer_to_list(Min)
	end,
	if Sec < 10 ->
			Sec2 = "0"++integer_to_list(Sec);
		true ->
			Sec2 = integer_to_list(Sec)
	end,
	Year2++Mon2++Day2++Hour2++Min2++Sec2++"001".



str_to_term(Str)->
	M = lists:foldl(fun(E,Acc)->
							case E of
								$,->
									[${,$,,$}|Acc];
								$:->
									[$,|Acc];
								${->
									[${,$[|Acc];
								$}->
									[$],$}|Acc];
								P ->
									[P|Acc]
							end end,[], Str),
	M1 = lists:flatten(lists:reverse(["."|M])),
	{ok,Tokens,_} = erl_scan:string(M1),
	{ok,M3} = erl_parse:parse_term(Tokens),
	M3.



