%% @author admin
%% @doc @todo Add description to mod_login_pp.


-module(mod_login_pp).
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
	Token = proplists:get_value("sessionID", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	if  Token =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(Token) of
			   {true, Uid} ->
				   case db_func:get_pp_accid(Uid,DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_PP, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_PP),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false,Reason}  ->
				   ?ERR("pplogin check failed, Reason:~w",[Reason]),
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Token) ->
    Now = util:now(),
    Md5 = util:md5("sid=" ++ Token ++ "1e09ff50da9f9c4d2322f8b9528c9493"),
    Body = "{\"id\":" ++ erlang:integer_to_list(Now) ++ ",\"service\":\"account.verifySession\",\"game\":{\"gameId\":5005},\"data\":{\"sid\":\"" ++ Token ++ "\"},\"encrypt\":\"MD5\",\"sign\":\"" ++ Md5 ++ "\"}",
	case httpc:request(post, {"http://passport_i.25pp.com:8080/account?tunnel-command=2852126760",
                              [],
                              "application/json",
                              Body}, [], []) of
		{ok,{_,_,R}} ->
			{RS} = ejson:decode(R),
			case get_value(RS, <<"id">>) of
				Now ->
                    State = lists:flatten(erlang:tuple_to_list(get_value(RS, <<"state">>))),
                    Code = get_value(State, <<"code">>),
                    Msg = get_value(State, <<"msg">>),
                    case Code =:= 1 andalso Msg =:= <<"OK">> of
                        true ->
                            Data = lists:flatten(erlang:tuple_to_list(get_value(RS, <<"data">>))),
                            {true, erlang:binary_to_list(get_value(Data, <<"accountId">>))};
                        false ->
                            {false, {error_code, Code, Msg}}
                    end;
				_ ->
					{false, error_id}
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

test()->
	X=check_auth("80a5fe53d3540300005a17e308a4b1fb"),
	?ERR("what is X:~w",[X]).

