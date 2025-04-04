%% @author admin
%% @doc 获取角色基本信息
%% Created 2013-2-26


-module(mod_login91).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).

%% Internal functions
-export([]).

-define(ACT_CHECK_AUTH, "4").

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 获取角色基本信息
handle(Req) ->
    QueryString = Req:parse_qs(),
	%io:format("QS=~p\n",[QueryString]),
    %AccountName = proplists:get_value("account", QueryString),
	Uin = proplists:get_value("uin", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	SessionID = proplists:get_value("sessionID", QueryString),
	if Uin =:= ?undefined orelse SessionID =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,3}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(Uin, SessionID) of
			   true ->
				   case db_func:get_91_accid(Uin, DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_91, Version),
						  ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
						   
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_91),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												 {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   false  ->
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

check_auth(Uin,SessionID) ->
	AppIDStr = "116508",
	AppKey = "7fa47abbd9086e7613bcbec5c2405b9024933be1a7c768fc",
	Sign = sign([AppIDStr, ?ACT_CHECK_AUTH, Uin,SessionID, AppKey]),
	URL = "http://service.sj.91.com/usercenter/AP.aspx?AppId="++AppIDStr
			  ++"&Act=4"++"&Uin="++Uin++"&SessionId="++SessionID++"&Sign="++Sign,
	case httpc:request(get, {URL, []}, [], []) of
		{ok,{_,_,R}} ->
			{RS} = ejson:decode(R),
			case get_value(RS, <<"ErrorCode">>) of
				<<"1">> ->
					true;
				_ ->
					false
			end;
		Err ->
            ?ERR("", [Err]),
			false
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

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").

%% ====================================================================
%% Internal functions
%% ====================================================================


