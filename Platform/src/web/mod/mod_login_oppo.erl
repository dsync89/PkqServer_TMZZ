%% @author admin
%% @doc @todo Add description to mod_login_oppo.


-module(mod_login_oppo).
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
	Secret = proplists:get_value("oauth_token_secret", QueryString),
	if SessionID =:= ?undefined orelse DevID =:= ?undefined orelse Secret =:=?undefined ->
		   Reply = ejson:encode({[{<<"result">>,2}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(SessionID,Secret) of
			   {true,Uid}->
				   case db_func:get_oppo_accid(Uid,DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_OPPO, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
						   pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_OPPO),
						   Reply = ejson:encode({[{<<"result">>,0},
												  {<<"accid">>,Accid},
												  {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
												  {<<"valid_time">>,list_to_binary(ValidTime)},
												  {<<"login_history">>,list_to_binary(LatestSLInfo)},
												  {<<"server_list">>,ServerList}]}),
						   platform_tool:return(Req, Reply)
				   end;
			   {false, Reason}->
				   ?ERR("oppo login check failed, Reason:~w",[Reason]),
				   Reply = ejson:encode({[{<<"result">>,2}]}),
				   platform_tool:return(Req, Reply)
		   end
	end.

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").
%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(SessionID,Secret) ->
	Url="http://thapi.nearme.com.cn/account/GetUserInfoByGame",
	Base = "POST&http%3A%2F%2Fthapi.nearme.com.cn%3A8087%2Faccount%2FGetUserInfoByGame&",
	BaseStr = merge_baseString_info(SessionID),
	BaseStr1 = uri_encode(BaseStr),
	Data = list_to_binary(Base++BaseStr1),
	%%　Key = "5dB0F06b4f12bCf4D81cd0D8C3C4877c"++Secret,
	Key = "BCefcf9Ae75505eD777bbd3E78971834"++Secret,
	<<Mac:160/integer>> = crypto:hmac('sha',list_to_binary(Key), Data),
	Sign = http_uri:encode(binary_to_list(base64:encode(lists:flatten(io_lib:format("~40.16.0b", [Mac]))))),
	%URL= Url ++ "?"++ BaseStr1 ++ "&signature="++Sign,
	Header = get_header(BaseStr, Sign),
	URL = Url,

	%io:format("url:~s\n~s",[URL,Header]),
	case httpc:request(post,{URL,[{"Authorization",Header}],"application/x-www-form-urlencoded",[]},[],[]) of
		{ok,{_,_,R}}->
			{RS} = ejson:decode(R),
			%			io:format("~w",[RS]),
			case get_value(RS, <<"BriefUser">>) of
				false ->
					{false,"return no such user"};
				{Info} ->
					%{Info2} = ejson:decode(Info),
					case get_value(Info,<<"id">>) of
						false ->
							{false, "return info err"};
						ID->
							{true, binary:bin_to_list(ID)}
					end
			end;
		Err ->
			{false,Err}
	end.

get_header(BaseStr,Sign)->
	[_H|L]=lists:foldl(fun({A,B},Acc)->
							  Acc++","++A++"="++"\""++B++"\""
					  end, [], BaseStr),
	"OAuth "++L++",oauth_signature="++"\""++Sign++"\"".%++","++L.

uri_encode(Str)->
	[_,_,_|L]=lists:foldl(fun({A,B},Acc)->
						  Acc++"%26"++A++"%3D"++B
						  end, "", Str),
	L.

merge_baseString_info(SessionID)->
	Consumer_key = {"oauth_consumer_key","Evpq8yxeyzsOswWg8cg8s8sC0"},
	Nonce = {"oauth_nonce",integer_to_list(random:uniform(1000))},
	Method = {"oauth_signature_method","HMAC_SHA1"},
	TimeStamp = {"oauth_timestamp",integer_to_list(util:now())},
	Token = {"oauth_token",SessionID},
	Version = {"oauth_version","1.0"},
	lists:keysort(1, [Consumer_key, Nonce, Method, TimeStamp,Token, Version]).
%	Consumer_key++"%26"++Nonce++"%26"++
%		Mechod++"%26"++TimeStamp++"%26"++Token++"%26"++Version.

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
	[{"sessionID","1407e846dffd853f62378ee804078038"},{"oauth_token_secret","c8cb73b210924ff966a7e2f593880d89"},
                                      {"devid","50f95e79-25f2-3db2-8560-44d2be97990c"}].


