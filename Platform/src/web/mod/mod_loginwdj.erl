%% @author admin
%% @doc @todo Add description to mod_loginwdj.


-module(mod_loginwdj).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl").

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
	%% ?ERR("Req:~w, QS:~p",[Req,QueryString]),
	Uid = proplists:get_value("uid", QueryString),
    DevID = proplists:get_value("devid", QueryString),
	Token = proplists:get_value("token", QueryString),
	if Uid =:= ?undefined orelse Token =:= ?undefined orelse DevID =:= ?undefined ->
		   Reply = ejson:encode({[{<<"result">>,3}]}),
		   platform_tool:return(Req, Reply);
	   true ->
		   case check_auth(Uid, Token) of
			   true ->
				   case db_func:get_wdj_accid(Uid, DevID,util:get_ip_from_req(Req)) of
					   [] ->
						   Reply = ejson:encode({[{<<"result">>,1}]}),
						   platform_tool:return(Req, Reply);
					   [Accid] ->
                          Version = proplists:get_value("version", QueryString),
                          {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_WDJ, Version),
						   ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
						   ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
						   LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                           pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_WDJ),
						   %% io:format("~w",[LatestSLInfo]),
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

latestSL(LatestSL) ->
	string:join([integer_to_list(E)||E<-LatestSL],",").

check_auth(Uid, Token) ->
	AppID = "100018839",
	Token3 = encode(Token),
	Token2 = mochiweb_util:urlencode([{uid,Uid},{token,Token3},{appkey_id,AppID}]),
	URL = "https://pay.wandoujia.com/api/uid/check?"++Token2,
	%% ?ERR("URL:~w",[URL]),
	case httpc:request(get, {URL, []}, [], []) of
		{ok,{_,_,R}} ->
			case R of
				"true" ->
					true;
				_ ->
					?ERR("wdj check_auth request failed."),
					false
			end;
		Err ->
			?ERR("wdj check_auth request err:~w",[Err]),
			false
	end.


encode(Token) ->
	lists:foldr(fun(E,Acc)->
						case E of
							$\s ->
								[$+ |Acc];
							Other ->
								[Other|Acc]
						end 
					end,[], Token).
	
