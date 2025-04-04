%% @author caijunjun
%% @doc 爱苹果sdk登录处理


-module(mod_login_iiapple).
-include("common.hrl").
-include("record.hrl").

-define(SecretKey, "ea4f873a5ddfe3765bb092f39df182ac").
-define(GameKey, "8d49be89e102ed7c7abe2442eece3553").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).


handle(Req) ->
	QueryString = Req:parse_qs(),
	case check_auth(QueryString) of
		{true, Uid, DevID, Version} ->
			case db_func:get_iiapple_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_IIAPPLE, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_IIAPPLE),
                    Reply = ejson:encode({[
                                            {<<"result">>,0},
                                            {<<"accid">>,Accid},
                                            {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                            {<<"valid_time">>,list_to_binary(ValidTime)},
                                            {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                            {<<"server_list">>,ServerList}
                                        ]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
		false ->
			Reply = ejson:encode({[{<<"result">>,2}]}),
			platform_tool:return(Req, Reply)
	end,
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(QS) ->
	UserID  = proplists:get_value("uin", QS),
	Session = proplists:get_value("sessionID", QS),
	DevID = proplists:get_value("devid", QS),
    Version = proplists:get_value("version", QS),
	case (not lists:member(?undefined, [UserID,Session,DevID,Version])) of
        false ->
            ?ERR("iiapple login, query string error, QueryString = ~p~n", [QS]),
			false;
		true ->
			Sign = make_sign(["game_id=", ?GameKey, "&session=", Session, "&user_id=", UserID]),
			URL = "http://ucenter.iiapple.com/foreign/oauth/verification.php?user_id=" ++ UserID ++ "&session="++Session 
					++ "&game_id=" ++ ?GameKey ++ "&_sign=" ++ Sign,
			case httpc:request(get, {URL,[]}, [], []) of
                {ok,{_,_,Ret}} ->
                    case unpack_response(Ret) of
                        true ->
                            {true, UserID, DevID, Version};
                        _ ->
                            ?ERR("iiapple login, check ret error,Ret = ~p~n", [Ret]),
							false
                    end;
                _ ->
                    ?ERR("iiapple login, url request error, URL = ~p~n", [URL]),
					false
            end
	end.


unpack_response(Ret) ->
    {Ret2} = try ejson:decode(Ret) catch
                 _:_ -> {[]}
             end,
	
    Status = proplists:get_value(<<"status">>, Ret2),
    Desc = proplists:get_value(<<"desc">>, Ret2),
    case (not lists:member(?undefined, [Status,Desc])) of
        false ->
            false;
        true ->
            if 
				Status =:= 1 -> true;
				true		 -> ?ERR("iiapple login, url request error, reason = ~p~n", [Status])
			end
    end.

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").

make_sign(SignList) ->
	md5(sign(SignList) ++ ?SecretKey).

sign(StrList) ->
	md5(sign2(StrList)).

sign2([A]) ->
	A;
sign2([A,B]) ->
	A ++ B;
sign2([]) ->
	"";
sign2([A|T]) ->
	A ++ sign2(T).
		
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


