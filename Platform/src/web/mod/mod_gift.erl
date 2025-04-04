%% @author admin
%% @doc 礼品码接口
%% Created 2013-7-5


-module(mod_gift).
-include("common.hrl").
-include("record.hrl").
-compile(export_all).
%% API functions

%% Internal functions
-export([]).



%% ====================================================================
%% API functions
%% ====================================================================

check_gift(ServerID, ServerKey, GiftCode, Accid) ->
    case data_server_list:get(ServerID) of
        #server{serverKey=ServerKey} ->
            case gift_code:draw_gift(GiftCode, Accid, ServerID) of
                {true, Reward} ->
                    {true, Reward};
                {false, Reason} ->
                    {false, Reason}
            end;
        _ ->
            {false, 6}
    end.

%% @doc 获取角色基本信息
handle(Req) ->
    QueryString = Req:parse_post(),
	%io:format("QS=~p\n",[QueryString]),
	AccountIDStr = proplists:get_value("accountID", QueryString),
	GiftCode = proplists:get_value("giftcode", QueryString),
	ServerIDStr = proplists:get_value("serverID", QueryString),
	ServerKey = proplists:get_value("serverKey", QueryString),
%% 	io:format("ServerKey:~s\n",[ServerKey]),
	ServerID = list_to_integer(ServerIDStr),
	Accid = list_to_integer(AccountIDStr),
	case data_server_list:get(ServerID) of
		#server{serverKey=ServerKey} ->
			case gift_code:draw_gift(GiftCode, Accid rem ?AccidBase , ServerID) of
				{true, Reward} ->
					reply_ok(Req, Reward); 
				{false, Reason} ->
					reply(Req, Reason)
			end;
		_ ->
			reply(Req, 6)
	end.



%% ====================================================================
%% Internal functions
%% ====================================================================

reply(Req, Result) ->
	Reply = ejson:encode({[{<<"result">>,Result}]}),
	platform_tool:return(Req, Reply).

reply_ok(Req, Reward) ->
	Reply = ejson:encode({[{<<"result">>,1}, {<<"reward">>, base64:encode(term_to_binary(Reward))}]}),
	platform_tool:return(Req, Reply).


test() ->
	inets:start(),
	Arg = lists:flatten(io_lib:format("accountID=~s&giftcode=~s&serverID=~s&serverKey=~s", 
									  ["1","A210RTN9IV","2", "iwQ3QJvT3vA1RFZwAXexwJy7UzyGhqaU"])),
	io:format("~s\n",[Arg]),
		httpc:request(post, {"http://localhost:12380/gift",
							  [], "application/x-www-form-urlencoded",Arg}, [], []).
	

