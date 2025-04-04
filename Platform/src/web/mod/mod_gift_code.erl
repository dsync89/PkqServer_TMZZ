%% @author admin
%% @doc 礼品码生成接口
%% Created 2013-7-5


-module(mod_gift_code).
-include("common.hrl").
-include("record.hrl").
-compile(export_all).
%% API functions

%% Internal functions
-export([]).



%% ====================================================================
%% API functions
%% ====================================================================


%% @doc 获取角色基本信息
handle(Req) ->
    QueryString = Req:parse_qs(),
	%io:format("QS=~p\n",[QueryString]),
	?DEBUG("debug mod_gift_code create, QueryList = ~p~n", [QueryString]),
	GiftType = proplists:get_value("type", QueryString),
	NumCode = proplists:get_value("num", QueryString),
	
%% 	io:format("ServerKey:~s\n",[ServerKey]),
	GiftCodeNum = list_to_integer(NumCode),
	gift_code:gen(GiftType,GiftCodeNum),
	
	reply_ok(Req, {true,1}).




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
	

