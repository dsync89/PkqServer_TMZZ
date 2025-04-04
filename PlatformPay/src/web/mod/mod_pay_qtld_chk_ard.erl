%% @author liuqiang
%% @doc 奇天乐地ARD 充值验证用户

-module(mod_pay_qtld_chk_ard).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(GID, "2036").
-define(APPKEY, "af31c25c0a8608a9b7f4d0ea8c0fdfcc").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    QueryList = Req:parse_post(),
%    ?ERR("mod_pay_qtld_chk_ard, QueryList = ~p~n", [QueryList]),
    Sid = proplists:get_value("sid", QueryList),
    Uid = proplists:get_value("uid", QueryList),
    case check_suid(Sid, Uid) of
        true ->
            Reply = "1",
            Req:ok({"text/html; charset=utf-8",Reply});
        _ ->
            Reply = "0",
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_suid(Sid, Uid) ->
    Sid2 = erlang:list_to_integer(Sid),
    Uid2 = "qtld_ard_" ++ Uid,
    case db_func:get_account_info_ets(Uid2) of
        [Accid, _Passwd] -> ask_for_gameserver(Sid2, Accid);
        _ ->
            Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w;", [Uid2, ?ACCOUNT_TYPE_QTLD_ARD]),
            case db_func:get_row(Sql) of
                [Accid] -> ask_for_gameserver(Sid2, Accid);
                _ -> false
            end
    end.

ask_for_gameserver(Sid, Accid) ->
try
    Server = data_server_list:get(Sid),
    HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
    SendQS = mochiweb_util:urlencode([{sid,Sid},{accid,Accid}]),
    URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_qtld_chk_ard?"++SendQS,
    case httpc:request(get, {URL,[]},[],[]) of
        {ok,{_,_,"1"}} -> true;
        _ -> false
    end
catch _:_ -> false end.
