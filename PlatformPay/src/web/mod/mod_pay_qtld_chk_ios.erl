%% @author liuqiang
%% @doc 奇天乐地IOS 充值验证用户

-module(mod_pay_qtld_chk_ios).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

-define(GID, "2036").
-define(APPKEY, "af31c25c").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    QueryList = Req:parse_post(),
%    ?ERR("mod_pay_qtld_chk_ios, QueryList = ~p~n", [QueryList]),
    Uid = proplists:get_value("uid", QueryList),
    Sid = proplists:get_value("sid", QueryList),
    case check_uid(Uid) and check_sid(Sid) of
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

check_uid(Uid) ->
    Uid2 = "qtld_ios_" ++ Uid,
    case db_func:get_account_info_ets(Uid2) of
        [_, _] -> true;
        _ ->
            Sql = io_lib:format("select accountID from account where accountName='~s' and type=~w;", [Uid2, ?ACCOUNT_TYPE_QTLD_IOS]),
            case db_func:get_row(Sql) of
                [_] -> true;
                _ -> false
            end
    end.

check_sid(Sid) ->
    case data_server_list:get(erlang:list_to_integer(Sid)) of
        ?undefined -> false;
        _ -> true
    end.
