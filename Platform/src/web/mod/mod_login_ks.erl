%% @author liuqiang
%% @doc 金山sdk登录处理


-module(mod_login_ks).

-include("common.hrl").

-export([handle/1]).

-define(GID, "104").
-define(SUPPLIERID, "200104").
-define(SUPPLIERKEY, "zk38wq6o4u310z").


%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_auth(Req) of
        false ->
            Reply = ejson:encode({[{<<"result">>,2}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,Uid,DevID,Version} ->
%%             ?ERR("debug login ks, Uid = ~p~n", [Uid]),
            case db_func:get_ks_accid(Uid, DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                [Accid] ->
                    {DelServerIDList,ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_KS, Version),
                    ValidTime = integer_to_list(util:now() + ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid)++ValidTime++?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_KS),
                    Reply = ejson:encode({[
                                            {<<"result">>,0},
                                            {<<"accid">>,Accid},
                                            {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                            {<<"valid_time">>,list_to_binary(ValidTime)},
                                            {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                            {<<"server_list">>,ServerList}
                                        ]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(Req) ->
    QueryString = Req:parse_qs(),
%%     ?ERR("debug login ks, QueryString = ~p~n", [QueryString]),
    Mutk = proplists:get_value("sessionID", QueryString),
%%     ?ERR("debug login ks, Mutk = ~p~n", [Mutk]),
    {ok,{IPAddr,_}} = inet:peername(mochiweb_request:get(socket, Req)),
    ClientIP = inet_parse:ntoa(IPAddr),
%%     ?ERR("debug login ks, ClientIP = ~p~n", [ClientIP]),
    DevID = proplists:get_value("devid", QueryString),
    Version = proplists:get_value("version", QueryString),
    case (not lists:member(?undefined, [Mutk,ClientIP,DevID,Version])) of
        false ->
            ?ERR("ks login, query string error, QueryString = ~p~n", [QueryString]), false;
        true ->
            {OriStr,Sign} = sign(Mutk, ClientIP),
            URL = "http://m.wan.liebao.cn/user/validate_mutk?"++OriStr++"&sign="++Sign,
%%             ?ERR("debug login ks, URL = ~p~n", [URL]),
            case httpc:request(get, {URL,[]}, [], []) of
                {ok,{_,_,Ret}} ->
                    {Ret2} = try ejson:decode(Ret) catch
                                 _:_ -> {json_false}
                             end,
%%                     ?ERR("debug login ks, Ret2 = ~p~n", [Ret2]),
                    case Ret2 of
                        [{<<"data">>,{[{<<"uid">>,BUid}]}},{<<"msg">>,_},{<<"code">>,1}] ->
                            {true,binary_to_list(BUid),DevID,Version};
                        _ ->
                            ?ERR("ks login, check ret error,Ret = ~p~n", [Ret2]), false
                    end;
                _ ->
                    ?ERR("ks login, url request error, URL = ~p~n", [URL]), false
            end
    end.

sign(Mutk, ClientIP) ->
    OriStr = mochiweb_util:urlencode([{client_ip,ClientIP},{mutk,Mutk},{supplier_id,?SUPPLIERID},{time,util:now()}]),
    {OriStr,md5(OriStr ++ ?SUPPLIERKEY)}.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- erlang:binary_to_list(erlang:md5(S))]).

latestSL(LatestSL) ->
    string:join([erlang:integer_to_list(E) || E <- LatestSL], ",").



%% ----------------------------test code----------------------------
test() ->
    Mutk = "ki3cbjat6dg9slt1u8u6n2jbj4",
    ClientIP = "125.71.211.184",
    {OriStr,Sign} = sign(Mutk, ClientIP),
    "http://m.wan.liebao.cn/user/validate_mutk?"++OriStr++"&sign="++Sign.
