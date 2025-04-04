%% @author admin
%% @doc quick SDK充值处理

-module(mod_pay_quick).
-export([handle/1]).
-include("common.hrl").
-include("record.hrl").

%%md5key
-define(APPKEY, "ebvcgz7vmdowdtlrzbvnp1fhs244xhub").

%% ====================================================================
%% API functions
%% ====================================================================

handle(Req) ->
    case check_sign(Req) of
        {true,ServerID,RoleID,Amount,QueryList,_} ->
            Server = data_server_list:get(ServerID),
            HttpPort = erlang:integer_to_list(Server#server.serverHttpPort),
            SendQS = mochiweb_util:urlencode(QueryList),
            URL = "http://"++Server#server.serverIP++":"++HttpPort++"/pay_quick?"++SendQS,
			?ERR("pay quick, game server, URL = ~p~n", [URL]),
            Response = httpc:request(get, {URL,[]},[],[]),
            case Response of
                {ok,{_,_,Content}} ->
                    {Content2} = try ejson:decode(Content) catch _:_ -> {[]} end,
                    Result = proplists:get_value(<<"result">>, Content2),
                    case Result of
                        1 ->
                            AccID = proplists:get_value(<<"accid">>, Content2),
                            db_func:log_pay_info(1, RoleID, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_QUICK, SendQS),
                            Reply = "SUCCESS",
                            Req:ok({"text/html; charset=utf-8",Reply});
                        _ ->
                            db_func:log_pay_info(3, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_QUICK, SendQS),
                            ?ERR("pay quick, game server return false, QueryList = ~p~n", [QueryList]),
                            Reply = "FAILED",
                            Req:ok({"text/html; charset=utf-8",Reply})
                    end;
                _ ->
                    db_func:log_pay_info(4, RoleID, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_QUICK, SendQS),
                    ?ERR("pay quick, game server doesn't response, URL = ~p~n", [URL]),
                    Reply = "FAILED",
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ -> void
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%app=1234567890ABCDEF&cbi=CBI123456&ct=1376578903&fee=100&pt=1376577801&sdk=09CE2B99C22E6D06
%&ssid=123456&st=1&tcd=137657AVDEDFS&uid=1234&ver=1

check_sign(Req) ->
	QueryList = Req:parse_post(),

    ?DEBUG("debug quick pay, QueryList = ~p~n", [QueryList]),
    Nt_data = proplists:get_value("nt_data", QueryList),
    Md5Sign = proplists:get_value("md5Sign", QueryList),
    Sign = proplists:get_value("sign", QueryList),
	
	Nxml = data_decode(Nt_data),
	
    case (not lists:member(?undefined, [Nt_data,Md5Sign,Sign])) of
        false ->
            ?ERR("quick pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "FAILED",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            case sign(Nt_data,Sign) =:= Md5Sign of
                false ->
                    db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_QUICK, QueryList),
                    ?ERR("quick pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "FAILED",
                    Req:ok({"text/html; charset=utf-8",Reply}), false;
                true ->
					?DEBUG("debug quick pay, singn ====true "),
					%%金额
					Bindex = string:str(Nxml,"<amount>")+8,	
					Str1 = string:substr(Nxml, Bindex),
					Eindex = string:str(Str1,"</amount>"),
					Str2 = string:substr(Str1, 1,Eindex-1 ),
					Amount2 = trunc(list_to_float(Str2)),
					%%io:format("~p~n",[Amount]),
					
					
					%%服务器id
					Bindex1 = string:str(Nxml,"<extras_params>")+15,	
					Str11 = string:substr(Nxml, Bindex1),
					Eindex1 = string:str(Str11,"</extras_params>"),
					Str21 = string:substr(Str11, 1,Eindex1-1 ),
					[SServerID,SRoleID] = string:tokens(Str21, "|"),
					%%io:format("~p~n",[SServerID]),
					%%io:format("~p~n",[SRoleID]),
                    %%[SServerID,SRoleID] = string:tokens(Cbi, "|"),
                    ServerID = erlang:list_to_integer(SServerID),
                    RoleID = erlang:list_to_integer(SRoleID),
					
					Amount = Amount2*800,
					?DEBUG("debug quick pay, servier = ~p~n", [ServerID]),
					?DEBUG("debug quick pay, RoleID = ~p~n", [RoleID]),
					?DEBUG("debug quick pay, Amount2 = ~p~n", [Amount]),
                    %%Amount2 = erlang:round(erlang:list_to_integer(Fee) * 2),
                    {true,ServerID,RoleID,Amount,QueryList,Sign}
            end
    end.

sign(Nt_data,Sign) ->
    OriStr = Nt_data++Sign++?APPKEY,
    ?DEBUG("OriStr = ~s~n", [OriStr]),
    md5(OriStr).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
	
data_decode(NData) ->
	%%S = "@108@119@174@165@158@82@167@152@171@166@161@162@159@115@90@106@94@103@83@85@154@166@156@161@155@160@166@155@118@90@139@141@118@101@110@90@82@165@165@148@167@151@153@159@160@164@157@118@82@165@160@87@116@118@117@165@162@176@165@163@168@166@169@152@157@157@169@171@147@153@150@113@117@160@157@166@164@151@159@158@110@115@166@158@153@118@109@110@102@172@161@152@119@116@162@168@151@161@164@151@160@147@158@152@119@164@169@164@162@167@169@117@95@163@160@156@158@166@152@160@152@164@157@114@117@167@171@173@143@167@168@156@151@164@144@161@168@113@116@98@160@171@172@152@159@169@149@154@167@151@167@161@117@115@167@166@157@157@168@152@158@167@116@104@99@98@97@101@105@100@108@100@98@102@109@106@101@106@100@101@102@105@106@100@111@103@107@103@117@103@165@171@148@157@168@151@160@161@111@111@169@148@177@146@165@159@165@158@110@105@97@102@105@101@106@99@100@103@109@84@106@109@112@108@99@114@103@108@110@97@161@148@178@146@172@156@158@155@118@117@145@164@160@170@163@172@119@98@101@103@105@112@104@153@163@168@165@166@170@118@110@165@165@148@173@168@171@113@97@114@103@172@164@152@165@170@168@118@117@151@175@171@170@149@172@151@166@154@162@153@163@171@112@170@169@171@117@98@157@171@165@168@153@172@143@167@146@167@150@165@172@112@115@102@165@153@172@171@151@160@149@118@114@103@165@157@170@160@168@162@166@166@144@163@157@172@163@152@152@154@115",
	Key = "72414604225870082586325224336914",
	%%NL = string:tokens(NData,"@"),
	NL = lists:map(fun(N0)-> list_to_integer(N0) end,  string:tokens(NData,"@")),
%%	NL = re:run(S,"\\d+"),
	%%NL = [108,119,174,165,158,82,167,152,171,166,161,162,159,115,90,106,94,103,83,85,154,166,156,161,151],
%%	NL = lists:flatten(lists:duplicate(3,NL0)),
	KLen = length(Key),
	NLen = length(NL),

	{Numbers,Keys} = if
				 KLen> NLen-> {H,_} = lists:split(NLen,Key) ,{NL,H};
				 KLen < NLen->
					 Count = NLen div KLen,
					 Rem   = NLen rem KLen,
					 {Tail,_} = lists:split(Rem,Key),
					 {NL,lists:flatten(lists:duplicate(Count,Key) ++  Tail)};
				 true-> {NL,Key}
		 end,


	OutList = lists:map(fun({N,K})-> <<B:8>> = <<(N - (K band 16#FF)) : 8 /unsigned>> ,B end,lists:zip(Numbers,Keys)),
	OutString = binary_to_list(iolist_to_binary(OutList)),
	?DEBUG("debug quick pay, OutString = ~p~n", [OutString]),
	OutString.
	


