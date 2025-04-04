-module(mod_pay_hm).
-include("common.hrl").
-include("record.hrl").

-include_lib("public_key/include/public_key.hrl"). 

-define(PlatPKey, <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDR6LLuqYsNuV5jUItJHFDuZMftLy7uytpoze3wwrElafRjbwnEnEPjA/CevNRz6KFQjTTAGqGv5ny5GbuklOHlQRFR/KkDizT/qqQlqvp60fuyKpfDTOCryztu3l9c2KCA54BvwTLpt0WFMFQr1SBzCkFW49G7sU4tIunDPqHFiQIDAQAB\n-----END PUBLIC KEY-----">>).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req)->
    QueryString = Req:parse_post(),
    Sign = proplists:get_value("sign", QueryString),
    TransData = proplists:get_value("transdata", QueryString),
	case verify_sign(TransData, Sign) of
		true ->
			{DataList} = ejson:decode(TransData),
			Amount = trunc(proplists:get_value(<<"money">>,DataList) * 10),
			RoleID = erlang:binary_to_list(proplists:get_value(<<"cpprivate">>,DataList)),
			RoleID2 = erlang:list_to_integer(RoleID),
			ServerID = (RoleID2 div 1000000) - 1,
			Server = data_server_list:get(ServerID),            
			HttpPort = integer_to_list(Server#server.serverHttpPort),
			SendQS = mochiweb_util:urlencode(QueryString),
			Url = "http://"++Server#server.serverIP++":"++HttpPort++"/payhm?" ++ SendQS,
			Response = httpc:request(get, {Url, []}, [], []),
			case Response of
				{ok, {_,_,Content}} ->
					{Content2} = ejson:decode(Content),
					Result = get_value(Content2, <<"result">>),
					if Result == 1 ->
						   AccID = get_value(Content2, <<"accid">>),
						   db_func:log_pay_info(1, RoleID2, AccID, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_HM, QueryString),
						   Reply = "SUCCESS",
						   Req:ok({"text/html; charset=utf-8", Reply});
					   true ->
						   db_func:log_pay_info(3, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_HM, QueryString),
						   Reply = "FAILURE",
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   ?ERR("hm check_order failed. reason:game server return false,order:~w, result ~p",[QueryString, Result])
					end;
				Error ->
					db_func:log_pay_info(4, RoleID2, 0, Amount, erlang:localtime(), 1, ?ACCOUNT_TYPE_HM, QueryString),
					Reply = "FAILURE", %% 订单未正确获取,需要重新获取订单
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("hm check_order failed. reason:game Server not response not 0,order:~w, ~p",[QueryString, Error])
			end;
		_ ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_HM, QueryString),
			Reply = "FAILED",
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("hm check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_pubKey()->
	PubKey = ?PlatPKey,
	PemEntries = public_key:pem_decode(PubKey),
	public_key:pem_entry_decode(hd(PemEntries)).

verify_sign(Data, Base64Sign)->
	RSAPubKey = get_pubKey(),
	Sign = base64:decode(Base64Sign),
	public_key:verify(to_binary(Data), 'md5', Sign, RSAPubKey).

to_binary(Data) when is_binary(Data) ->
	Data;
to_binary(Data) when is_list(Data) ->
	erlang:list_to_binary(Data).
	

%% get_rsa_key() ->
%%     Key = "OTRCMUI4RjIzNEYxN0U5M0FGRUU4RTY1QjM2Qzc2RTJGRDk5ODYyM01UTTVOekUwTnpZd056RXpOREEyTnpNM01Ua3JNVGN6TkRnME16YzVNRFUyTlRBeU1UY3lOemMwT1RZd01EZzRNVEl5TXpJMU56QTVOVGc1",
%%     Key1 = base64:decode_to_string(Key),
%%     Key2 = lists:sublist(Key1, 41, erlang:length(Key1) - 40),
%%     Key3 = base64:decode_to_string(Key2),
%%     [PrivateKey, ModKey] = string:tokens(Key3, "+"),
%%     {ok, erlang:list_to_integer(PrivateKey), erlang:list_to_integer(ModKey)}.
%% 
%% check_sign(TransData,Sign)->
%%     {ok, P_key, M_key} = get_rsa_key(),
%%     Sign2 = string:tokens(Sign, " "),
%%     Md5Sign = lists:foldl(fun(H,Acc) ->
%%                                   I = crypto:mod_exp(erlang:list_to_integer(H,16),P_key, M_key),
%%                                   Acc++get_value_byte(I)
%%                                   end,[],Sign2),
%%     Md5Sign2 = http_uri:decode(string:strip(Md5Sign)),
%%     Md5Data = util:md5(TransData),
%%     Md5Data == Md5Sign2.
%% 
%% 
get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.
%% 
%% get_value_byte(Num)->
%%     get_value_byte(Num,"").
%% get_value_byte(0,S)->
%%     lists:flatten(S);
%% get_value_byte(In, S)->
%%     P = In rem 256,
%%     P1 = io_lib:format("~c", [P]),
%%     get_value_byte(erlang:trunc(In div 256), P1++S).
    
%% test()->
%%     TransData = "{\"exorderno\":\"1.2010082.1417686186\",\"transid\":\"04114120417431090987\",\"waresid\":6,\"appid\":\"300148696\",\"feetype\":2,\"money\":10,\"result\":0,\"transtype\":0,\"transtime\":\"2014-12-04 17:43:36\",\"count\":1,\"cpprivate\":\"1.2010082.1417686186\",\"paytype\":501}",
%%     Sign = "16a5977714436d72eb68c829f68464b6 5712c1ac8ce009f06b4b336fb88c44ec 88fef52b40cf0555da43523c129cfb67 ",
%%     check_sign(TransData,Sign).

