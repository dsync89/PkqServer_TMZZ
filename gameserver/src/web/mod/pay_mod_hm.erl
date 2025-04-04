
-module(pay_mod_hm).
-export([pay_gold/1]).
-include("common.hrl").

-define(PlatPKey, <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDR6LLuqYsNuV5jUItJHFDuZMftLy7uytpoze3wwrElafRjbwnEnEPjA/CevNRz6KFQjTTAGqGv5ny5GbuklOHlQRFR/KkDizT/qqQlqvp60fuyKpfDTOCryztu3l9c2KCA54BvwTLpt0WFMFQr1SBzCkFW49G7sU4tIunDPqHFiQIDAQAB\n-----END PUBLIC KEY-----">>).


%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    Sign = proplists:get_value("sign", QueryString),
    TransData = proplists:get_value("transdata", QueryString),
    case verify_sign(TransData,Sign) of
        true ->
			{DataList} = ejson:decode(TransData),
			RoleID = erlang:binary_to_list(proplists:get_value(<<"cpprivate">>,DataList)),
			RoleID2 = erlang:list_to_integer(RoleID),
			RoleAccID = db_sql:get_role_accid(RoleID2),
			Req:ok({"text/html; charset=utf-8", ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]})}),
			QS = mochiweb_util:urlencode(QueryString),
			Amount = trunc(proplists:get_value(<<"money">>,DataList) * 10),
			pay_gold2(RoleID2,Amount,QS,util:md5(TransData),?ACCOUNT_TYPE_HM);
		false ->
			Req:ok({"text/html; charset=utf-8", ejson:encode({[{<<"result">>,0}]})}),
			?ERR("pay hm pay failed, QueryString:~w",[QueryString])
	end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_hm(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

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
%% 
%% get_value(Response, Key) when is_binary(Key)->
%%     case lists:keyfind(Key, 1, Response) of
%%         false ->
%%             false;
%%         {Key, Value} ->
%%             Value
%%     end.
%% 
%% get_value_byte(Num)->
%%     get_value_byte(Num,"").
%% get_value_byte(0,S)->
%%     lists:flatten(S);
%% get_value_byte(In, S)->
%%     P = In rem 256,
%%     P1 = io_lib:format("~c", [P]),
%%     get_value_byte(erlang:trunc(In div 256), P1++S).


