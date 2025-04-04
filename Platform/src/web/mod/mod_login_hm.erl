-module(mod_login_hm).
-include("common.hrl").
-include("record.hrl").

-define(AppID, '3001738887').
-define(AppVKey,  <<"-----BEGIN RSA PRIVATE KEY-----\nMIICXAIBAAKBgQCH09Ih29XaRtjGqxZKeuTQ0ovrkI3YOWAL1wAx5OWgFHjhCAC6eda+OimJIKdrFCPnCScOsCfFtaDNvGaLUPH84EFhX5A9vrtZH4bmW2U5Ed2guzyo4/9iDW5ctVUMtoKcJ+ihxZA7XCvnFZtjkHC6h9a5Ryw9YImF08PUtYAmOwIDAQABAoGAfLGcVpMgB2xE9FX6d9PMnly8MT0ayycZIqV1u1hc6hIBSu1y5b6WwzpYCVid+fKaZy6C2bwkNyBgCJ/uL/XWwK6MwTfN+xyLnCapdVphKH3wIBU8qYeyHzINdyR2RhU7i1xBjFjqR924dKn9IjYFeoGLD+bkIz1OIkHsHD2htLECQQC/ZmOzAaYcThJ0yZ9J5wCWAen3d6pai1KYXTgDS9CcoV819mc1rBO7XOGra3JjAjfmNDDBmcjHXBYjUrO94siTAkEAtavGSOelFQowDPeHLKEJCmFEyRFqGdatnRYbqZeq3CVomhX3yF1d7yzd9fsT2tKmDcswBd80urwchGWupnJ8uQJBAJFkvielAQJPbEW7Q31oNVG+cnTCOm49l6iWZ45ZcUcKbsEXRJ/I79RMYkFNhb8gWEZPLxpiL9sBYO/ESzXbqHECQFLWN3you1pjodNXzrgn2ZJ1FcNFAIc1n0ZgfU6QTKVDHXYdDDW9dQaJA9CUotIFeRgTDRuwUj/PZIVuQ/u7emkCQCgECzrrkBJjHoD16ROe8BsrU3ZDrWhpFTkiXS1AikLYbp9d7HRtEt3BnUyf8TMthB4J8Dm9tUaNef34DFX6M/I=\n-----END RSA PRIVATE KEY-----">>).

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
%%     UID = proplists:get_value("logintoken", QueryString),
	case check_auth(QueryString) of
		{true, DevID, Version, UID} ->
			case db_func:get_hm_accid(UID, DevID,util:get_ip_from_req(Req)) of
				[] ->
					Reply = ejson:encode({[{<<"result">>,1}]}),
					platform_tool:return(Req, Reply);
				[Accid] ->
					{DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_HM, Version),
					ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
					ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
					LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
					pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_HM),
					Reply = ejson:encode({[{<<"result">>,0},
										   {<<"accid">>,Accid},
										   {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
										   {<<"valid_time">>,list_to_binary(ValidTime)},
										   {<<"login_history">>,list_to_binary(LatestSLInfo)},
										   {<<"server_list">>,ServerList}]}),
					platform_tool:return(Req, Reply)
			end;
		false ->
			Reply = ejson:encode({[{<<"result">>,2}]}),
			platform_tool:return(Req, Reply)
	end.

check_auth(QS) ->
	DevID = proplists:get_value("devid", QS),
%% 	UID = proplists:get_value("uin", QS),
	Version = proplists:get_value("version", QS),
	LoginToken = proplists:get_value("logintoken", QS),
	TransData = ejson:encode({[{<<"appid">>, ?AppID}, {<<"logintoken">>, erlang:list_to_atom(LoginToken)}]}),

	Sign = decode_data(TransData),
	case check_auth2(TransData, Sign) of
		{true, UID} ->
			{true, DevID, Version, UID};
		false ->
			false
	end.
		
check_auth2(TransData, Sign) ->
	URL = "http://ipay.iapppay.com:9999/payapi/tokencheck",
	Message = mochiweb_util:urlencode([{transdata, TransData}, {sign,Sign}, {signtype, "RSA"}]),
	
	Response = httpc:request(post, {URL,[], "application/x-www-form-urlencoded",Message}, [], []),
	
	case Response of
		{ok,{_,_,Ret}} ->
			case unpack_response(Ret) of
				{true, UserID} ->
					{true, UserID};
				_ ->
					?ERR("iiapple login, check ret error,Ret = ~p~n", [Ret]),
					false
			end;
		_ ->
			?ERR("iiapple login, url request error, URL = ~p~n", [URL]),
			false
	end.


get_private_key() ->
	PubKey = ?AppVKey,
	PemEntries = public_key:pem_decode(PubKey),
	public_key:pem_entry_decode(hd(PemEntries)).

decode_data(Data) ->
	PrivateKey = get_private_key(),
	Sign = public_key:sign(Data, md5, PrivateKey),
	base64:encode(Sign).


unpack_response(Ret) ->
    {Ret2} = try 
				 RetJson = mochiweb_util:parse_qs(Ret),
				 TransData = proplists:get_value("transdata", RetJson),
				 ejson:decode(TransData) 
			 catch
                 _:_ -> {[]}
             end,
    UID = proplists:get_value(<<"userid">>, Ret2),
    LoginName = proplists:get_value(<<"loginname">>, Ret2),
    case (not lists:member(?undefined, [LoginName,UID])) of
        false ->
			?ERR("hm login error code ~p, ~ts~n", [proplists:get_value(<<"code">>, Ret2), proplists:get_value(<<"errmsg">>, Ret2)]),
            {false,Ret2};
        true ->
            {true,erlang:binary_to_list(UID)}
    end.

latestSL(LatestSL) ->
    string:join([integer_to_list(E)||E<-LatestSL],",").



