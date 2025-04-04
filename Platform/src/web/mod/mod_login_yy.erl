%% @author caijunjun
%% @doc YY 登录.


-module(mod_login_yy).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-define(AppKey, "0cdd28d4353d4f5c9caf79fcc0e9324f").
-define(AppID,  "MKDYG").

%% -define(ACCESSKEY, "ADX981P71IES").
%% -define(SECRETKEY, "617b465b91b1c6eea5a808fb874cb446").

handle(Req) ->
    QueryString = Req:parse_qs(),
    case check_auth(QueryString) of
        true -> 
            DevID = proplists:get_value("devid", QueryString), 
            Uid = proplists:get_value("account", QueryString), 
            case db_func:get_yy_accid(Uid,DevID,util:get_ip_from_req(Req)) of
                [] ->
                    Reply = ejson:encode({[{<<"result">>,1}]}),
                    platform_tool:return(Req, Reply);
                [Accid] ->
                    Version = proplists:get_value("version", QueryString),
                    {DelServerIDList, ServerList} = tk_config:get_server_list(?ACCOUNT_TYPE_YY, Version),
                    ValidTime = integer_to_list(util:now()+ ?KEY_VALID_INTERVAL),
                    ServerLoginKey = util:md5(integer_to_list(Accid) ++ ValidTime ++ ?TICKET),
                    LatestSLInfo = latestSL(lists:filter(fun(Elem) -> not lists:member(Elem, DelServerIDList) end, db_func:get_login_hist(Accid))),
                    
                    pm_platform_server:add_loglogin(Accid, ?ACCOUNT_TYPE_YY),
                    Reply = ejson:encode({[{<<"result">>,0},
                                           {<<"accid">>,Accid},
                                           {<<"login_ticket">>,list_to_binary(ServerLoginKey)},
                                           {<<"valid_time">>,list_to_binary(ValidTime)},
                                           {<<"login_history">>,list_to_binary(LatestSLInfo)},
                                           {<<"server_list">>,ServerList}]}),
                    platform_tool:return(Req, Reply)
            end;
        false ->
            ?ERR("yy login check failed"),
            Reply = ejson:encode({[{<<"result">>,2}]}),
            platform_tool:return(Req, Reply)
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


check_auth(QS) ->
    Sid = proplists:get_value("sid", QS),
    Account = proplists:get_value("account", QS),
    Time = proplists:get_value("time", QS),
    
    string:to_upper(util:md5(?AppKey ++ ?AppID ++ Account ++ Time)) =:= Sid.

latestSL(LatestSL) ->
    string:join([integer_to_list(E)||E<-LatestSL],",").

