%% @author admin
%% @doc @todo Add description to mod_check_receipt.


-module(mod_check_receipt).
-include("common.hrl").
-include("record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(Req) ->
	%% [{QS,_}] = Req:parse_post(),
	QS = Req:recv_body(),
	?ERR("QS:~p~n",[QS]),
	{QueryString} = ejson:decode(QS),
	%% ?ERR("QueryString:~w~n Req:~p~n",[QueryString,Req]),
	ReceiptBin = proplists:get_value(<<"receipt">>, QueryString),
	Receipt = binary_to_list(ReceiptBin), 
	[Receipt2] = string:tokens(Receipt,"\" "),
	%% ?ERR("Receipt2:~p~n",[Receipt2]),
	Receipt3Bin = base64:decode(Receipt2),
	Receipt3 = binary_to_list(Receipt3Bin),
	%% ?ERR("Receipt3:~p~n",[Receipt3]),
	[Receipt4] = string:tokens(Receipt3,"\" "),
	%% ?ERR("Receipt4:~p~n",[Receipt4]),
	ReceiptFinal = base64:decode_to_string(Receipt4),
	%% ?ERR("ReceiptFinal:~p~n",[ReceiptFinal]),
	Md5Bin = proplists:get_value(<<"md5">>,QueryString),
	Md5 = binary_to_list(Md5Bin), 
	RoleIDBin = proplists:get_value(<<"roleid">>,QueryString),
	RoleID = binary_to_list(RoleIDBin), 
	RoleID2 = list_to_integer(RoleID),
	Amount = proplists:get_value(<<"amount">>, QueryString),
	Accid = proplists:get_value(<<"accid">>, QueryString),
	?ERR("receipt:~p~n md5:~p~n roleid:~p~n Amount:~p~n, Accid:~p~n",[ReceiptFinal,Md5,RoleID,Amount,Accid]),
	case checkReceiptMd5(ReceiptFinal,Md5,RoleID2,1,Amount,Accid) of
		true ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply});
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.
			


%% ====================================================================
%% Internal functions
%% ====================================================================

checkReceiptMd5(Receipt,Md5,RoleID,SrcType,Amount,Accid) ->
	case db_func:check_pay_receipt_duplicate(Md5) of
		true ->
			db_func:add_pay_receipt(Md5,RoleID,Receipt,SrcType,Amount,Accid),
			true;
		false ->
			false
	end.

