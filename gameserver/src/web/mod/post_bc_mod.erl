%% @author admin
%% @doc 接收http协议来的广播消息，认证后对广播消息进行全服务器广播
%% @notice 使用的验证方式码为MD5(密码+message),密码在setting.config中配置

-module(post_bc_mod).
-export([post_bc/1]).

-include("common.hrl").

post_bc(Req)	->
	case parse_req(Req) of
		{ok, Message}	->
			refresh(Message),
			?INFO("broadcast message ~p", [Message]),
			Reply =  ejson:encode({[{<<"result">>,<<"succ">>}]});
		_	->
			Reply = ejson:encode({[{<<"result">>,<<"fail">>}]})
	end,
	Req:ok({"text/html; charset=utf-8", Reply}).

parse_req(Req)	->
	QueryString = Req:parse_post(),
	%% 提取消息中的信息
	Pass = proplists:get_value("pass", QueryString),
	Message = proplists:get_value("message", QueryString),
	{can_pass(Pass, Message),Message}.

can_pass(Pass, Message)	->
	%% 认证消息是否有效,认证失败,则消息不进行公告广播
	{Auth, _, _} = data_setting:get(passinfo),
	Auth2 = util:md5(Auth ++ Message),
	if Pass =:= Auth2 -> ok;
		true -> false
	end.

refresh(Message)	->
	%% 将收到的消息数据写入到config文件中,然后重新加载config文件
	dump_message_config(Message),
	user_default:lc(data_notice).

dump_message_config(Message)	->
	EchoMsg=lists:flatten(tuple_to_list(ejson:decode(Message))),
	%% 先清空文件,然后以append方式将数据写进文件
	file:write_file("config/data_notice.config","%%公告,配置说明，每次修改或者增加一条公告时，请将公告ID 置为  当前最大ID+1\r\n%%第一条为置顶公告，第二条为滚动公告\r\n"),
	%% 消息中可能只有一条广播, 也可能有两条广播, 含有多条广播时只取前两条
	lists:foreach(fun(X)->{A,B}=X, 
				Message2 = 
					"{notice, " ++ erlang:integer_to_list(
								lists:max(data_notice:get_list())
									+erlang:list_to_integer(lists:flatten(binary:bin_to_list(A))))
							 	++ ", \"\",\"" ++ lists:flatten(binary:bin_to_list(B)) ++"\"}.\r\n" ,
				file:write_file(filename:join([tk_config:root_dir(),"config/data_notice.config"]), 
								Message2,[append])
				end, 
				lists:sublist(EchoMsg,1,2)).


%%  tests  ------------------------------------------------------------------------------------

web_test()	->
	inets:start(),
	Msg1=ejson:encode({[{<<"1">>,<<"<prop ff00ff00-14--1-1>活动12354
	<prop ffffffff-4>
	<prop ff00ffff-12>【活动时间】：<prop ffffff00-14>测试期间hahahapiupiu
	<prop ff00ffff-12>【活动内容】：<prop ffffffff-12>每天tkeieieie有！霸主三国~~
	">>}
	,{<<"2">>,<<"
	
	<prop ff00ff00-14--1-1>活动2：每天登陆VIP自动升级enenen啦啦啦
	<prop ffffffff-4>
	<prop ff00ffff-12>【活动时间】：<prop ffffff00-14>测试期间：折折折
	<prop ff00ffff-12>【活动内容】：<prop ffffffff-12>qqqqqll哈哈哈。
">>}
,{<<"3">>,<<"
	
	<prop ff00ff00-14--1-1>活动mmmmmmmmmmmmmmm：每天登陆VIP自动升级enenen啦啦啦
	<prop ffffffff-4>
	<prop ff00ffff-12>【活动时间】：<prop ffffff00-14>测试期间：折折折
	<prop ff00ffff-12>【活动内容】：<prop ffffffff-12>qqqqqll哈哈哈。
">>}
]}),
	Msg = http_uri:encode(binary:bin_to_list(Msg1)),
	Pass=util:md5(lists:merge("passed", binary:bin_to_list(Msg1))),
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s", 
									  [Pass,Msg])),
	io:format("~s\n",[Arg]),
		httpc:request(post, {"http://192.168.1.27:12381/broadcast",
							  [], "application/x-www-form-urlencoded",Arg}, [], []).
