%% @author admin
%% @doc client version check


-module(mod_version_check).
-include("common.hrl").
-include("record.hrl").
%% API functions
-export([handle/1]).
%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 创建角色逻辑
handle(Req) ->
    QueryString = Req:parse_post(),
    Version = proplists:get_value("version", QueryString),
    case check_client_version(Version) of
    	true ->
    		Reply = ejson:encode({[{<<"result">>,1}]}),
			platform_tool:return(Req, Reply);
		_ ->
			Reply = ejson:encode({[{<<"result">>,2}]}),
			platform_tool:return(Req, Reply)
	end.
	
test()->
	Version="0.9.3",
	Arg = lists:flatten(io_lib:format("version=~s", 
									  [Version])),
	io:format("~s\n",[Arg]),
		httpc:request(post, {"http://192.168.1.27:12380/version",
							  [], "application/x-www-form-urlencoded",Arg}, [], []).

test2()->
	Version="1.1.0",
	Arg = lists:flatten(io_lib:format("version=~s", 
									  [Version])),
	io:format("~s\n",[Arg]),
		httpc:request(post, {"http://192.168.1.27:12380/version",
							  [], "application/x-www-form-urlencoded",Arg}, [], []).

test3()->
	Version="1.2.3",
	Arg = lists:flatten(io_lib:format("version=~s", 
									  [Version])),
	io:format("~s\n",[Arg]),
		httpc:request(post, {"http://192.168.1.27:12380/version",
							  [], "application/x-www-form-urlencoded",Arg}, [], []).
test4()->
	Version="1.1.0",
	Arg = lists:flatten(io_lib:format("version=~s", 
									  [Version])),
	io:format("~s\n",[Arg]),
		httpc:request(post, {"http://192.168.1.27:12380/version",
							  [], "application/x-www-form-urlencoded",Arg}, [], []).
%% ====================================================================
%% Internal functions
%% ====================================================================

check_client_version(Version)->
	{M,S,L} = data_common:get(client_version),
%% 	P = re:split(Version, "[.]",[{return,list}]),
%% 	io:format("ll,~s,~w",[P,P]),
	[CMT,CST,CLT|_] = re:split(Version, "[.]",[{return,list}]),
	CM = erlang:list_to_integer(CMT),
	CS = erlang:list_to_integer(CST),
	CL = erlang:list_to_integer(CLT),
	%compare_two_digit(CM,M) andalso compare_two_digit(CS,S) andalso compare_two_digit(CL,L).
	if CM < M ->
				 false;
			 CM =:= M ->
				 if CS < S ->
						false;
					CS =:= S ->
						if CL < L ->
							   false;
						   true ->
							   true
						end;
					true ->
						true
				 end;
			 true ->
				 true
		  end.

compare_two_digit(N1,N2)->
	if N1 >= N2 ->
		   true;
	   N1 < N2 ->
		   false
	end.