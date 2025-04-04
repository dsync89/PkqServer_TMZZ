%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for platform.

-module(platform_pay_web).
-include("common.hrl").
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, port1_loop/2]).

%% External API

start(Options) ->
	?DEBUG("platform_pay_web start1"),
    {DocRoot, OptionsT} = get_option(docroot, Options),
	?DEBUG("platform_pay_web start2 QueryList = ~p~n", [Options]),
	Options1 = proplists:get_value(port1, OptionsT),
	?DEBUG("platform_pay_web start3 QueryList = ~p~n", [Options1]),
    Port1_Loop = fun (Req) ->
                   ?MODULE:port1_loop(Req, DocRoot)
           end,
	?DEBUG("platform_pay_web start4"),
  mochiweb_http:start([{name, port1}, {loop, Port1_Loop} | Options1]).

stop() ->
    mochiweb_http:stop(port1).

port1_loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    try
        platform_pay_handle:handle(Method, Path, Req)
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
