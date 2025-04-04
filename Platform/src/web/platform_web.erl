%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for platform.

-module(platform_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, port1_loop/2]).

%% External API

start(Options) ->
    {DocRoot, OptionsT} = get_option(docroot, Options),
	Options1 = proplists:get_value(port1, OptionsT),
%%     [Options1,Options2] = depart_options(OptionsT),
    Port1_Loop = fun (Req) ->
                   ?MODULE:port1_loop(Req, DocRoot)
           end,
%%     Port2_Loop = fun (Req) ->
%%               ?MODULE:port2_loop(Req, DocRoot)
%%          end,
  mochiweb_http:start([{name, port1}, {loop, Port1_Loop} | Options1]).
%%   mochiweb_http:start([{name, port2}, {loop, Port2_Loop} | Options2]).

stop() ->
    mochiweb_http:stop(port1).
%%     mochiweb_http:stop(port2).

port1_loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    try
        platform_handle:handle(Method, Path, Req)
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

%% port2_loop(Req, _DocRoot) ->
%%     "/" ++ Path = Req:get(path),
%%     Method = Req:get(method),
%%     try
%%         platform_handle:handle(Method, Path, Req)
%%     catch
%%         Type:What ->
%%             Report = ["web request failed",
%%                       {path, Path},
%%                       {type, Type}, {what, What},
%%                       {trace, erlang:get_stacktrace()}],
%%             error_logger:error_report(Report),
%%             %% NOTE: mustache templates need \ because they are not awesome.
%%             Req:respond({500, [{"Content-Type", "text/plain"}],
%%                          "request failed, sorry\n"})
%%     end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%% depart_options(Options)-> 
%%   Options1 = proplists:get_value(port1, Options),
%%   Options2 = proplists:get_value(port2, Options),
%%   [Options1,Options2].

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
