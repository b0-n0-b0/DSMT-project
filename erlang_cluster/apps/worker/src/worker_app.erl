%%%-------------------------------------------------------------------
%% @doc worker public API
%% @end
%%%-------------------------------------------------------------------

-module(worker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[Worker] -> starting supervisor~n"),
    worker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
