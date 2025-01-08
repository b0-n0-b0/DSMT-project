-module(worker_server).

-behaviour(gen_server).

%API
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
  io:format("[Worker] -> starting new worker~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args)->
    {ok, {}}.

setup_erlang_job(Content)->
    Filename =  "job.erl",
    file:write_file(Filename, Content),
    compile:file(Filename),
    file:delete(Filename).
% ping
handle_call(ping, _From, State) ->
    {reply, pong, State};
% compile received module
handle_call({setup_erlang_job, Content}, _From, State)->
    io:format("[Worker] -> erlang job setup~n"),
    spawn_monitor(fun() -> setup_erlang_job(Content) end),
    {reply, done, State}.

handle_cast(msg, State) ->
    io:format("~p~n",[msg]),
    {noreply, State}.