-module(worker_server).
-behaviour(gen_server).

-record(state, {
    monitors = #{}
}).

%API
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
  io:format("[Worker] -> starting new worker~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args)->
    {ok, #state{monitors = #{}}}.

setup_erlang_job(Content)->
    Filename =  "job.erl",
    file:write_file(Filename, Content),
    {ok, Module, Binary}  = compile:file(Filename,[binary]),
    code:load_binary(Module, Filename, Binary),
    file:delete(Filename).

% Ping
handle_call(ping, _From, State) ->
    % TO SEND MORE STUFF
    {FromPid, _Tag} = _From,
    FromPid ! {other_info},
    {reply, pong, State};

% Compile received module
handle_call({setup_erlang_job, Content}, _From, State)->
    io:format("[Worker] -> erlang job setup~n"),
    {Pid, Ref} = spawn_monitor(fun() -> setup_erlang_job(Content) end), %%TODO: handle error message
    {reply, done, #{monitors => maps:put(Ref, Pid, State#state.monitors)}};

% Execute the "map" function
handle_call(start_erlang_job, _From, State)->
    io:format("[Worker] -> erlang job start~n"),
    {Pid, Ref} = spawn_monitor(fun() -> job:run() end), %%TODO: handle error message
    {reply, done, #{monitors => maps:put(Ref, Pid, State#state.monitors)}};
% Catch-all clause for unrecognized messages
handle_call(_UnexpectedMessage, _From, State) ->
    {reply, {error, unsupported_request}, State}.

%% TODO: check this
%% Handle DOWN messages from monitored processes
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    io:format("[Worker] -> Monitored process down. Ref: ~p, Reason: ~p~n", [Ref, Reason]),
    %% Remove the monitor from state
    Monitors = maps:get(monitors, State),
    UpdatedMonitors = maps:remove(Ref, Monitors),
    {noreply, #state{monitors = UpdatedMonitors}};
%% Catch-all for other messages
handle_info(Other, State) ->
    io:format("[Worker] -> Received unexpected message: ~p~n", [Other]),
    {noreply, State}.
%% TODO: check this


handle_cast(msg, State) ->
    io:format("~p~n",[msg]),
    {noreply, State}.