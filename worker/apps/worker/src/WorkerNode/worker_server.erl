-module(worker_server).
-behaviour(gen_server).
-record(state, {
    monitors = #{},
    task_id = "",
    input_split_id="",
    processes_number=1
}).
%API
-export([start_link/0]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    io:format("[Worker] -> starting mnesia setup~n"),
    mnesia:start(),
    {ok, MainNode} = application:get_env(main_node),
    %%start mnesia or just aggregate to the cluster
    if 
        MainNode == node() -> mnesia_utils:initialize_schema([]), mnesia_utils:create_tables(), mnesia:info();
        true -> gen_server:call({worker_server, MainNode},{add_node_to_mnesia_cluster, node()}),mnesia:info()
    end,
    io:format("[Worker] -> starting worker server ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args)->
    {ok, #state{}}.

% Ping
handle_call(ping, _From, State) ->
    % TO SEND MORE STUFF
    {FromPid, _Tag} = _From,
    FromPid ! {other_info},
    {reply, pong, State};

% create task in DB
% TODO: REMOVE ONCE THE FULL INTERACTION IS DONE
handle_call({create_erlang_task, TaskId, TaskModule, InputSplits}, _From, State)->
    mnesia_utils:create_task(TaskId,TaskModule,InputSplits),
    io:format("[Worker] -> erlang task creation~n"),
    {reply, done, State};

% Compile received module
handle_call({setup_erlang_task, TaskId, InputSplitId, ProcessNumber}, _From, State)->
    io:format("[Worker] -> erlang task setup~n"),
    TaskModule = mnesia_utils:get_task_code_by_id(TaskId),
    Result = task_utils:setup_erlang_task(TaskModule),
    if 
        Result == compilation_error ->
            {reply, {error, "compilation error"}, State};
        Result == export_error ->
            {reply, {error, "the \"run/1\" function must be exported"}, State};
        true ->
            {reply, {done}, #state{monitors=State#state.monitors, task_id=TaskId, input_split_id=InputSplitId, processes_number=ProcessNumber}}
    end;

% Execute the "map" function
handle_call(start_erlang_task, _From, State)->
    io:format("[Worker] -> starting erlang task~n"),
    InputSplit = mnesia_utils:get_input_split_by_id(State#state.input_split_id),
    % {Pid, Ref} = spawn_monitor(fun() -> task_utils:execute_erlang_task(InputSplit, State#state.task_id) end),
    SpawnedProcesses = task_utils:spawn_workers(State#state.processes_number,[],InputSplit,State#state.task_id),
    io:format("[Worker] -> all workers spawned~n"),
    task_utils:start_workers(SpawnedProcesses),
    {reply, done, #state{monitors = SpawnedProcesses, 
        task_id=State#state.task_id, 
        input_split_id=State#state.input_split_id,
        processes_number=State#state.processes_number}};

handle_call({add_node_to_mnesia_cluster, MnesiaNode}, _From, State)->
    io:format("[Worker] -> Adding node ~p to Mnesia cluster~n", [MnesiaNode]),
    mnesia_utils:add_node(MnesiaNode),
    {reply, done, State};

% Catch-all clause for unrecognized messages
handle_call(_UnexpectedMessage, _From, State) ->
    {reply, {error, unsupported_request}, State}.
%%TODO: handle error message
%% Handle DOWN messages from monitored processes
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    io:format("[Worker] -> Monitored process down. Ref: ~p, Reason: ~p~n", [Ref, Reason]),
    %% Remove the monitor from state
    {noreply, #state{monitors = maps:remove(Ref, State#state.monitors), 
        task_id=State#state.task_id, 
        input_split_id=State#state.input_split_id,
        processes_number=State#state.processes_number}};

%% Catch-all for other messages
handle_info(Other, State) ->
    io:format("[Worker] -> Received unexpected message: ~p~n", [Other]),
    {noreply, State}.

handle_cast(msg, State) ->
    io:format("~p~n",[msg]),
    {noreply, State}.