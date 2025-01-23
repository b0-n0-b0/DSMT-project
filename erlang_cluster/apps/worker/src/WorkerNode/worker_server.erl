-module(worker_server).
-behaviour(gen_server).
-record(state, {
    monitors = #{},
    task_id = "",
    input_split_id = "",
    processes_number = 1
}).
%API
-export([start_link/0]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    io:format("[Worker] -> starting mnesia setup~n"),
    mnesia:start(),
    {ok, ControllerNode} = application:get_env(controller_node),
    % ask to be added to the mnesia cluster
    gen_server:call({cowboy_listener, ControllerNode}, {add_node_to_mnesia_cluster, node()}),
    mnesia:info(),
    io:format("[Worker] -> starting worker server ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

% Ping
handle_call(ping, From, State) ->
    % TO SEND MORE STUFF
    % {FromPid, _Tag} = _From,
    % FromPid ! {other_info},
    io:format("[Worker] -> received ping from ~p~n", [From]),
    {reply, pong, State};
% Compile received module
handle_call({setup_erlang_task, TaskId, InputSplitId, ProcessNumber}, _From, State) ->
    io:format("[Worker] -> erlang task setup~n"),
    TaskModule = mnesia_utils:get_task_code_by_id(TaskId),
    Result = task_utils:setup_erlang_task(TaskModule),
    if
        Result == compilation_error ->
            {reply, {error, "compilation error"}, State};
        Result == export_error ->
            {reply, {error, "the \"run/1\" function must be exported"}, State};
        true ->
            {reply, {done}, #state{
                monitors = State#state.monitors,
                task_id = TaskId,
                input_split_id = InputSplitId,
                processes_number = ProcessNumber
            }}
    end;
% Execute the "map" function
handle_call(start_erlang_task, _From, State) ->
    io:format("[Worker] -> starting erlang task~n"),
    InputSplit = mnesia_utils:get_input_split_by_id(State#state.input_split_id),
    % {Pid, Ref} = spawn_monitor(fun() -> task_utils:execute_erlang_task(InputSplit, State#state.task_id) end),
    SpawnedProcesses = task_utils:spawn_workers(
        State#state.processes_number, [], InputSplit, State#state.task_id
    ),
    io:format("[Worker] -> all workers spawned~n"),
    task_utils:start_workers(SpawnedProcesses),
    {reply, done, #state{
        monitors = SpawnedProcesses,
        task_id = State#state.task_id,
        input_split_id = State#state.input_split_id,
        processes_number = State#state.processes_number
    }};
% Catch-all clause for unrecognized messages
handle_call(_UnexpectedMessage, _From, State) ->
    {reply, {error, unsupported_request}, State}.

%% Handle DOWN messages from monitored processes
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    io:format("[Worker] -> Monitored process down.~n"),
    %% Remove the monitor from state
    {_, ControllerNode} = application:get_env(controller_node),
    case Reason of
        normal ->
            gen_server:call({cowboy_listener, ControllerNode}, {worker_communication, done});
        _ ->
            gen_server:call(
                {cowboy_listener, ControllerNode},
                {worker_communication, {error, "Task crashed, please re-check the provided module"}}
            )
    end,
    {noreply, #state{
        monitors = maps:remove(Ref, State#state.monitors),
        task_id = State#state.task_id,
        input_split_id = State#state.input_split_id,
        processes_number = State#state.processes_number
    }};
%% Catch-all for other messages
handle_info(Other, State) ->
    io:format("[Worker] -> Received unexpected message: ~p~n", [Other]),
    {noreply, State}.

handle_cast(msg, State) ->
    io:format("~p~n", [msg]),
    {noreply, State}.
