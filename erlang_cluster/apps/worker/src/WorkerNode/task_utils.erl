-module(task_utils).
-export([
    setup_erlang_task/1, execute_erlang_task/2, spawn_workers/4, start_workers/1, kill_workers/1
]).

spawn_workers(0, SpawnedProcesses, _InputSplit, _TaskId) ->
    maps:from_list(SpawnedProcesses);
spawn_workers(ProcessesNumber, SpawnedProcesses, InputSplit, TaskId) ->
    Process = spawn_monitor(fun() -> task_utils:execute_erlang_task(InputSplit, TaskId) end),
    spawn_workers(ProcessesNumber - 1, [Process | SpawnedProcesses], InputSplit, TaskId).

start_workers(Workers) ->
    maps:foreach(
        fun(Key, _Value) ->
            Key ! start
        end,
        Workers
    ).

setup_erlang_task(TaskModule) ->
    Filename = "task.erl",
    file:write_file(Filename, TaskModule),
    CompilationResult = compile:file(Filename, [binary]),
    if
        CompilationResult == error ->
            Return = compilation_error;
        true ->
            {ok, Module, Binary} = CompilationResult,
            code:load_binary(Module, Filename, Binary),
            Return =
                case erlang:function_exported(Module, run, 1) of
                    true -> ok;
                    false -> export_error
                end,
            file:delete(Filename)
    end,
    Return.

execute_erlang_task(InputSplit, TaskId) ->
    receive
        start ->
            PartialResult = task:run(InputSplit),
            PartialResultId = random_string:generate(20),
            mnesia_utils:insert_partial_result(PartialResultId, TaskId, PartialResult)
    end.

kill_workers([]) ->
    ok;
kill_workers([Worker | Workers]) ->
    io:format("[Worker] -> killing process ~p~n",[Worker]),
    exit(Worker, killed),
    kill_workers(Workers).
