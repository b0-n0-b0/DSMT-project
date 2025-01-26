-module(task_utils).
-export([
    setup_erlang_task/1,
    execute_erlang_task/2,
    spawn_workers/4,
    start_workers/1,
    kill_workers/1,
    split_input_per_process/2
]).

spawn_workers(_, SpawnedProcesses, [], _) ->
    maps:from_list(SpawnedProcesses);
spawn_workers(0, SpawnedProcesses, _, _) ->
    maps:from_list(SpawnedProcesses);
spawn_workers(ProcessesNumber, SpawnedProcesses, [CurrentProcessSplit | Splits], TaskId) ->
    Process = spawn_monitor(fun() -> task_utils:execute_erlang_task(CurrentProcessSplit, TaskId) end),
    spawn_workers(ProcessesNumber - 1, [Process | SpawnedProcesses], Splits, TaskId).

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
    io:format("[Worker] -> killing process ~p~n", [Worker]),
    exit(Worker, killed),
    kill_workers(Workers).

split_input_per_process(List, 0, Splits, SplitNumber) ->
    split_input_per_process(List, 1, Splits, SplitNumber);
split_input_per_process(List, _, Splits, 1) ->
    case List of
        [] ->
            Splits;
        _ ->
        [List | Splits]
    end;
split_input_per_process([], _, Splits, _) ->
    Splits;
split_input_per_process(List, SplitSize, Splits, SplitNumber) ->
    {CurrentSplit, NewList} = lists:split(SplitSize, List),
    split_input_per_process(NewList, SplitSize, [CurrentSplit | Splits], SplitNumber - 1).
split_input_per_process(List, HowManySplits) ->
    Length = length(List),
    SplitSize = Length div HowManySplits,
    Splits = split_input_per_process(List, SplitSize, [], HowManySplits),
    Splits.
