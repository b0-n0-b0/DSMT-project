-module(mnesia_utils).
-include("./include/task.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([
    create_tables/0,
    initialize_schema/1,
    insert_partial_result/3,
    get_input_split_by_id/1,
    get_partial_results_by_id/1,
    create_task/3,
    add_node/1,
    remove_task/1,
    get_task_code_by_id/1,
    update_task_status/2,
    get_task_by_id/1,
    get_partial_results_by_taskid/1,
    write_final_result/2
]).

%% SETUP
create_tables() ->
    mnesia:create_table(
        task,
        [{attributes, record_info(fields, task)}]
    ),
    mnesia:create_table(
        input_split,
        [
            {attributes, record_info(fields, input_split)},
            {index, [task_id]}
        ]
    ),
    mnesia:create_table(
        partial_result,
        [
            {attributes, record_info(fields, partial_result)},
            {index, [task_id]}
        ]
    ),
    ok.

initialize_schema(OtherNodes) ->
    mnesia:create_schema([node() | OtherNodes]),
    mnesia:start().

add_node(Node) ->
    mnesia:change_config(extra_db_nodes, [Node]).

%% UTILITIES

%% Create a partial result
insert_partial_result(PartialResultId, TaskId, Data) ->
    PartialResult = #partial_result{
        id = PartialResultId,
        task_id = TaskId,
        data = Data
    },
    Fun = fun() ->
        mnesia:write(PartialResult)
    end,
    mnesia:transaction(Fun).

%% Write final result
write_final_result(TaskId, Result) ->
    Fun = fun() ->
        [T] = mnesia:wread({task, TaskId}),
        mnesia:write(T#task{final_result = Result})
    end,
    mnesia:transaction(Fun).

%% Get partial result by ID
get_partial_results_by_id(PartialResultId) ->
    Fun = fun() ->
        mnesia:read(partial_result, PartialResultId)
    end,
    mnesia:transaction(Fun).

% Get partial result by TaskID
get_partial_results_by_taskid(TaskId) ->
    Fun = fun() ->
        mnesia:index_read(partial_result, TaskId, task_id)
    end,
    {_, PartialResults} = mnesia:transaction(Fun),
    [PartialResult || {_, _, _,PartialResult} <- PartialResults].

%% Get input split by ID
get_input_split_by_id(InputSplitId) ->
    Fun = fun() ->
        mnesia:read(input_split, InputSplitId)
    end,
    {_, [{_, _, _, Data} | _Rest]} = mnesia:transaction(Fun),
    Data.

get_task_code_by_id(TaskId) ->
    Fun = fun() ->
        mnesia:read(task, TaskId)
    end,
    {_, [{_, _, TaskModule, _, _} | _Rest]} = mnesia:transaction(Fun),
    TaskModule.

get_task_by_id(TaskId) ->
    Fun = fun() ->
        mnesia:read(task, TaskId)
    end,
    case mnesia:transaction(Fun) of
        {atomic, [Task | _Rest]} ->
            Task;
        {atomic, []} ->
            undefined; % Default return value if no task is found
        {aborted, Reason} ->
            {error, Reason} % Return an error tuple if the transaction fails
    end.

%% util for input split IDs
create_input_split_ids(_, 0, SplitIds) ->
    SplitIds;
create_input_split_ids(TaskId, SplitNumber, SplitIds) ->
    CurrentSplitId = TaskId ++ "-" ++ integer_to_list(SplitNumber),
    create_input_split_ids(TaskId, SplitNumber - 1, [CurrentSplitId | SplitIds]).

%% Create an input split
insert_input_split(_, [], _) ->
    ok;
insert_input_split(TaskId, [CurrentSplit | Others], [CurrentInputSplitId | InputSplitIds]) ->
    Fun = fun() ->
        mnesia:write(#input_split{
            id = CurrentInputSplitId,
            task_id = TaskId,
            data = CurrentSplit
        })
    end,
    mnesia:transaction(Fun),
    insert_input_split(TaskId, Others, InputSplitIds).

%% Create a new task
create_task(TaskId, TaskModule, InputSplits) ->
    InputSplitIds = create_input_split_ids(TaskId, length(InputSplits), []),
    Task = #task{
        id = TaskId,
        task_module = TaskModule,
        status = ready,
        final_result = nil
    },
    Fun = fun() ->
        mnesia:write(Task),
        insert_input_split(TaskId, InputSplits, InputSplitIds)
    end,
    mnesia:transaction(Fun),
    InputSplitIds.

update_task_status(Value, TaskId) ->
    Fun = fun() ->
        [T] = mnesia:wread({task, TaskId}),
        mnesia:write(T#task{status = Value})
    end,
    mnesia:transaction(Fun).

%%CLEANUP
remove_task(TaskId) ->
    Fun = fun() ->
        mnesia:delete(input_split, TaskId, write),
        mnesia:delete(partial_result, TaskId, write)
    end,
    mnesia:transaction(Fun).
