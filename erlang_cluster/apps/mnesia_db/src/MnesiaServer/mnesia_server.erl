-module(mnesia_server).
-include("./include/task.hrl").
-include_lib("stdlib/include/qlc.hrl").
%API
-export([start_link/0, create_task/3, insert_partial_result/3, get_partial_results_by_id/1, get_input_split_by_id/1]).


start_link() ->
    io:format("[Mnesia DB] -> starting mnesia db node~n"),
    initialize_schema(),
    {ok,self()}.

%% SETUP
%% TODO: add a section for replica setup
create_tables()->
    mnesia:create_table(task,
                        [{attributes, record_info(fields, task)}]),
    mnesia:create_table(input_split,
                        [{attributes, record_info(fields, input_split)}]),
    mnesia:create_table(partial_result,
                        [
                            {attributes, record_info(fields, partial_result)},
                            {index, [task_id]}
                        ]
                    ),
    ok.

initialize_schema()->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_tables(),
    mnesia:info().

%% UTILITIES
%% Create an input split
insert_input_split(TaskId, SplitNumber, [CurrentSplit|Others]) ->
    mnesia:write(#input_split{
        id = TaskId ++ "-" ++ SplitNumber, 
        task_id = TaskId,
        data = CurrentSplit}),
    insert_input_split(TaskId, SplitNumber + 1, Others);
insert_input_split(_, _, []) -> ok.

%% Create a partial result
insert_partial_result(PartialResultId,TaskId, Data)->
    PartialResult = #partial_result{
            id=PartialResultId,
            task_id=TaskId,
            data=Data
        },
    Fun = fun() ->
            mnesia:write(PartialResult)
        end,
    mnesia:transaction(Fun).

%% Get partial result by ID
get_partial_results_by_id(PartialResultId)->
    Fun = fun()->
        mnesia:read(partial_result, PartialResultId)
        end,
    mnesia:transaction(Fun).

%% Get input split by ID
get_input_split_by_id(InputSplitId)->
    Fun = fun()->
        mnesia:read(input_split, InputSplitId)
        end,
    mnesia:transaction(Fun).
%% Create a new task
create_task(TaskId, TaskModule, InputSplits) ->
    Task = #task{
            id = TaskId,
            task_module = TaskModule
        },
    Fun = fun() ->
                mnesia:write(Task),
                insert_input_split(TaskId, 0, InputSplits)
          end,
    mnesia:transaction(Fun).

%% CLEANUP