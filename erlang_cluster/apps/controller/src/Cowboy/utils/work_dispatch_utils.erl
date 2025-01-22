-module(work_dispatch_utils).
-export([setup_worker_nodes/4,start_worker_nodes/1]).

setup_worker_nodes([],_,_,_)->
    ok;
setup_worker_nodes([CurrentNode | Nodes], TaskId, [CurrentSplit | InputSplitIds], ProcessNumber)->
    case gen_server:call({worker_server, CurrentNode}, {setup_erlang_task, TaskId, CurrentSplit, ProcessNumber})of 
        {done} ->
            setup_worker_nodes(Nodes, TaskId, InputSplitIds, ProcessNumber);
        {error, "compilation error"}->
            {error, "compilation error"};
        {error, "the \"run/1\" function must be exported"}->
            {error, "the \"run/1\" function must be exported"}
    end.

start_worker_nodes([])->
    ok;
start_worker_nodes([CurrentNode | Nodes])->
    gen_server:call({worker_server, CurrentNode}, start_erlang_task),
    start_worker_nodes(Nodes).