-module(work_dispatch_utils).
-export([
    setup_worker_nodes/4, start_worker_nodes/1, get_available_nodes_list/2, send_updates_to_ws/2
]).

setup_worker_nodes([], _, _, _) ->
    ok;
setup_worker_nodes([CurrentNode | Nodes], TaskId, [CurrentSplit | InputSplitIds], ProcessNumber) ->
    case
        gen_server:call(
            {worker_server, CurrentNode}, {setup_erlang_task, TaskId, CurrentSplit, ProcessNumber}
        )
    of
        {done} ->
            setup_worker_nodes(Nodes, TaskId, InputSplitIds, ProcessNumber);
        {error, "compilation error"} ->
            {error, "compilation error"};
        {error, "the \"run/1\" function must be exported"} ->
            {error, "the \"run/1\" function must be exported"}
    end.

start_worker_nodes([]) ->
    ok;
start_worker_nodes([CurrentNode | Nodes]) ->
    gen_server:call({worker_server, CurrentNode}, start_erlang_task),
    start_worker_nodes(Nodes).

get_available_nodes_list([], AvailableNodes) ->
    AvailableNodes;
get_available_nodes_list([Node | Nodes], AvailableNodes) ->
    case net_adm:ping(Node) of
        pong ->
            get_available_nodes_list(Nodes, [Node | AvailableNodes]);
        pang ->
            get_available_nodes_list(Nodes, [AvailableNodes])
    end.


send_updates_to_ws(_, []) ->
    ok;
send_updates_to_ws(Update, [Current | RegisteredPids]) ->
    case string:str(atom_to_list(Current), "websocket_") of
        0 ->
            ok;
        _ ->
            Current ! (Update)
    end,
    send_updates_to_ws(Update, RegisteredPids).
