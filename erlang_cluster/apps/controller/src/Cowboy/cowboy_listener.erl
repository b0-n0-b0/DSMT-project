-module(cowboy_listener).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    io:format("[ClusterController] -> starting mnesia server"),
    mnesia:start(),
    %% mnesia db setup
    mnesia_utils:initialize_schema([]),
    mnesia_utils:create_tables(),
    mnesia:info(),
    io:format("[ClusterController] -> Cowboy server spawned~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

routes() ->
    Routes = [
        % {OfferEndpoint, socket_listener, []}
        % HTTP: start cluster stuff
        {"/start_cluster", cowboy_http_requests_handler, #{"gen_server" => self()}},
        {"/", cowboy_static, {priv_file, controller, "index.html"}},
        % WS: monitoring / control cluster work
        {"/websocket", cowboy_ws_requests_handler, [{stats_interval, list_to_integer("50")}]}
    ],
    {Routes}.

init(_) ->
    % TODO: take from env
    Port = 1337,
    {Routes} = routes(),

    Dispatcher = cowboy_router:compile([{'_', Routes}]),

    {ok, CowboyPid} = cowboy:start_clear(cowboy_routes, [{port, Port}], #{
        env => #{dispatch => Dispatcher}
    }),
    io:format("[ClusterController] -> new cowboy listener initialized with pid ~p at port ~p~n", [
        CowboyPid, Port
    ]),
    {ok, #{
        "available_nodes" => [],
        "currently_running_processes" => 0,
        "current_task" => null,
        "total_process_number" => null
    }}.

handle_call({add_node_to_mnesia_cluster, MnesiaNode}, _From, State) ->
    io:format("[ClusterController] -> Adding node ~p to Mnesia cluster~n", [MnesiaNode]),
    mnesia_utils:add_node(MnesiaNode),
    NewState = maps:put(
        "available_nodes", [MnesiaNode | maps:get("available_nodes", State)], State
    ),
    {reply, done, NewState};
handle_call({create_erlang_task, [TaskId, TaskModule, Input, ProcessNumber]}, _From, State) ->
    % io:format("~p~n~p~n~p~n",[binary_to_list(TaskId),binary_to_list(TaskModule),binary_to_list(InputSplits)]),
    io:format("[ClusterController] -> erlang task creation~n"),
    AvailableWorkers = work_dispatch_utils:get_available_nodes_list(
        maps:get("available_nodes", State), []
    ),
    case WorkerNumber = length(AvailableWorkers) of
        0 ->
            {reply, {error, "no worker nodes available in the cluster"}, State};
        _ ->
            TokenizedInput = input_utils:input_line_tokenizer(Input),
            InputSplits = input_utils:input_splitter(TokenizedInput, WorkerNumber),
            InputSplitIds = mnesia_utils:create_task(
                binary_to_list(TaskId), binary_to_list(TaskModule), InputSplits
            ),

            io:format("[ClusterController] -> Task and InputSplits created in mnesia~n"),
            Nodes = AvailableWorkers,
            {ProcessNumberInteger, _} = string:to_integer(binary_to_list(ProcessNumber)),
            case
                work_dispatch_utils:setup_worker_nodes(
                    Nodes, binary_to_list(TaskId), InputSplitIds, ProcessNumberInteger
                )
            of
                {error, "compilation error"} ->
                    {reply, {error, "compilation error"}, State};
                {error, "the \"run/1\" function must be exported"} ->
                    {reply, {error, "the \"run/1\" function must be exported"}, State};
                ok ->
                    work_dispatch_utils:start_worker_nodes(Nodes),
                    io:format("[ClusterController] -> Work dispached~n"),
                    mnesia_utils:update_task_status(running, binary_to_list(TaskId)),
                    NewState1 = maps:put(
                        "currently_running_processes", WorkerNumber * ProcessNumberInteger, State
                    ),
                    NewState2 = maps:put(
                        "total_process_number", WorkerNumber * ProcessNumberInteger, NewState1
                    ),
                    NewState = maps:put("current_task", binary_to_list(TaskId), NewState2),
                    {reply, done, NewState}
            end
    end;
handle_call({worker_communication, Message}, _From, State) ->
    % TODO: updates from workers -> send update to ws_info via cowboy PID
    case Message of
        done ->
            CurrentlyWorkingNodes = maps:get("currently_running_processes", State) - 1,
            case CurrentlyWorkingNodes of
                0 ->
                    io:format("[ClusterController] -> Work done"),
                    mnesia_utils:update_task_status(done, maps:get("current_task", State));
                _ ->
                    Total = maps:get("total_process_number", State),
                    Progress = ((Total - CurrentlyWorkingNodes) / Total) * 100,
                    work_dispatch_utils:send_updates_to_ws(Progress, registered()),
                    io:format("[ClusterController] -> Progress: ~p~n", [Progress]),
                    ok
            end;
        % TODO: handle errors
        {error, ErrorMessage} ->
            io:format("~p~n", [ErrorMessage])
    end,
    NewState = maps:put(
        "currently_running_processes", maps:get("currently_running_processes", State) - 1, State
    ),
    {reply, ok, NewState};
% Catch-all clause for unrecognized messages
handle_call(_, _, State) ->
    io:format("[ClusterController] -> received unexpected request"),
    {reply, {error, unsupported_request}, State}.

handle_cast(_, State) ->
    {noreply, State}.
