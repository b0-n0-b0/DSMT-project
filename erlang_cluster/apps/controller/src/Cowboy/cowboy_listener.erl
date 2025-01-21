-module(cowboy_listener).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
  io:format("[ClusterController] -> starting mnesia server"),
  mnesia:start(),
  mnesia_utils:initialize_schema([]), mnesia_utils:create_tables(), mnesia:info(), %% mnesia db setup
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

  {ok, Pid} = cowboy:start_clear(cowboy_routes, [{port, Port}], #{env => #{dispatch => Dispatcher}}),
  io:format("[ClusterController] -> new cowboy listener initialized with pid ~p at port ~p~n", [Pid, Port]),
  {ok, []}.

% TODO: add endpoints to receive updates from workers -> send update to ws_info via cowboy PID
handle_call({add_node_to_mnesia_cluster, MnesiaNode}, _From, State)->
    io:format("[ClusterController] -> Adding node ~p to Mnesia cluster~n", [MnesiaNode]),
    mnesia_utils:add_node(MnesiaNode),
    io:format("~p~n",[self()]),
    NewState = [MnesiaNode|State],
    {reply, done, NewState};

% Catch-all clause for unrecognized messages
handle_call(_, _, State) ->
  io:format("[ClusterController] -> received unexpected request"),
  {reply, {error, unsupported_request}, State}.

handle_cast(_, State) ->
  {noreply, State}.

% Handle down nodes in State
handle_info({create_erlang_task, [TaskId, TaskModule, Input]}, State)->
  % io:format("~p~n~p~n~p~n",[binary_to_list(TaskId),binary_to_list(TaskModule),binary_to_list(InputSplits)]),
  TokenizedInput = input_utils:input_line_tokenizer(Input),
  % TODO: take input and split it between the nodes 
  % mnesia_utils:create_task(binary_to_list(TaskId),binary_to_list(TaskModule),TokenizedInput),
  io:format("[ClusterController] -> erlang task creation~n"),
  {noreply, State}.