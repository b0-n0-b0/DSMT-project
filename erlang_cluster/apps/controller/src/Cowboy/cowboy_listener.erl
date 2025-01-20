-module(cowboy_listener).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
  io:format("[ClusterController] -> starting mnesia server"),
  mnesia:start(),
  mnesia_utils:initialize_schema([]), mnesia_utils:create_tables(), mnesia:info(), %% mnesia db setup
  io:format("[ClusterController] -> Cowboy server spawned~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

routes() ->
  Routes = [
    % {OfferEndpoint, socket_listener, []}
    % HTTP
    {"/", cowboy_static, {priv_file, controller, "index.html"}},
    {"/alive", cowboy_static,  {priv_file, controller, "alive.html"}}, %% test route
    % WS
    {"/websocket", cowboy_ws_requests_handler, [{stats_interval, list_to_integer("50")}]} %% test route
  ],
  {Routes}.

init(_) ->
  % TODO: take from env
  Port = 8080,
  {Routes} = routes(),

  Dispatcher = cowboy_router:compile([{'_', Routes}]),

  {ok, Pid} = cowboy:start_clear(cowboy_routes, [{port, Port}], #{env => #{dispatch => Dispatcher}}),
  io:format("[ClusterController] -> new cowboy listener initialized with pid ~p at port ~p~n", [Pid, Port]),
  {ok, []}.

% TODO: add endpoints to receive updates from workers ???
% probably can do this with ws_info
handle_call({add_node_to_mnesia_cluster, MnesiaNode}, _From, State)->
    io:format("[ClusterController] -> Adding node ~p to Mnesia cluster~n", [MnesiaNode]),
    mnesia_utils:add_node(MnesiaNode),
    {reply, done, State};
% create task in DB
handle_call({create_erlang_task, TaskId, TaskModule, InputSplits}, _From, State)->
    mnesia_utils:create_task(TaskId,TaskModule,InputSplits),
    io:format("[ClusterController] -> erlang task creation~n"),
    {reply, done, State};
% Catch-all clause for unrecognized messages
handle_call(_, _, State) ->
  io:format("[ClusterController] -> received unexpected request"),
  {reply, {error, unsupported_request}, State}.

handle_cast(_, State) ->
  {noreply, State}.