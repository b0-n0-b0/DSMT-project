-module(cowboy_listener).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
  io:format("[ClusterController] -> Cowboy server spawned~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

routes() ->
  Routes = [
    % {OfferEndpoint, socket_listener, []}
    % HTTP
    {"/", cowboy_static, {priv_file, controller, "index.html"}},
    {"/ping", cowboy_http_requests_handler, []}, %% test route
    {"/websocket", cowboy_ws_requests_handler, [{stats_interval, list_to_integer("50")}]} %% test route
    % WS
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
handle_call(Req, _, State) ->
  io:format("received request"),
  {reply, Req, State}.

handle_cast(_, State) ->
  {noreply, State}.