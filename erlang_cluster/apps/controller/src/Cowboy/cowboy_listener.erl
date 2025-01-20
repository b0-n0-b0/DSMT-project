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
    {"/ping", cowboy_requests_handler, []}
  ],
  {Routes}.

init(_) ->
  Port = 8080,
  {Routes} = routes(),

  Dispatcher = cowboy_router:compile([{'_', Routes}]),

  {ok, Pid} = cowboy:start_clear(cowboy_routes, [{port, Port}], #{env => #{dispatch => Dispatcher}}),
  io:format("[ClusterController] -> new cowboy listener initialized with pid ~p at port ~p~n", [Pid, Port]),
  {ok, []}.

% TODO: add endpoints to receive updates from workers
handle_call(Req, _, State) ->
  io:format("received request"),
  {reply, Req, State}.

handle_cast(_, State) ->
  {noreply, State}.