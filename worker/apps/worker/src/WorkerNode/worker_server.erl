-module(worker_server).
-behaviour(gen_server).

-record(state, {
    monitors = #{}
}).

%API
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    io:format("[Worker] -> starting mnesia setup~n"),
    mnesia:start(),
    {ok, MainNode} = application:get_env(main_node),
    if 
    
        MainNode == node() -> mnesia_utils:initialize_schema([]), mnesia_utils:create_tables(), mnesia:info();
        true -> gen_server:call({worker_server, MainNode},{add_node_to_mnesia_cluster, node()}),mnesia:info()
    end,
    io:format("[Worker] -> starting worker server ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args)->
    {ok, #state{}}.

setup_erlang_job(Content)->
    Filename =  "job.erl",
    file:write_file(Filename, Content),
    {ok, Module, Binary}  = compile:file(Filename,[binary]),
    code:load_binary(Module, Filename, Binary),
    file:delete(Filename).

% Ping
handle_call(ping, _From, State) ->
    % TO SEND MORE STUFF
    {FromPid, _Tag} = _From,
    FromPid ! {other_info},
    {reply, pong, State};

% Compile received module
handle_call({setup_erlang_job, Content}, _From, State)->
    io:format("[Worker] -> erlang job setup~n"),
    {Pid, Ref} = spawn_monitor(fun() -> setup_erlang_job(Content) end),
    {reply, done, #state{monitors = maps:put(Ref, Pid, State#state.monitors)}};

% Execute the "map" function
handle_call(start_erlang_job, _From, State)->
    io:format("[Worker] -> erlang job start~n"),
    {Pid, Ref} = spawn_monitor(fun() -> job:run() end),
    {reply, done, #state{monitors = maps:put(Ref, Pid, State#state.monitors)}};

handle_call({add_node_to_mnesia_cluster, MnesiaNode}, _From, State)->
    io:format("[Worker] -> Adding node ~p to Mnesia cluster~n", [MnesiaNode]),
    mnesia_utils:add_node(MnesiaNode),
    {reply, done, State};
% Catch-all clause for unrecognized messages
handle_call(_UnexpectedMessage, _From, State) ->
    {reply, {error, unsupported_request}, State}.

%% TODO: check this
%% Handle DOWN messages from monitored processes
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    %%TODO: handle error message
    io:format("[Worker] -> Monitored process down. Ref: ~p, Reason: ~p~n", [Ref, Reason]),
    %% Remove the monitor from state
    {noreply, #state{monitors = maps:remove(Ref, State#state.monitors)}};
%% Catch-all for other messages
handle_info(Other, State) ->
    io:format("[Worker] -> Received unexpected message: ~p~n", [Other]),
    {noreply, State}.
%% TODO: check this


handle_cast(msg, State) ->
    io:format("~p~n",[msg]),
    {noreply, State}.