-module(cowboy_ws_requests_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).


init(Req, State) ->
    io:format("[ClusterController] -> websocket connection initiated~n"),
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    register(list_to_atom("websocket_"++random_string:generate(20)), self()),
    self() ! send_status,
    {ok, State}.

websocket_handle({text, Data}, State) ->
    io:format("[ClusterController] -> WebSocket data from client: ~p~n", [Data]),
    case Data of
        <<"get_final_result">> ->
            try
                Reply = gen_server:call({cowboy_listener, node()}, get_final_result),
                ReplyBin = list_to_binary(io_lib:format("~p", [Reply])),
                ReplyJson = jsone:encode( #{<<"final_result">>=> ReplyBin}),
                io:format("Reply sent to client: ~p~n", [ReplyJson]),
                {reply, {text, ReplyJson}, State}
            catch
                Class:Reason ->
                    io:format("Error fetching final result: ~p, Reason: ~p~n", [Class, Reason]),
                    {reply, {text, <<"error">>}, State}
            end;
        _ ->
            io:format("Unexpected request received: ~p~n", [Data]),
            {reply, {text, <<"unexpected request">>}, State}
    end;
websocket_handle(_, State) ->
    io:format("Invalid WebSocket frame received.~n"),
    {reply, {text, <<"invalid frame">>}, State}.

websocket_info(send_status, State) ->
    Status = gen_server:call({cowboy_listener, node()}, {ws_request, get_status}),
    StateJson = jsone:encode( #{<<"status">>=> list_to_binary(Status)}),
    {reply, {text, StateJson}, State};

% Send data on ws when worker sends update
websocket_info({info, Info}, State) ->
    case Info of
        {error, ErrorMessage}->
            Status = gen_server:call({cowboy_listener, node()}, {ws_request, get_status}),
            StateJson = jsone:encode( #{<<"status">>=> list_to_binary(Status), <<"errorMessage">> => list_to_binary(ErrorMessage)}),
            {reply, {text, StateJson}, State};
        _ ->
            Status = gen_server:call({cowboy_listener, node()}, {ws_request, get_status}),
            StateJson = jsone:encode( #{<<"status">>=> list_to_binary(Status), <<"progress">>=> list_to_binary(float_to_list(Info))}),
            {reply, {text, StateJson}, State}
    end.


terminate(_Reason, Req, _State) ->
    io:format("[ClusterController] ->  connection terminated~n~p~n", [maps:get(peer, Req)]),
    ok.