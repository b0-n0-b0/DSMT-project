-module(cowboy_http_requests_handler).
 
-export([init/2]).
 

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    case Path of 
        % TODO: receive input as file
        <<"/start_cluster">> ->
            case Method of
                <<"POST">> ->
                    start_cluster(Req, State);
                _ ->
                    handle_not_found(Req, State)
            end
    end.

start_cluster(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_urlencoded_body(Req),
    RequiredParams = [<<"TaskId">>, <<"ErlangModule">>, <<"Input">>,<<"ProcessNumber">>],
    case parse_body(Body, RequiredParams)of 
        {error, {missing_params, _MissingParams}} ->
            Reply = <<"Missing params">>,
            Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, Reply, Req2),
            {ok, Req3, State};
        Values ->
            case gen_server:call({cowboy_listener, node()},{create_erlang_task, Values}) of
                {error, ErrorMessage}->
                    Reply = ErrorMessage,
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, Reply, Req2),
                    {ok, Req3, State};
                done ->
                % TODO: handle stop/re-start(?) requests from phoenix (WS)
                    Reply = <<"Done">>,
                    Req3 = cowboy_req:reply(201, #{<<"content-type">> => <<"text/plain">>}, Reply, Req2),
                    {ok, Req3, State}
            end   
    end.

handle_not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"Not Found">>, Req),
    {ok, Req2, State}.
 
%parse POST request body
parse_body(Params, RequiredParams) ->
    % Check if all required parameters are present
    case check_required_params(Params, RequiredParams) of
        ok ->
            Values = [get_param(Params, Param) || Param <- RequiredParams],
            Values;
        {error, MissingParams} ->
            {error, {missing_params, MissingParams}}
    end.

% Check if all required parameters are present
check_required_params(Params, RequiredParams) ->
    ParamKeys = [Key || {Key, _Value} <- Params],
    MissingParams = [Param || Param <- RequiredParams, not lists:member(Param, ParamKeys)],
    case MissingParams of
        [] -> ok;
        _  -> {error, MissingParams}
    end.

% Get parameter value
get_param(Params, Param) ->
    proplists:get_value(Param, Params).
