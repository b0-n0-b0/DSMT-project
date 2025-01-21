-module(cowboy_http_requests_handler).
 
-export([init/2]).
 

init(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            handle_post(Req, State);
        _ ->
            handle_not_found(Req, State)
    end.

handle_post(Req, State) ->
    % {ok, Body, Req2} = cowboy_req:read_body(Req),
    {ok, Body, Req2} = cowboy_req:read_urlencoded_body(Req),
    RequiredParams = [<<"TaskId">>, <<"ErlangModule">>, <<"Input">>],
    % io:format("~p~n",[Body]),
    case parse_body(Body, RequiredParams)of 
        {error, {missing_params, MissingParams}} ->
            Reply = <<"Missing params">>,
            {ok, Req3} = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, Reply, Req2),
            {ok, Req3, State};
        Values ->
            % TODO: start cluster -> handle notification (fail / completion) from nodes (WS)
            maps:get("gen_server", State) ! {create_erlang_task, Values},

            % TODO: handle stop/re-start(?) requests from phoenix (WS)
            Reply = <<"Done">>,
            {ok, Req3} = cowboy_req:reply(201, #{<<"content-type">> => <<"text/plain">>}, Reply, Req2),
            {ok, Req3, State}
    end.

handle_not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"Not Found">>, Req),
    {ok, Req2, State}.
 
%Function to check parameters and call another function
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
