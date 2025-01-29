-module(cowboy_http_requests_handler).
 
-export([init/2]).
 

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    case Path of 
        <<"/start_cluster">> ->
            case Method of
                <<"POST">> ->
                    start_cluster(Req, State);
                _ ->
                    handle_not_found(Req, State)
            end
    end.

multipart(Req0) ->
    multipart(Req0, #{}).  %% Use a map to store parameters

multipart(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    {ok, Body, Req2} = cowboy_req:read_part_body(Req1),
                    multipart(Req2, Acc#{FieldName => Body});
                {file, FieldName, _Filename, _CType} ->
                    {Req2, FileContent} = stream_file(Req1),
                    multipart(Req2, Acc#{FieldName => FileContent})
            end;
        {done, Req} ->
            {Req, Acc}  %% Return request state + accumulated parameters
    end.

stream_file(Req0) ->
    stream_file(Req0, <<>>).

stream_file(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, LastBodyChunk, Req} ->
            FinalContent = <<Acc/binary, LastBodyChunk/binary>>,
            {Req, FinalContent};  %% Return both Req and file content
        {more, BodyChunk, Req} ->
            stream_file(Req, <<Acc/binary, BodyChunk/binary>>)
    end.

start_cluster(Req, State) ->
    {Req2, Body} = multipart(Req),
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
    ParamKeys = maps:keys(Params),
    MissingParams = [Param || Param <- RequiredParams, not lists:member(Param, ParamKeys)],
    case MissingParams of
        [] -> ok;
        _  -> {error, MissingParams}
    end.

% Get parameter value
get_param(Params, Param) ->
    maps:get(Param,Params).
