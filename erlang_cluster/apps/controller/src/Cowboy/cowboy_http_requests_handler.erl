-module(cowboy_http_requests_handler).
 
-export([init/2]).
 
init(#{method := Method} = Req, _State) ->
    handle_req(Method, Req).
 
handle_req(<<"GET">>, Req) ->
    {ok, text_plain(Req, <<"pong">>)};
 
handle_req(_Method, Req) ->
    cowboy_req:reply(404, Req).
 
text_plain(Request, ResponseBody) ->
    ResponseHeaders = #{
        <<"content-type">> => <<"text/plain">>
    },
    cowboy_req:reply(200, ResponseHeaders, ResponseBody, Request).
