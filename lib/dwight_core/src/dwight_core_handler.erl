%%% @author Tristan Sloughter <>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2012 by Tristan Sloughter <>
-module(dwight_core_handler).

-export([init/3, 
         handle/2, 
         terminate/2]).

-record(state, {}).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {[HostTokens], Req2} = cowboy_http_req:host(Req),   
    case dwight_routes:find_route_id(HostTokens) of
        {ok, RouteId} ->    
            case dwight_routes:find_service(RouteId) of
                {ok, Host, Port} ->
                    {Method, Req3} = cowboy_http_req:method(Req2), 
                    {Path, Req4} = cowboy_http_req:path(Req3), 
                    {Headers, Req5} = cowboy_http_req:headers(Req4), 
                    {ok, Body, Req6} = cowboy_http_req:body(Req5), 

                    %% make request
                    {RespStatus, RespHeaders, RespBody} = 
                        dwight_core_req_server:send(atom_to_list(Method), Host, Port, Headers, Path, Body),
                    {ok, Req7} = cowboy_http_req:set_resp_body(RespBody, Req6),
                    {ok, Req8} = cowboy_http_req:reply(RespStatus, RespHeaders, Req7),

                    {ok, Req8, State};
                undefined ->
                    %% No service found, return a 500 error
                    {ok, Req3} = cowboy_http_req:reply(500, [], <<"">>, Req2),
                    {ok, Req3, State}
            end;
        undefined ->
            %% No domain found, return a 404
            {ok, Req3} = cowboy_http_req:reply(404, [], <<"">>, Req2),
            {ok, Req3, State}
    end.

terminate(_Req, _State) ->
    ok.

%%
%% Internal functions
%%
