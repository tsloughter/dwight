%%% @author Tristan Sloughter <>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2012 by Tristan Sloughter <>
-module(dwight_core_handler2).

-export([init/3, 
         info/3, 
         terminate/2]).

-define(TIMEOUT, 60000).

-record(state, {}).

init({tcp, http}, Req, _Opts) ->
    {[HostTokens], Req2} = cowboy_http_req:host(Req),   
    case dwight_routes:find_route_id(HostTokens) of
        {ok, RouteId} ->    
            case dwight_routes:find_service(RouteId) of
                {ok, Host, Port} ->
                    {Method, Req3} = cowboy_http_req:method(Req2), 
                    {Path, Req4} = cowboy_http_req:path(Req3), 
                    {Headers, Req5} = cowboy_http_req:headers(Req4), 
                    {ok, Body, Req6} = cowboy_http_req:body(Req5), 

                    %% make async request
                    dwight_core_req_server:send_async(self(), atom_to_list(Method), Host, 
                                                      Port, Headers, Path, Body),

                    {loop, Req6, #state{}, ?TIMEOUT, hibernate};
                undefined ->
                    %% No service found, return a 500 error
                    {ok, Req3} = cowboy_http_req:reply(500, [], <<"">>, Req2),
                    {ok, Req3, #state{}}
            end;
        undefined ->
            %% No domain found, return a 404
            {ok, Req3} = cowboy_http_req:reply(404, [], <<"">>, Req2),
            {ok, Req3, #state{}}
    end.

info({reply, Status, Headers, Body}, Req, State) ->
    {ok, Req2} = cowboy_http_req:set_resp_body(Body, Req),
    {ok, Req3} = cowboy_http_req:reply(Status, Headers, Req2),
    {ok, Req3, State};
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Req, _State) ->
    ok.

%%
%% Internal functions
%%
