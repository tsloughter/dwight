%%% @author Tristan Sloughter <tristan@lenin>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2012 by Tristan Sloughter <tristan@lenin>

-module(dwight_core_utils).

-export([make_request/3]).

make_request(SendReqFun, Req, State) ->
    {Host, Req2} = cowboy_http_req:raw_host(Req),   
    case dwight_routes:find_route_id(Host) of
        {ok, RouteId} ->    
            case dwight_routes:find_service(RouteId) of
                {ok, Service, Port} ->
                    {Method, Req3} = cowboy_http_req:method(Req2), 
                    {Path, Req4} = cowboy_http_req:path(Req3), 
                    {Headers, Req5} = cowboy_http_req:headers(Req4), 
   
                    case cowboy_http_req:has_body(Req5) of
                        {true, Req6} ->
                            {ok, Body, Req7} = cowboy_http_req:body(Req6),                     
                            SendReqFun(Method, Service, Port, Headers, Path, Req7, State);
                        {false, Req6} ->
                            SendReqFun(Method, Service, Port, Headers, Path, Req6, State)
                    end;
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
