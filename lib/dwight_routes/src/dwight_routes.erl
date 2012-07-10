%%% @author Tristan Sloughter <>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2012 by Tristan Sloughter <>
-module(dwight_routes).

-export([find_route_id/1, 
         find_service/1]).

-spec find_route_id(binary()) -> {ok, binary()} | undefined.
find_route_id(Domain) ->
    case ets:lookup(domains, Domain) of
        [{Domain, RouteId} | _] ->
            {ok, RouteId};
        [] ->
            undefined
    end.

-spec find_service(binary()) -> {ok, binary()} | undefined.
find_service(RouteId) ->
    case ets:lookup(route_ids, RouteId) of
        [{RouteId, Host, Port} | _] ->
            {ok, Host, Port};
        [] ->
            undefined
    end.
