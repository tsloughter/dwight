%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@lenin>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2012 by Tristan Sloughter <tristan@lenin>
%%%-------------------------------------------------------------------
-module(dwight_routes_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------

init([]) ->
    populate_tables(),
    {ok, #state{}}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

populate_tables() ->
    {ok, Domains} = application:get_env(dwight_routes, domains),
    {ok, RouteIds} = application:get_env(dwight_routes, route_ids),
        
    create_table(domains, Domains),
    create_table(route_ids, RouteIds),

    ok.

create_table(Name, Elems) ->    
    ets:new(Name, [public, named_table]), 

    lists:foreach(fun(Elem) ->
                          ets:insert(Name, Elem)
                  end, Elems).

