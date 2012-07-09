%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2012 Tristan Sloughter
%%%----------------------------------------------------------------
-module(dwight_routes_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%% @private
-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                       ignore | {error, Reason::any()}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    %% Restart = permanent,
    %% Shutdown = 2000,
    %% Type = worker,

    %% AChild = {'AName', {'AModule', start_link, []},
    %%           Restart, Shutdown, Type, ['AModule']},
    
    populate_tables(),

    {ok, {SupFlags, []}}.

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

%%%====================================================================
%%% tests
%%%====================================================================
