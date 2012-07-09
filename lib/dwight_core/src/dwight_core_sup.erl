%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2012 Tristan Sloughter
%%%----------------------------------------------------------------
-module(dwight_core_sup).
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

    Dispatch = [{'_', [{[], dwight_core_handler, []}]}],    

    ChildSpec = cowboy:child_spec(dwight_cowboy, 100, cowboy_tcp_transport, 
                                  [{port, 8080}], cowboy_http_protocol, [{dispatch, Dispatch}]),

    populate_tables(),

    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

populate_tables() ->
    {ok, Domains} = {ok, [{<<"localhost">>, <<"abc">>}]}, %application:get_env(dwight_core, domains),
    {ok, RouteIds} = {ok, [{<<"abc">>, <<"localhost">>, 7999}]}, %application:get_env(dwight_core, route_ids),
        
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
