%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@lenin>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2012 by Tristan Sloughter <tristan@lenin>
%%%-------------------------------------------------------------------
-module(dwight_core_req_server).

-behaviour(gen_server).

%% API
-export([start_link/2,
         close/1,
         send/6,
         send/7,
         send_async/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {key, client}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

send(Pid, Method, Host, Port, Headers, Path, Body) ->
    gen_server:call(Pid, {Method, Host, Port, Headers, Path, Body}).

send(Method, Host, Port, Headers, Path, Body) ->
    gen_server:call(get_client(Host, Port), {Method, Host, Port, Headers, Path, Body}).

send_async(From, Method, Host, Port, Headers, Path, Body) ->
    gen_server:cast(get_client(Host, Port), {From, Method, Host, Port, Headers, Path, Body}).

close(Pid) ->
    gen_server:call(Pid, close).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    {ok, Client} = cowboy_client:init([]),
    {ok, Client2} = cowboy_client:connect(cowboy_tcp_transport, Host, Port, Client),

    try
        gproc:reg({n, l, {Host, Port}}, self()),
        {ok, #state{key={Host, Port}, client=Client2}}
    catch
        error:badarg ->
            ignore
    end.

%%--------------------------------------------------------------------

handle_call(close, _From, State=#state{client=Client}) ->
    {ok, Client2} = cowboy_client:close(Client),
    {reply, ok, State#state{client=Client2}};
handle_call({Method, Host, Port, Headers, Path, Body}, _From, State=#state{client=Client}) ->    
    {ok, Status, RespHeaders, RespBody, Client2} = 
        send_request(Client, Method, Host, Port, Headers, Path, Body),

    {reply, {Status, RespHeaders, RespBody}, State#state{client=Client2}}.

%%--------------------------------------------------------------------

handle_cast({From, Method, Host, Port, Headers, Path, Body}, State=#state{client=Client}) ->    
    {ok, _T, Socket} = cowboy_client:transport(Client),
    io:format("Request on socket ~p~n", [Socket]),

    {ok, Status, RespHeaders, RespBody, Client2} = 
        send_request(Client, Method, Host, Port, Headers, Path, Body),

    From ! {reply, Status, RespHeaders, RespBody},

    {noreply, State#state{client=Client2}}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, #state{key={Host, Port}}) ->
    gproc:unreg({n, l, {Host, Port}}).

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_client(binary(), integer()) -> pid().
get_client(Host, Port) ->
    %% TODO: If list of pids is < list of backends, use new backend
    case gproc:lookup_pids({n, l, {Host, Port}}) of
        [] ->
            {ok, Pid} = dwight_core_req_sup:start_child(Host, Port),
            Pid;
        Pids ->
            lists:nth(random:uniform(length(Pids)), Pids)
    end.

send_request(Client, Method, Host, Port, Headers, Path, Body) ->
    Url = list_to_binary(lists:flatten(io_lib:format("http://~s:~p/~s", [Host, Port, Path]))),
    BinHeaders = [{cowboy_http_req:header_to_binary(H), V} || {H, V} <- Headers],

    case cowboy_client:request(Method, Url, BinHeaders, Client) of
        {ok, Client2} ->
            {ok, Status, Response, Client3} = cowboy_client:response(Client2),
            {ok, RespBody, Client4} = cowboy_client:response_body(Client3),   
            {ok, Status, Response, RespBody, Client4};
        {error, _Reason} ->
            {ok, Client2} = cowboy_client:close(Client),
            {ok, Client3} = cowboy_client:connect(cowboy_tcp_transport, Host, Port, Client2),
            send_request(Client3, Method, Host, Port, Headers, Path, Body)
    end.
