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
         send/5,
         send/6,
         send_async/6]).

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

send(Pid, Method, Host, Port, Path, Body) ->
    gen_server:call(Pid, {Method, Host, Port, Path, Body}).

send(Method, Host, Port, Path, Body) ->
    gen_server:call(get_client(Host, Port), {Method, Host, Port, Path, Body}).

send_async(From, Method, Host, Port, Path, Body) ->
    gen_server:cast(get_client(Host, Port), {From, Method, Host, Port, Path, Body}).

close(Pid) ->
    gen_server:call(Pid, close).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    {ok, Client} = cowboy_client:init([]),
    {ok, Client2} = cowboy_client:connect(cowboy_tcp_transport, Host, Port, Client),
    gproc:reg({n, l, {Host, Port}}, self()),
    {ok, #state{key={Host, Port}, client=Client2}}.

%%--------------------------------------------------------------------

handle_call(close, _From, State=#state{client=Client}) ->
    {ok, Client2} = cowboy_client:close(Client),
    {reply, ok, State#state{client=Client2}};
handle_call({Method, Host, Port, Path, Body}, _From, State=#state{client=Client}) ->    
    {ok, Status, Headers, RespBody, Client2} = 
        send_request(Client, Method, Host, Port, Path, Body),

    {reply, {Status, Headers, RespBody}, State#state{client=Client2}}.

%%--------------------------------------------------------------------

handle_cast({From, Method, Host, Port, Path, Body}, State=#state{client=Client}) ->    
    {ok, Status, Headers, RespBody, Client2} = 
        send_request(Client, Method, Host, Port, Path, Body),

    From ! {reply, Status, Headers, RespBody},

    {noreply, State#state{client=Client2}}.

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

-spec get_client(binary(), integer()) -> pid().
get_client(Host, Port) ->
    case gproc:where({n, l, {Host, Port}}) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            {ok, Pid} = dwight_core_req_sup:start_child(Host, Port),
            Pid
    end.

send_request(Client, Method, Host, Port, Path, _Body) ->
    Url = list_to_binary(lists:flatten(io_lib:format("http://~s:~p/~s", [Host, Port, Path]))),
    {ok, Client2} = cowboy_client:request(Method, Url, Client),
    {ok, Status, Response, Client3} = cowboy_client:response(Client2),

    {ok, RespBody, Client4} = cowboy_client:response_body(Client3),
    
    {ok, Status, Response, RespBody, Client4}.
