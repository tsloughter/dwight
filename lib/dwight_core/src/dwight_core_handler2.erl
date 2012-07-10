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
    dwight_core_utils:make_request(fun handle_request/8, Req, #state{}).

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
handle_request(Method, Host, Port, Headers, Path, Body, Req, State) ->
    dwight_core_req_server:send_async(self(), atom_to_list(Method), Host, 
                                      Port, Headers, Path, Body),

    {loop, Req, State, ?TIMEOUT, hibernate}.
