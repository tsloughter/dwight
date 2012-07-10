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
    dwight_core_utils:make_request(fun handle_request/8, Req, State).

terminate(_Req, _State) ->
    ok.

%%
%% Internal functions
%%

handle_request(Method, Host, Port, Headers, Path, Body, Req, State) ->
    {RespStatus, RespHeaders, RespBody} = 
        dwight_core_req_server:send(atom_to_list(Method), Host, Port, Headers, Path, Body),
    {ok, Req2} = cowboy_http_req:set_resp_body(RespBody, Req),
    {ok, Req3} = cowboy_http_req:reply(RespStatus, RespHeaders, Req2),

    {ok, Req3, State}.
