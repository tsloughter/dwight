%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@lenin>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2012 by Tristan Sloughter <tristan@lenin>
%%%-------------------------------------------------------------------
-module(dwight_test).

%% API
-export([run/0]).

%%%===================================================================
%%% API
%%%===================================================================

run() ->
    test_stream_once("http://localhost:8080", get, [], 5000).

test_stream_once(Url, Method, Options, Timeout) ->
    case ibrowse:send_req(Url, [], Method, [], [{stream_to, {self(), once}} | Options], Timeout) of
	{ibrowse_req_id, Req_id} ->
	    case ibrowse:stream_next(Req_id) of
		ok ->
		    test_stream_once(Req_id);
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.

test_stream_once(Req_id) ->
    receive
	{ibrowse_async_headers, Req_id, StatCode, Headers} ->
	    io:format("Recvd headers~n~p~n", [{ibrowse_async_headers, Req_id, StatCode, Headers}]),
	    case ibrowse:stream_next(Req_id) of
		ok ->
		    test_stream_once(Req_id);
		Err ->
		    Err
	    end;
	{ibrowse_async_response, Req_id, {error, Err}} ->
	    io:format("Recvd error: ~p~n", [Err]);
	{ibrowse_async_response, Req_id, Body_1} ->
	    io:format("Recvd body part: ~n~p~n", [{ibrowse_async_response, Req_id, Body_1}]),
	    case ibrowse:stream_next(Req_id) of
		ok ->
		    test_stream_once(Req_id);
		Err ->
		    Err
	    end;
	{ibrowse_async_response_end, Req_id} ->
	    ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
