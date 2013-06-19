%% Feel free to use, reuse and abuse the code in this file.

%% @doc Stream handler for clock synchronizing.
-module(stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 1000).

init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
	TRef = erlang:send_after(?PERIOD, self(), refresh),
	{ok, Req, TRef}.

stream(<<"ping">>, Req, State) ->
	io:format("ping received~n"),
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
	io:format("stream received ~s~n", [Data]),
	{ok, Req, State}.

info(refresh, Req, _) ->
	TRef = erlang:send_after(?PERIOD, self(), refresh),
	DateTime = cowboy_clock:rfc1123(),
	io:format("clock refresh timeout: ~s~n", [DateTime]),
	{reply, DateTime, Req, TRef};
info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, TRef) ->
	io:format("bullet terminate~n"),
	erlang:cancel_timer(TRef),
	ok.
