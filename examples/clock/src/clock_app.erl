%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(clock_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{[], toppage_handler, []},
			{[<<"bullet">>], bullet_handler, [{handler, stream_handler}]},
			{[<<"static">>, '...'], cowboy_http_static, [
				{directory, {priv_dir, bullet, []}},
				{mimetypes, [
					{<<".js">>, [<<"application/javascript">>]}
				]}
			]}
		]}
	],
	{ok, _} = cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	clock_sup:start_link().

stop(_State) ->
	ok.
