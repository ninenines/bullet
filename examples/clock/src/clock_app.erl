%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(clock_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_handler, []},
			{"/bullet", bullet_handler, [{handler, stream_handler}]},
			{"/static/[...]", cowboy_static, [
				{directory, {priv_dir, bullet, []}},
				{mimetypes, [
					{<<".js">>, [<<"application/javascript">>]}
				]}
			]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100,
		[{port, 8080}], [{env, [{dispatch, Dispatch}]}]
	),
	clock_sup:start_link().

stop(_State) ->
	ok.
