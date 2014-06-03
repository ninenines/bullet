%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(bullet_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([handle/2]).
-export([info/3]).
-export([terminate/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
	handler :: module(),
	handler_state :: term(),
	% poll or eventsource for GET requests
	get_mode :: 'undefined' | 'poll' | 'eventsource'
}).

-define(TIMEOUT, 60000). %% @todo Configurable.

%% HTTP.

init(Transport, Req, Opts) ->
	case cowboy_req:header(<<"upgrade">>, Req) of
		{undefined, Req2} ->
			{Method, Req3} = cowboy_req:method(Req2),
			init(Transport, Req3, Opts, Method);
		{Bin, Req2} when is_binary(Bin) ->
			case cowboy_bstr:to_lower(Bin) of
				<<"websocket">> ->
					{upgrade, protocol, cowboy_websocket};
				_Any ->
					{ok, Req3} = cowboy_req:reply(501, [], [], Req2),
					{shutdown, Req3, undefined}
			end
	end.

init(Transport, Req, Opts, <<"GET">>) ->
	{handler, Handler} = lists:keyfind(handler, 1, Opts),
	State = #state{handler=Handler},
	{GetMode, Req2} = get_mode(Req),
	Active = case GetMode of
		poll -> once;
		eventsource -> true
	end,
	case Handler:init(Transport, Req2, Opts, Active) of
		{ok, Req3, HandlerState} ->
			{ok, Req4} = start_get_mode(GetMode, Req3),
			Req5 = cowboy_req:compact(Req4),
			{loop, Req5, State#state{handler_state=HandlerState,
				get_mode=GetMode}, ?TIMEOUT, hibernate};
		{shutdown, Req3, HandlerState} ->
			{shutdown, Req3, State#state{handler_state=HandlerState}}
	end;
init(Transport, Req, Opts, <<"POST">>) ->
	{handler, Handler} = lists:keyfind(handler, 1, Opts),
	State = #state{handler=Handler},
	case Handler:init(Transport, Req, Opts, false) of
		{ok, Req2, HandlerState} ->
			{ok, Req2, State#state{handler_state=HandlerState}};
		{shutdown, Req2, HandlerState} ->
			{shutdown, Req2, State#state{handler_state=HandlerState}}
	end;
init(_Transport, Req, _Opts, _Method) ->
	{ok, Req2} = cowboy_req:reply(405, [], [], Req),
	{shutdown, Req2, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	handle(Req2, State, Method).

handle(Req, State=#state{handler=Handler, handler_state=HandlerState},
		<<"POST">>) ->
	case cowboy_req:body(Req) of
		{ok, Data, Req2} ->
			case Handler:stream(Data, Req2, HandlerState) of
				{ok, Req3, HandlerState2} ->
					{ok, Req3, State#state{handler_state=HandlerState2}};
				{reply, Reply, Req3, HandlerState2} ->
					{ok, Req4} = cowboy_req:reply(200, [], Reply, Req3),
					{ok, Req4, State#state{handler_state=HandlerState2}}
			end;
		{error, _} ->
			%% An error occurred, stop there.
			{ok, Req, State}
	end.

info(Message, Req,
		State=#state{get_mode=GetMode, handler=Handler,
		handler_state=HandlerState}) ->
	case Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			{loop, Req2, State#state{handler_state=HandlerState2}, hibernate};
		{reply, Data, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			case reply_get_mode(GetMode, Data, Req2) of
				{ok, Req3} ->
					{ok, Req3, State2};
				{loop, Req3} ->
					{loop, Req3, State2, hibernate}
			end
	end.

terminate(_Reason, _Req, undefined) ->
	ok;
terminate(_Reason, Req, #state{handler=Handler, handler_state=HandlerState}) ->
	Handler:terminate(Req, HandlerState).

%% Websocket.

websocket_init(Transport, Req, Opts) ->
	{handler, Handler} = lists:keyfind(handler, 1, Opts),
	State = #state{handler=Handler},
	case Handler:init(Transport, Req, Opts, true) of
		{ok, Req2, HandlerState} ->
			Req3 = cowboy_req:compact(Req2),
			{ok, Req3, State#state{handler_state=HandlerState},
				?TIMEOUT, hibernate};
		{shutdown, Req2, _HandlerState} ->
			{shutdown, Req2}
	end.

websocket_handle({text, Data}, Req,
		State=#state{handler=Handler, handler_state=HandlerState}) ->
	case Handler:stream(Data, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			{ok, Req2, State#state{handler_state=HandlerState2}, hibernate};
		{reply, Reply, Req2, HandlerState2} ->
			{reply, {text, Reply}, Req2,
				State#state{handler_state=HandlerState2}, hibernate}
	end;
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_info(Info, Req, State=#state{
		handler=Handler, handler_state=HandlerState}) ->
	case Handler:info(Info, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			{ok, Req2, State#state{handler_state=HandlerState2}, hibernate};
		{reply, Reply, Req2, HandlerState2} ->
			{reply, {text, Reply}, Req2,
				State#state{handler_state=HandlerState2}, hibernate}
	end.

websocket_terminate(_Reason, Req,
		#state{handler=Handler, handler_state=HandlerState}) ->
	Handler:terminate(Req, HandlerState).

%% Eventsource and poll utilities

get_mode(Req) ->
	case cowboy_req:parse_header(<<"accept">>, Req) of
		{ok, Accepts, Req2} ->
			get_mode(Accepts, Req2);
		_ ->
			{poll, Req}
	end.

get_mode([{{<<"text">>, <<"event-stream">>, _}, _, _}|_], Req) ->
	{eventsource, Req};
get_mode([_|Accepts], Req) ->
	get_mode(Accepts, Req);
get_mode([], Req) ->
	{poll, Req}.

start_get_mode(poll, Req) ->
	{ok, Req};
start_get_mode(eventsource, Req) ->
	Headers = [{<<"content-type">>, <<"text/event-stream">>}],
	{ok, _} = cowboy_req:chunked_reply(200, Headers, Req).

reply_get_mode(poll, Data, Req) ->
	{ok, _} = cowboy_req:reply(200, [], Data, Req);
reply_get_mode(eventsource, Data, Req) ->
	Bin = iolist_to_binary(Data),
	Event = [[<<"data: ">>, Line, <<"\n">>] ||
		Line <- binary:split(Bin, [<<"\r\n">>, <<"\r">>, <<"\n">>], [global])],
	ok = cowboy_req:chunk([Event, <<"\n">>], Req),
	{loop, Req}.
