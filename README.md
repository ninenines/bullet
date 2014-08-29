Bullet
======

Bullet is a Cowboy handler and associated Javascript library for
maintaining a persistent connection between a client and a server.

Bullet abstracts a general transport protocol similar to WebSockets.
Bullet will use a WebSocket if possible but will fallback to other
transports when necessary. If the client supports EventSource
connections then Bullet will use EventSource to send messages from the
server to the client and XHR for messages from the client to the
server. If EventSource is not available then Bullet will use XHR for
both directions, using long-polling for server-to-client messages.

A common interface is defined for both client and server to easily
facilitate the handling of such connections. Bullet additionally takes care
of reconnecting automatically whenever a connection is lost, and also
provides an optional heartbeat which is managed on the client side.

Dispatch options
----------------

Similar to any other handler, you need to setup the dispatch list before
you can access your Bullet handlers. Bullet itself is a Cowboy HTTP
handler that translates some of the lower-level functions into a
simplified higher-level interface.

The dispatch options for a Bullet handler looks as follow:

``` erlang
{[<<"path">>, <<"to">>, <<"bullet">>], bullet_handler,
	[{handler, my_stream}]}
```

Simply define this in your dispatch list and your handler will be
available and handled by Bullet properly.

The third element in the tuple ([{handler, my_stream}]) will be passed 
to init/4 as Opts, you can add your own options and get them using
lists:keyfind, for example if we define our handler as:

``` erlang
{[<<"path">>, <<"to">>, <<"bullet">>], bullet_handler,
	[{handler, my_stream}, {channel, "my channel"}]}
```

you can retrieve the channel value as follows:

``` erlang
init(_Transport, Req, Opts, _Active) ->
	{channel, Channel} = lists:keyfind(channel, 1, Opts),
	{ok, Req, #state{channel=Channel}}.
```

Cowboy handler
--------------

Similar to websocket handlers, you need to define 4 functions.
A very simple bullet handler would look like the following:

``` erlang
-module(stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
	{ok, Req, undefined_state}.

stream(Data, Req, State) ->
	{reply, Data, Req, State}.

info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(_Req, _State) ->
	ok.
```

Of note is that the init/4 and terminate/2 functions are called
everytime a connection is made or closed, respectively, which can
happen many times over the course of a bullet connection's life,
as Bullet will reconnect everytime it detects a disconnection.

Note that you do not need to handle a heartbeat server-side, it
is automatically done when needed by the Bullet client as explained
later in this document.

You might have noticed the odd Active argument to init/4. It
indicates what type of connection we have. When Active == once,
we have a temporary connection that only allows one reply before
terminating. When Active == true, the connection allows any number
of replies. You can use this information to inform your session
process that it should send only 1 message, in the case of
Active == once, or that it can send messages whenever in the
case of Active == true.

You would typically use init/4 to inform your session process
that it can send you messages. In the same manner you can use
terminate/2 to inform it that the connection is going down.

Bullet handlers should only contain transport related code,
logic should be done in your session process if any, or other
parts of your application. Bullet processes should be considered
temporary as you never know when a connection is going to close
and therefore lose your State.

Client-side javascript
----------------------

Bullet requires the jQuery library to be used. Initializing a
bullet connection is quite simple and can be done directly from
a document.ready function like this:

``` js
$(document).ready(function(){
	var bullet = $.bullet('ws://localhost/path/to/bullet/handler');
	bullet.onopen = function(){
		console.log('bullet: opened');
	};
	bullet.ondisconnect = function(){
		console.log('bullet: disconnected');
	};
	bullet.onclose = function(){
		console.log('bullet: closed');
	};
	bullet.onmessage = function(e){
		alert(e.data);
	};
	bullet.onheartbeat = function(){
		bullet.send('ping');
	}
});
```

Use the WebSocket (ws:) form for your bullet URLs and Bullet
will change the URL as needed for non-WebSocket transports.

Use the standard (http:) form for your bullet URLs and Bullet
will only try non-WebSocket transports.

The `$.bullet` function takes an optional second 'options' object.
The following properties are supported:

| Name                   | Default | Description                         |
| ---------------------- | --------|------------------------------------ |
| disableWebSocket       | false   | Never make WebSocket connections.   |
| disableEventSource     | false   | Never make EventSource connections. |
| disableXHRPolling      | false   | Never fallback to XHR long polling. |

Note that if EventSource is enabled and chosen as the underlying
transport, XHR will be used for client-to-server messages.

Bullet works especially well when it is used to send JSON data
formatted with the jQuery JSON plugin.

``` js
bullet.send($.toJSON({type: 'event', data: 'hats!'}));
```

When receiving JSON you would typically receive a list of events,
in which case your onmessage handler can look like this, assuming
you previously defined a handlers function array for all your events:

``` js
	bullet.onmessage = function(e){
		var obj = $.parseJSON(e.data);
		for (i = 0; i < obj.length; i++){
			handlers[obj[i].type](obj[i]);
		}
	};
```
