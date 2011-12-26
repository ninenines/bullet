Bullet
======

Bullet is a Cowboy handler and associated Javascript library for
maintaining a persistent connection between a client and a server.

Bullet abstracts a general transport protocol familiar to WebSockets, and
is equipped with several "fallback" transports. Bullet will automatically
use one of these when the browser used is not able to support WebSockets.

A common interface is defined for both client and server-side to easily
facilitate the handling of such connections. Bullet additionally takes care
of reconnecting automatically whenever a connection is lost, and also
provides an optional heartbeat which is managed on the client side.

Today Bullet only supports websocket and long-polling transports.

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
indicates what type of connection we have. When Active == false,
we have a temporary connection that only allows one reply before
terminating. When Active == true, the connection allows any number
of replies. You can use this information to inform your session
process that it should send only 1 message, in the case of
Active == false, or that it can send messages whenever in the
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
	var bullet = $.bullet(stream);
	bullet.onopen = function(){
		console.log('WebSocket: opened');
	};
	bullet.onclose = function(){
		console.log('WebSocket: closed');
	};
	bullet.onmessage = function(e){
		alert(e.data);
	};
	bullet.onheartbeat = function(){
		bullet.send('ping');
	}
});
```

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
