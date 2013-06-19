%% Feel free to use, reuse and abuse the code in this file.

%% @doc Main page of the clock application.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Body = <<"
<!DOCTYPE html>
<html lang=\"en\">
<head>
	<meta charset=\"utf-8\">
	<title>Bullet Clock</title>
</head>

<body>
	<p><input type=\"checkbox\" checked=\"yes\" id=\"enable_best\"></input>
		Current time (best source): <span id=\"time_best\">unknown</span>
		<span> </span><span id=\"status_best\">unknown</span></p>
	<p><input type=\"checkbox\" checked=\"yes\" id=\"enable_websocket\"></input>
		Current time (websocket only): <span id=\"time_websocket\">unknown</span>
		<span> </span><span id=\"status_websocket\">unknown</span></p>
	<p><input type=\"checkbox\" checked=\"yes\" id=\"enable_eventsource\"></input>
		Current time (eventsource only): <span id=\"time_eventsource\">unknown</span>
		<span> </span><span id=\"status_eventsource\">unknown</span></p>
	<p><input type=\"checkbox\" checked=\"yes\" id=\"enable_polling\"></input>
		Current time (polling only): <span id=\"time_polling\">unknown</span>
		<span> </span><span id=\"status_polling\">unknown</span></p>

	<script
		src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js\">
	</script>
	<script src=\"/static/bullet.js\"></script>
	<script type=\"text/javascript\">
// <![CDATA[
$(document).ready(function(){
	var start = function(name, options) {
		var bullet;
		var open = function(){
			bullet = $.bullet('ws://localhost:8080/bullet', options);
			bullet.onopen = function(){
				$('#status_' + name).text('online');
			};
			bullet.onclose = bullet.ondisconnect = function(){
				$('#status_' + name).text('offline');
			};
			bullet.onmessage = function(e){
				if (e.data != 'pong'){
					$('#time_' + name).text(e.data);
				}
			};
			bullet.onheartbeat = function(){
				console.log('ping: ' + name);
				bullet.send('ping: ' + name);
			};
		}
		open();
		$('#enable_' + name).on('change', function(){
			if (this.checked){
				open();
			} else{
				bullet.close();
				bullet = null;
			}
		});
	};

	start('best', {});
	start('websocket', {'disableEventSource': true,
		'disableXHRPolling': true});
	start('eventsource', {'disableWebSocket': true,
		'disableXHRPolling': true});
	start('polling', {'disableWebSocket': true,
		'disableEventSource': true});
});
// ]]>
	</script>
</body>
</html>
">>,
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}],
		Body, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
