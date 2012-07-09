%% Feel free to use, reuse and abuse the code in this file.

%% @doc Main page of the clock application.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

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
	<p>Connection status: <span id=\"status\">bullet not started</span></p>
	<p>Current time: <span id=\"time\">unknown</span></p>

	<script
		src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js\">
	</script>
	<script src=\"/static/bullet.js\"></script>
	<script type=\"text/javascript\">
// <![CDATA[
$(document).ready(function(){
	var bullet = $.bullet('ws://localhost:8080/bullet');
	bullet.onopen = function(){
		$('#status').text('online');
	};
	bullet.ondisconnect = function(){
		$('#status').text('offline');
	};
	bullet.onmessage = function(e){
		if (e.data != 'pong'){
			$('#time').text(e.data);
		}
	};
	bullet.onheartbeat = function(){
		console.log('ping');
		bullet.send('ping');
	}
});
// ]]>
	</script>
</body>
</html>
">>,
	{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],
		Body, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.
