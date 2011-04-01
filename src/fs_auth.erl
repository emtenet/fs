%% vim:set softtabstop=4 shiftwidth=4 tabstop=4:

-module(fs_auth).
-author("Michael Taylor <michael@emte.net.au>").

-export([request/1, authenticate/2]).

request(Req) ->
	case get_user(Req) of
		{ok, User} ->
			request(Req, User);
		{error, _User} ->
			redirect_to_login(Req)
	end.

request(Req, User) ->
	case Req:get(path) of
		"/" ->
			fs_recent:request(Req, User);
		"/recent/" ->
			fs_recent:request(Req, User);
		_ ->
			Req:not_found()
	end.

get_user(Req) ->
    Cookies = Req:parse_cookie(),
    User = proplists:get_value("user", Cookies),
    Pass = proplists:get_value("pass", Cookies),
	%error_logger:info_report(["authenticate", {path, Req:get(path)}, {cookies, Cookies}, {user, User}, {pass, Pass}]),
	authenticate(User, Pass).

authenticate("200", "Michael") -> {ok, "200"};
authenticate(User, _) -> {error, User}.

redirect_to_login(Req) ->
	Path = Req:get(path),
	Headers = [
		{"Location", "/login" ++ Path},
		{"Content-Type", "text/html"}
	],
	Body = [
		<<"<html>
<head>
</title>303 See Other</title>
</head>
<body>
<h1>Login Required</h1>
<p>Please login <a href=\"/login">>,
		Path,
		<<"\">here</a>.</p>
</body>
</html>">>],
	Req:respond({303, Headers, Body}).

