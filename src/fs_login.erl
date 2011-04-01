%% vim:set softtabstop=4 shiftwidth=4 tabstop=4:

-module(fs_login).
-author("Michael Taylor <michael@emte.net.au>").

-export([request/1, post/1]).

request(Req) ->
    Cookies = Req:parse_cookie(),
    User = proplists:get_value("user", Cookies),
	case User of
		undefined ->
			request(Req, "");
		_ ->
			request(Req, User)
	end.

request(Req, User) ->
	Path = Req:get(path),
    {ok, Body} = fs_login_dtl:render([{path, Path}, {user, User}]),
	Req:ok({"text/html", Body}).

post(Req) ->
	Post = Req:parse_post(),
	User = proplists:get_value("user", Post),
	Pass = proplists:get_value("pass", Post),
	%error_logger:info_report(["login post", {post, Post}, {user, User}, {pass, Pass}]),
	case fs_auth:authenticate(User, Pass) of
		{ok, User} ->
			redirect_to(Req, User, Pass);
		{error, User} ->
			request(Req, User)
	end.

redirect_to(Req, User, Pass) ->
    "/login" ++ Path = Req:get(path),
	CookieOptions = [
		{path, "/"},
		{max_age, 7*24*60*60},
		{secure, true},
		{httponly, true}
	],
    Headers = [
        {"Location", Path},
		mochiweb_cookies:cookie("user", User, CookieOptions),
		mochiweb_cookies:cookie("pass", Pass, CookieOptions),
        {"Content-Type", "text/html"}
    ],
    Body = [
        <<"<html>
<head>
</title>303 See Other</title>
</head>
<body>
<h1>Login Successfull</h1>
<p>Please continue <a href=\"">>,
        Path,
        <<"\">here</a>.</p>
</body>
</html>">>],
    Req:respond({303, Headers, Body}).

