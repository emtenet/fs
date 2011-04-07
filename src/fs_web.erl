%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for fs.

%% vim:set softtabstop=4 shiftwidth=4 tabstop=4:

-module(fs_web).
-author("Mochi Media <dev@mochimedia.com>").

% External exports
-export([start/0, stop/0]).

% Internal exports
-export([request/1]).

%% External API

start() ->
	case application:get_env(docroot) of
		undefined ->
			{ok, Application} = application:get_application(),
			application:set_env(Application, docroot, fs_deps:local_path(["priv", "www"]));
		_ ->
			ok
	end,
	Config = case application:get_env(web) of
		{ok, Config0} ->
			Config0;
		_ ->
			[]
	end,
	Config1 = proplists:delete(docroot, Config),
	Config2 = default(Config1, ip, {0,0,0,0}),
	Config3 = default(Config2, port, 8080),
    mochiweb_http:start([
		{name, ?MODULE}, 
		{loop, fun ?MODULE:request/1} 
	|	Config3
	]).

stop() ->
    mochiweb_http:stop(?MODULE).

%% Internal API

%% @spec request(Req::request()) -> response()
%%
%% @doc Attempt to create response to request.
%%		Catch exceptions and respond with an error message.
%%
request(Req) ->
	error_logger:info_report(["request", {method, Req:get(method)}, {path, Req:get(path)}]),
    try
		request(Req, Req:get(method), Req:get(path))
    catch
        Type:What ->
			Report = [
				"web request failed",
				{path, Req:get(path)},
				{type, Type},
				{what, What},
				{trace, erlang:get_stacktrace()}
			],
			error_logger:error_report(Report),
            Req:respond({
				500, 
				[{"Content-Type", "text/plain"}],
				"request failed, sorry\n"
			})
    end.

%% @spec request(Req::request(), Method::atom(), Path::string()) -> response()
%%
%% @doc	Route request processing based on method and path of request
%%
request(Req, 'GET', "/favicon.ico") -> serve_file(Req);
request(Req, 'GET', "/css/" ++ _) -> serve_file(Req);
request(Req, 'GET', "/js/" ++ _) -> serve_file(Req);
request(Req, 'GET', "/login/") -> fs_login:request(Req);
request(Req, 'GET', "/") -> fs_auth:request(Req);
request(Req, 'GET', _) -> Req:not_found();
request(Req, 'POST', "/login/") -> fs_login:post(Req);
request(Req, 'POST', _) -> Req:not_found();
request(Req, _, _) -> 
	Req:respond({501, [], []}).

serve_file(Req) ->
	"/" ++ Path = Req:get(path),
	{ok, DocRoot} = application:get_env(docroot),
	Req:serve_file(Path, DocRoot).

default(Config, Key, Value) ->
	case proplists:is_defined(Key, Config) of
		true ->
			Config;
		_ ->
			[{Key, Value} | Config]
	end.

