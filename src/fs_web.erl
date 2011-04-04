%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for fs.

%% vim:set softtabstop=4 shiftwidth=4 tabstop=4:

-module(fs_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/0, stop/0, request/1]).

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

request(Req) ->
	error_logger:info_report(["request", {method, Req:get(method)}, {path, Req:get(path)}]),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
					"favicon.ico" ->
						Req:serve_file(Path, docroot());
					"css/" ++ _ ->
						Req:serve_file(Path, docroot());
					"js/" ++ _ ->
						Req:serve_file(Path, docroot());
					"login/" ++ _ ->
						fs_login:request(Req);
					_ ->
						fs_auth:request(Req)
                end;
            'POST' ->
                case Path of
					"login/" ++ _ ->
						fs_login:post(Req);
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

docroot() ->
	{ok, DocRoot} = application:get_env(docroot),
	DocRoot.

default(Config, Key, Value) ->
	case proplists:is_defined(Key, Config) of
		true ->
			Config;
		_ ->
			[{Key, Value} | Config]
	end.

