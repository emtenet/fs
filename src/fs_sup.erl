%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Supervisor for the fs application.

%% vim:set softtabstop=4 shiftwidth=4 tabstop=4:

-module(fs_sup).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Web = web_specs(),
	Mysql = mysql_specs(),
    Processes = [Web | Mysql],
    Strategy = {one_for_one, 10, 10}, % max 10 die in 10 seconds
    {ok, {Strategy, lists:flatten(Processes)}}.

web_specs() ->
    WebConfig = [
		{ip, {0,0,0,0}},
		{port, 8443},
		{ssl, true},
		{ssl_opts, [
			{certfile, "/home/met/ssl/free-server.cert"},
			{keyfile, "/home/met/ssl/free-server.key"}
		]},
		{docroot, fs_deps:local_path(["priv", "www"])}
	],
    {fs_web
		,{fs_web, start, [WebConfig]}
		,permanent
		, 5000
		, worker
		, dynamic
	}.

mysql_logger(_Module, _Line, _Level, _FormatFun) ->
	ok.

mysql_specs() ->
	[	{mysql_a
			, {mysql, start, [mysql_pool, "localhost", undefined, "met", "", "fs", fun mysql_logger/4]}
			, permanent
			, 5000
			, worker
			, dynamic
		}
	,	{mysql_b
			, {mysql, connect, [mysql_pool, "localhost", undefined, "met", "", "fs", undefined, true, false]}
			, permanent
			, 5000
			, worker
			, dynamic
		}
	].

