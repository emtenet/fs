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
    Processes = [
		web_spec(),
		mysql_spec()
	],
    Strategy = {one_for_one, 10, 10}, % max 10 die in 10 seconds
    {ok, {Strategy, Processes}}.

web_spec() ->
    {fs_web
		,{fs_web, start, []}
		,permanent
		, 5000
		, worker
		, dynamic
	}.

mysql_spec() ->
	{mysql
		, {mysql_pool, start_link, []}
		, permanent
		, 5000
		, worker
		, dynamic
	}.

