%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc fs.

-module(fs).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).
-export([stop_dev/0, stop_live/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the fs server.
start() ->
    fs_deps:ensure(),
    ensure_started(crypto),
    application:start(fs).


%% @spec stop() -> ok
%% @doc Stop the fs server.
stop() ->
    application:stop(fs).

%% @spec stop_dev() -> ok
%% @doc Stop the fs server on node fs_dev
stop_dev() ->
    rpc:call('fs_dev@dagonet', init, stop, []),
    init:stop().

%% @spec stop_live() -> ok
%% @doc Stop the fs server on node fs
stop_live() ->
    rpc:call('fs@dagonet', init, stop, []),
    init:stop().
