%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc fs.

-module(fs).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

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

