%% @author Mochi Media <dev@mochimedia.com>
%% @copyright fs Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the fs application.

-module(fs_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for fs.
start(_Type, _StartArgs) ->
    fs_deps:ensure(),
    fs_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for fs.
stop(_State) ->
    ok.
