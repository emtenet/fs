%% @author Michael Taylor <michael@emte.net.au>
%% @copyright 2011 Michael Taylor <michael@emte.net.au>

%% @doc Coordinator for the mysql pool

%% vim:set softtabstop=4 shiftwidth=4 tabstop=4:

-module(mysql_pool).
-author("Michael Taylor <michael@emte.net.au>").

%% External exports
-export([start_link/0, fetch/1, fetch/2]).

%% Internal exports
-export([logger/4]).

%% External API

%% @spec start_link() -> ServerRet
%%
%% @doc API for starting the mysql pool.
%%
start_link() ->
	start_pool(?MODULE, 10, "localhost", undefined, "met", "", "fs", undefined).

%% @spec fetch(Query::iolist()) -> query_result()
%%
%% @doc Send query to pool
%%
fetch(Query) ->
	mysql:fetch(?MODULE, Query).

%% @spec fetch(Query::iolist(), Timeout::integer()) -> query_result()
%%
%% @doc Send query to pool
%%
fetch(Query, Timeout) ->
	mysql:fetch(?MODULE, Query, Timeout).

%% Internal API

%% @spec start_pool(PoolId::atom(),
%%					Size::integer(),
%%					Host::string(),
%%					Port::integer(),
%%					User::string(),
%%					Password::string(),
%%					Database::string())
%%		->	{ok, Pid}
%%		|	{error, Err}
%%
%% @doc API for starting a mysql pool.
%%
start_pool(PoolId, Size, Host, Port, User, Password, Database, Encoding) when is_integer(Size), Size > 1 ->
	case mysql:start_link(PoolId, Host, Port, User, Password, Database, fun logger/4, Encoding) of
		{ok, Pid} ->
			extend_pool(Pid, PoolId, Size, Host, Port, User, Password, Database, Encoding);
		Error ->
			Error
	end.

%% @spec extend_pool(Pid, PoolId, Size, Host, Port, User, Password, Database, Encoding) -> {ok, Pid} | {error, Error}
%%
%% @doc If pool size > 1 then create the addition connections in the pool
%%
extend_pool(Pid, _, 1, _, _, _, _, _, _) ->
	{ok, Pid};
extend_pool(Pid, PoolId, Size, Host, Port, User, Password, Database, Encoding) ->
	case mysql:connect(PoolId, Host, Port, User, Password, Database, Encoding, true) of
		{ok, _} ->
			extend_pool(Pid, PoolId, Size-1, Host, Port, User, Password, Database, Encoding);
		Error ->
			stop_pool(Pid),
			Error
	end.

%% @spec logger(Module::atom(),
%%				Line::integer(),
%%				Level::level(),
%%				FormatFun::fun())
%%		-> any
%%
%%		level() = error | warn | normal | debug
%%
%% @doc Receive log messages from mysql
%%
logger(Module, Line, Level, FormatFun) ->
	case logger_method(Level) of
		ignore ->
			ok;
		Method ->
			{Format, Arguments} = FormatFun(),
			Source = io_lib:format("~w:~b", [Module, Line]),
			Message = io_lib:format(Format, Arguments),
			error_logger:Method([{source, Source}, Message])
	end.

%% @spec logger_method(Level::level()) -> atom()
%%
%% @doc Configure how detailed the logging shall be
%%
logger_method(error) -> error_report;
logger_method(warn) -> info_report;
logger_method(_) -> ignore.

%% @spec stop_pool(Pid::pid()) -> any
%%
%% @doc Stop the pool process
%%		Logic taken from supervisor behavior
%%
stop_pool(Pid) ->
	case link_to_monitor(Pid) of
		ok ->
			exit(Pid, shutdown),
			receive
				{'DOWN', _MRef, process, Pid, shutdown} ->
					ok;
				{'DOWN', _MRef, process, Pid, OtherReason} ->
					{error, OtherReason}
			after 5000 -> %% 5 seconds
				exit(Pid, kill),  %% Force termination.
				receive
					{'DOWN', _MRef, process, Pid, OtherReason} ->
						{error, OtherReason}
				end
			end;
		{error, Reason} ->      
			{error, Reason}
    end.

%% @spec link_to_monitor(Pid::pid()) -> ok | {error, Reason}
%%
%% @doc	Help function to stop_pool/1 switches from link to monitor approach
%%
link_to_monitor(Pid) ->
    %% Do the monitor operation first so that if the child dies 
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
		%% If the child dies before the unlik we must empty
		%% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
		{'EXIT', Pid, Reason} -> 
			receive 
				{'DOWN', _, process, Pid, _} ->
					{error, Reason}
			end
    after 0 -> 
	    %% If a naughty child did unlink and the child dies before
	    %% monitor the result will be that shutdown/2 receives a 
	    %% 'DOWN'-message with reason noproc.
	    %% If the child should die after the unlink there
	    %% will be a 'DOWN'-message with a correct reason
	    %% that will be handled in shutdown/2. 
	    ok   
    end.

