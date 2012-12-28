%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc erjik start/stop interface

-module(erjik).

-export(
   [start/0,
    stop/0,
    hup/0,
    ping/0,
    shutdown/1,
    stop_remote/0
   ]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Load and start erjik.
%% @spec start() -> ok | {error, Reason}
%%     Reason = term()
start() ->
    ok = erjik_error_logger:install(),
    application:start(?MODULE, permanent).

%% @doc Stop erjik.
%% @spec stop() -> ok | {error, Reason}
%%     Reason = term()
stop() ->
    application:stop(?MODULE).

%% @doc Connects to Erlang node with erjik running and makes
%%      him re-read its configuration file and reopen log file.
%% @spec hup() -> no_return()
-spec hup() -> no_return().
hup() ->
    {ok, Node} = connect_server(),
    ok = rpc:call(Node, erjik_cfg, hup, []),
    halt(0).

%% @doc Checks if Erlang node with running erjik exists.
%% @spec ping() -> no_return()
-spec ping() -> no_return().
ping() ->
    {ok, Node} = connect_server(),
    Apps = rpc:call(Node, application, which_applications, []),
    case [?MODULE || {?MODULE, _, _} <- Apps] of
        [_] -> halt(0);
        _ -> halt(1)
    end.

%% @doc Flush log and terminate.
%% @spec shutdown(ExitCode) -> no_return()
%%     ExitCode = integer()
-spec shutdown(ExitCode::non_neg_integer()) -> no_return().
shutdown(ExitCode) ->
    ?loginf("~w> terminating (exitcode ~w)", [?MODULE, ExitCode]),
    ok = erjik_log:flush(),
    halt(ExitCode).

%% @doc Stop remote node with erjik running.
%% @spec stop_remote() -> no_return()
-spec stop_remote() -> no_return().
stop_remote() ->
    {ok, Node} = connect_server(),
    catch rpc:call(Node, ?MODULE, shutdown, [0]),
    halt(0).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

connect_server() ->
    [_, Hostname] = string:tokens(atom_to_list(node()), "@"),
    ServerNode = list_to_atom("erjik@" ++ Hostname),
    pong = net_adm:ping(ServerNode),
    {ok, ServerNode}.

