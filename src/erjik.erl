%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc erjik start/stop interface

-module(erjik).

-export(
   [start/0,
    stop/0,
    hup/0,
    ping/0
   ]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Load and start erjik.
%% @spec start() -> ok | {error, Reason}
%%     Reason = term()
start() ->
    application:start(?MODULE, permanent).

%% @doc Stop erjik.
%% @spec stop() -> ok | {error, Reason}
%%     Reason = term()
stop() ->
    application:stop(?MODULE).

%% @doc Connects to Erlang node with erjik running and makes
%%      him re-read its configuration file and reopen log file.
%% @spec hup() -> no_return()
hup() ->
    {ok, Node} = connect_server(),
    ok = rpc:call(Node, erjik_cfg, hup, []),
    halt(0).

%% @doc Checks if Erlang node with running erjik exists.
%% @spec ping() -> no_return()
ping() ->
    {ok, Node} = connect_server(),
    Apps = rpc:call(Node, application, which_applications, []),
    case [?MODULE || {?MODULE, _, _} <- Apps] of
        [_] -> halt(0);
        _ -> halt(1)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

connect_server() ->
    [_, Hostname] = string:tokens(atom_to_list(node()), "@"),
    ServerNode = list_to_atom("erjik@" ++ Hostname),
    pong = net_adm:ping(ServerNode),
    {ok, ServerNode}.

