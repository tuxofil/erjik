%%% @doc
%%% Erjik main interface module.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash

-module(erjik).

%% escript entry point
-export([main/1]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% Type definitions
%% --------------------------------------------------------------------

-type parsed_args() :: [parsed_arg()].

-type parsed_arg() :: sasl | hup | {config, file:filename()}.

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec main(Args :: [string()]) -> no_return().
main(Args) ->
    case lists:member("-h", Args) orelse lists:member("--help", Args) of
        true ->
            usage();
        false ->
            nop
    end,
    %% use non-default TCP port number for Erlang port mapper
    %% to avoid interfere with normal Erlang nodes running on the host.
    true = os:putenv("ERL_EPMD_PORT", "4370"),
    %% bind Erlang port mapper only to loopback network interface
    _IgnoredStdout = os:cmd("epmd -address 127.0.0.1 -daemon"),
    %% parse and process command line arguments
    ParsedArgs = parse_args(Args),
    case proplists:is_defined(sasl, ParsedArgs) of
        true ->
            ok = application:start(sasl, permanent);
        false ->
            nop
    end,
    ConfigFilePath = proplists:get_value(config, ParsedArgs),
    {InstanceID, Cookie} = erjik_config_parser:read_daemon_cfg(ConfigFilePath),
    case proplists:is_defined(hup, ParsedArgs) of
        true ->
            MyID = list_to_atom(atom_to_list(InstanceID) ++ "_hupper"),
            ok = start_net_kernel(MyID, Cookie),
            pong = net_adm:ping(ErjikNode = node_fullname(InstanceID)),
            ok = rpc:call(ErjikNode, erjik_cfg, hup, []);
        false ->
            ok = start_net_kernel(InstanceID, Cookie),
            ok = application:load(?MODULE),
            ok = application:set_env(?MODULE, ?CFG_ERJIK_CONFIG, ConfigFilePath),
            ok = application:start(?MODULE, permanent),
            timer:sleep(infinity)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% @doc
-spec start_net_kernel(NodeShortName :: atom(), Cookie :: atom()) ->
                              ok | no_return().
start_net_kernel(NodeShortName, Cookie) ->
    case net_kernel:start([node_fullname(NodeShortName)]) of
        {ok, _Pid} ->
            true = erlang:set_cookie(node(), Cookie),
            ok;
        {error, Reason} ->
            err("Failed to start Erlang Distribution:~n\t~p", [Reason])
    end.

%% @doc
-spec node_fullname(NodeShortName :: atom()) -> node().
node_fullname(NodeShortName) ->
    list_to_atom(atom_to_list(NodeShortName) ++ "@127.0.0.1").

%% @doc Parse and process command line options and arguments.
-spec parse_args(Args :: [string()]) -> parsed_args() | no_return().
parse_args(Args) ->
    parse_args(Args, []).

-spec parse_args(Args :: [string()], Acc :: parsed_args()) ->
                        parsed_args() | no_return().
parse_args(["--sasl" | Tail], Acc) ->
    parse_args(Tail, [sasl | Acc]);
parse_args(["--hup" | Tail], Acc) ->
    parse_args(Tail, [hup | Acc]);
parse_args([ConfigFilePath], Acc) ->
    [{config, ConfigFilePath} | Acc];
parse_args(Other, _Acc) ->
    err("Unrecognized option or arguments: ~p", [Other]).

%% @doc Report something to stderr and halt.
-spec err(Format :: string(), Args :: list()) -> no_return().
err(Format, Args) ->
    ok = io:format(standard_error, "Error: " ++ Format ++ "\n", Args),
    halt(1).

%% @doc
-spec usage() -> no_return().
usage() ->
    N = escript:script_name(),
    io:format(
      "Erjik - Squid URL rewriter v.~s~n~n"
      "Usage:~n"
      "  ~s -h | --help~n"
      "\tShow this memo;~n"
      "  ~s /path/to/config~n"
      "\tStart Erjik;~n"
      "  ~s --hup /path/to/config~n"
      "\tSend reconfig signal to running Erjik instance.~n",
      [version(), N, N, N]),
    halt().

%% @doc Return epv version.
-spec version() -> iolist().
version() ->
    case application:load(?MODULE) of
        ok ->
            ok;
        {error, {already_loaded, ?MODULE}} ->
            ok
    end,
    {ok, Version} = application:get_key(?MODULE, vsn),
    Version.
