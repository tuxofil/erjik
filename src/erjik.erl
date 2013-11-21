%%% @doc
%%% Erjik main interface module.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash

-module(erjik).

%% API exports
-export([shutdown/1]).

%% escript entry point
-export([main/1]).

-include("erjik.hrl").

%% command line-related configuration options
-define(CFG_DAEMON_ID, daemon_id).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-spec main(Args :: [string()]) -> no_return().
main(Args) ->
    IsHupMode = lists:member("--hup", Args),
    ok = load_app(),
    ok = parse_args(Args),
    ErjikNodeName =
        case application:get_env(?MODULE, ?CFG_DAEMON_ID) of
            {ok, ErjikNodeName0} ->
                ErjikNodeName0;
            undefined ->
                ?MODULE
        end,
    _IgnoredStdout = os:cmd("epmd -daemon"),
    if IsHupMode ->
            do_hup(ErjikNodeName);
       true ->
            do_start(ErjikNodeName)
    end.

%% @private
%% @doc Flush log and terminate.
-spec shutdown(ExitCode :: non_neg_integer()) -> no_return().
shutdown(ExitCode) ->
    ?loginf("~w> terminating (exitcode ~w)", [?MODULE, ExitCode]),
    ok = erjik_log:flush(),
    halt(ExitCode).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% @doc
-spec do_hup(ErjikNodeName :: atom()) -> ok.
do_hup(ErjikNodeName) ->
    ok = start_net_kernel(erjik_hupper),
    ok = call(ErjikNodeName, erjik_cfg, hup, []).

%% @doc
-spec do_start(ErjikNodeName :: atom()) -> no_return().
do_start(ErjikNodeName) ->
    ok = start_net_kernel(ErjikNodeName),
    ok = start_app(),
    timer:sleep(infinity).

%% @doc
-spec start_net_kernel(NodeShortName :: atom()) -> ok.
start_net_kernel(NodeShortName) ->
    case net_kernel:start([node_fullname(NodeShortName)]) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            err("Failed to start Erlang Distribution:~n\t~p", [Reason])
    end.

%% @doc
-spec call(NodeShortName :: atom(), Module :: atom(),
           Function :: atom(), Args :: list()) ->
                  RpcCallResult :: any() | {badrpc, Reason :: any()}.
call(NodeShortName, Module, Function, Args) ->
    rpc:call(connect_to_erjik(NodeShortName), Module, Function, Args).

%% @doc
-spec connect_to_erjik(NodeShortName :: atom()) -> ErjikNode :: node().
connect_to_erjik(NodeShortName) ->
    pong = net_adm:ping(ErjikNode = node_fullname(NodeShortName)),
    ErjikNode.

%% @doc
-spec node_fullname(NodeShortName :: atom()) -> node().
node_fullname(NodeShortName) ->
    list_to_atom(atom_to_list(NodeShortName) ++ "@127.0.0.1").

%% @doc
-spec usage() -> no_return().
usage() ->
    N = escript:script_name(),
    io:format(
      "Erjik - Squid URL rewriter v.~s~n~n"
      "Usage:~n"
      "  ~s -h | --help~n"
      "\tShow this memo;~n"
      "  ~s [--sasl|--id ID] /path/to/config~n"
      "\tStart Erjik;~n"
      "  ~s [--id ID] --hup~n"
      "\tSend reconfig signal to running Erjik instance.~n"
      "~n"
      "Options description:~n"
      "\t--sasl      - start SASL (only for debugging);~n"
      "\t--id DaemonID - Default is 'erjik'. Set it to something else if~n"
      "\t              you want to run several instances of the program~n"
      "\t              at the same time.~n"
      "~n",
      [version(), N, N, N]),
    halt().

%% @doc Parse and process command line options and arguments.
-spec parse_args(Args :: [string()]) -> ok | no_return().
parse_args([]) ->
    case get(no_args_needed) of
        true ->
            ok;
        _ ->
            usage()
    end;
parse_args(["-h" | _]) ->
    usage();
parse_args(["--help" | _]) ->
    usage();
parse_args(["--sasl" | Tail]) ->
    ensure_app_started(sasl),
    parse_args(Tail);
parse_args(["--hup" | Tail]) ->
    put(no_args_needed, true),
    parse_args(Tail);
parse_args(["--id", DaemonID | Tail]) ->
    put(?CFG_DAEMON_ID, list_to_atom(DaemonID)),
    parse_args(Tail);
parse_args([ConfigFilePath]) ->
    set_env(?CFG_ERJIK_CONFIG, ConfigFilePath);
parse_args(Other) ->
    err("Unrecognized option or arguments: ~p", [Other]).

%% @doc Set environment for application.
-spec set_env(Key :: atom(), Value :: any()) -> ok.
set_env(Key, Value) ->
    application:set_env(?MODULE, Key, Value).

%% @doc Report something to stderr and halt.
-spec err(Format :: string(), Args :: list()) -> no_return().
err(Format, Args) ->
    ok = io:format(standard_error, "Error: " ++ Format ++ "\n", Args),
    halt(1).

%% @doc
-spec load_app() -> ok | no_return().
load_app() ->
    case application:load(?MODULE) of
        ok ->
            ok;
        {error, {already_loaded, ?MODULE}} ->
            ok;
        {error, Reason} ->
            err("Failed to load application: ~p", [Reason])
    end.

%% @doc
-spec start_app() -> ok | no_return().
start_app() ->
    ensure_app_started(?MODULE).

%% @doc Start application if not started yet.
-spec ensure_app_started(Application :: atom()) -> ok | no_return().
ensure_app_started(Application) ->
    case application:start(Application, permanent) of
        ok ->
            ok;
        {error, {already_started, Application}} ->
            ok;
        {error, Reason} ->
            err("Failed to start ~w: ~p", [Application, Reason])
    end.

%% @doc Return epv version.
-spec version() -> iolist().
version() ->
    ok = load_app(),
    {ok, Version} = application:get_key(?MODULE, vsn),
    Version.

