%%% @doc
%%% HTTP server process warden.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 30 Mar 2012
%%% @copyright 2009-2012, Aleksey Morarash

-module(erjik_httpd_warden).

-behaviour(gen_server).

%% API exports
-export([start_link/0, hup/0, state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% state record
-record(state,
        {bind_ip,
         bind_port,
         www_root,
         server
        }).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start HTTP server warden process as part of supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends signal to HTTP server warden process to
%%      reread its configurations.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% @doc Return process state term.
%% @hidden
-spec state() -> State :: #state{}.
state() ->
    gen_server:call(?MODULE, ?SIG_STATE).

%% --------------------------------------------------------------------
%% Callback functions
%% --------------------------------------------------------------------

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Init) ->
    process_flag(trap_exit, true),
    %% make delay before first start to avoid crashes when
    %% configuration not ready
    {ok, _TRef} = timer:apply_after(3000, ?MODULE, hup, []),
    ?loginf("~w> started", [?MODULE]),
    {ok, #state{}}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info({'EXIT', From, Reason}, State)
  when is_pid(From), From == State#state.server ->
    ?logwrn("~w> died: ~99999p", [?MODULE, Reason]),
    schedule_restart(),
    {noreply, State#state{server = undefined}};
handle_info({'EXIT', _From, _Reason}, State) ->
    {noreply, State};
handle_info(Request, State) ->
    ?logwrn("~w> unknown info: ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {reply, Reply :: any(), State :: #state{}} |
                         {noreply, NewState :: #state{}}.
handle_call(?SIG_STATE, _From, State) ->
    {reply, State, State};
handle_call(Request, From, State) ->
    ?logwrn("~w> unknown call ~9999p from ~9999p",
            [?MODULE, Request, From]),
    {noreply, State}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?SIG_RECONFIG, State) ->
    ?loginf("~w> reconfig requested", [?MODULE]),
    BindIP = erjik_cfg:get(?CFG_BIND_IP),
    BindPt = erjik_cfg:get(?CFG_BIND_PORT),
    WwwRoot = erjik_cfg:get(?CFG_WWW_ROOT),
    NeedToRestart =
        not is_pid(State#state.server) orelse
        BindIP /= State#state.bind_ip orelse
        BindPt /= State#state.bind_port orelse
        WwwRoot /= State#state.www_root,
    if NeedToRestart ->
            {noreply, do_restart(State, BindIP, BindPt, WwwRoot)};
       true ->
            ?logdbg("~w> configuration remains the same",
                    [?MODULE]),
            {noreply, State}
    end;
handle_cast(Request, State) ->
    ?logwrn("~w> unknown cast: ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(Reason, State) ->
    catch exit(State#state.server, kill),
    ?loginf("~w> terminate(~9999p, ~9999p)",
            [?MODULE, Reason, State]).

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(OldVsn, State, Extra) ->
    ?loginf("~w> code_change(~9999p, ~9999p, ~9999p)",
            [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec do_restart(State :: #state{},
                 NewBindIP :: inet:ip_address(),
                 NewBindPort :: inet:port_number(),
                 NewWwwRoot :: file:filename()) -> NewState :: #state{}.
do_restart(State, NewBindIP, NewBindPort, NewWwwRoot) ->
    catch exit(State#state.server, kill),
    case erjik_httpd:start(NewBindIP, NewBindPort, NewWwwRoot) of
        {ok, Pid} ->
            ?loginf(
               "~w> listening on ~s",
               [?MODULE,
                erjik_lib:socket_to_list(NewBindIP, NewBindPort)]),
            State#state{
              bind_ip = NewBindIP,
              bind_port = NewBindPort,
              www_root = NewWwwRoot,
              server = Pid
             };
        {error, Reason} ->
            ?logerr(
               "~w> failed to listen on ~s: ~9999p",
               [?MODULE,
                erjik_lib:socket_to_list(NewBindIP, NewBindPort),
                Reason]),
            _Pid = schedule_restart(),
            State#state{server = undefined}
    end.

-spec schedule_restart() -> Pid :: pid().
schedule_restart() ->
    Millis = 5000,
    spawn(
      fun() ->
              ?logdbg(
                 "~w> scheduling respawn after ~w millis...",
                 [?MODULE, Millis]),
              timer:sleep(Millis),
              hup()
      end).

