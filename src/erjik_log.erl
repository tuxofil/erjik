%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc Logger process

-module(erjik_log).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    hup/0,
    log/3,
    state/0,
    flush/0
   ]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start logger process as part of supervision tree.
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends signal to logger process to reopen log file and
%%      reread its configurations.
%% @spec hup() -> ok
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% @doc Sends message to logger process.
%% @spec log(Severity, Format, Args) -> ok
%%     Severity = 1 | 2 | 3 | 4,
%%     Format = string(),
%%     Args = list()
log(Severity, Format, Args) ->
    catch ?MODULE ! {msg, now(), Severity, Format, Args},
    ok.

%% @doc Return process state term.
%% @hidden
%% @spec state() -> {ok, State}
%%     State = term()
state() ->
    gen_server:call(?MODULE, ?SIG_STATE).

%% @doc Flush and close log file. Usually called before
%%      VM termination.
%% @spec flush() -> ok
flush() ->
    gen_server:call(?MODULE, ?SIG_FLUSH).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

%% state record
-record(state, {loglevel = ?LOGLEVEL_INFO, handle}).

%% @hidden
init(_Args) ->
    %% trap exits for correct file close on termination
    process_flag(trap_exit, true),
    hup(),
    {ok, #state{}}.

%% @hidden
handle_info({msg, Time, Severity, Format, Args}, State)
  when State#state.handle /= undefined,
       State#state.loglevel >= Severity ->
    try
        Payload = io_lib:format(Format, Args),
        Message =
            io_lib:format(
              "~s ~s: ~s~n",
              [erjik_lib:timestamp(Time),
               severity_to_list(Severity),
               Payload]),
        file:write(State#state.handle, Message)
    catch
        Type:Reason ->
            CrashMessage =
                io_lib:format(
                  "~s ~s: LOG_ERROR: format_str: \"~s\"; args: ~99999999p; "
                  "CRASHED: ~99999999p",
                  [erjik_lib:timestamp(Time),
                   severity_to_list(1),
                   Format, Args,
                   {Type, Reason, erlang:get_stacktrace()}]),
            file:write(State#state.handle, CrashMessage)
    end,
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call(?SIG_STATE, _From, State) ->
    {reply, {ok, State}, State};
handle_call(?SIG_FLUSH, _From, State) ->
    catch file:close(State#state.handle),
    {reply, ok, State#state{handle = undefined}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(?SIG_RECONFIG, State) ->
    catch file:close(State#state.handle),
    Filename =
        case proplists:get_value(erjik_log, init:get_arguments()) of
            [[_ | _] = Filename0 | _] -> Filename0;
            _ ->
                %% default
                "./erjik.log"
        end,
    Handle =
        case file:open(Filename, [raw, append]) of
            {ok, Handle0} -> Handle0;
            _ -> undefined
        end,
    {noreply,
     State#state{
       loglevel = loglevel_to_severity(erjik_cfg:get(?CFG_LOGLEVEL)),
       handle   = Handle
      }};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
    catch file:close(State#state.handle).

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

loglevel_to_severity(?LOGLEVEL_NONE) -> 0;
loglevel_to_severity(?LOGLEVEL_ERROR) -> 1;
loglevel_to_severity(?LOGLEVEL_WARNING) -> 2;
loglevel_to_severity(?LOGLEVEL_INFO) -> 3;
loglevel_to_severity(?LOGLEVEL_DEBUG) -> 4.

severity_to_list(1) -> "ERROR";
severity_to_list(2) -> "WARNING";
severity_to_list(3) -> "INFO";
severity_to_list(4) -> "DEBUG".

