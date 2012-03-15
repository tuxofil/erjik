%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Logger process

-module(erjik_log).

-behaviour(gen_server).

%% API functions
-export([start_link/0, reconfig/0, log/3, log/2, error/2]).

%% Callback functions
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

reconfig() ->
    gen_server:call(?MODULE, reconfig).

log(LogLevel, Format, Args)
  when is_integer(LogLevel), is_list(Format), is_list(Args) ->
    catch ?MODULE ! {log, LogLevel, Format, Args},
    ok.

log(Format, Args)
  when is_list(Format), is_list(Args) ->
    catch ?MODULE ! {log, Format, Args},
    ok.

error(Format, Args) when is_list(Format), is_list(Args) ->
    io:format(
      erjik_lib:timestamp(now()) ++
          " ERROR: " ++ Format ++ "~n", Args).

%% --------------------------------------------------------------------
%% Callback functions
%% --------------------------------------------------------------------

-define(FILE_OPTS, [raw, append]).

%% server state record
-record(state,
        {loglevel,
         logfile,
         handle}).

init(_Init) ->
    process_flag(trap_exit, true),
    case do_reconfig() of
        {ok, State} = Ok ->
            do_log(State#state.loglevel, 10, State#state.handle,
                   "~w: started", [?MODULE]),
            erjik_cfg ! logready,
            Ok;
        Error ->
            Error
    end.

handle_info(Request, State) ->
    LL = State#state.loglevel,
    Hn = State#state.handle,
    case Request of
        {log, MLL, Format, Args} ->
            do_log(LL, 50, Hn, "~w: received message: ~9999p",
                   [?MODULE, {MLL, Format, Args}]),
            do_log(LL, MLL, Hn, Format, Args),
            {noreply, State};
        {log, Format, Args} when is_list(Format), is_list(Args) ->
            do_log(LL, 50, Hn, "~w: received message: ~9999p",
                   [?MODULE, {Format, Args}]),
            do_log(Hn, Format, Args),
            {noreply, State};
        _ ->
            do_log(LL, 7, Hn, "~w: unknown info ~9999p",
                   [?MODULE, Request]),
            {noreply, State}
    end.

handle_call(Request, From, State) ->
    LL = State#state.loglevel,
    Hn = State#state.handle,
    case Request of
        reconfig ->
            do_log(LL, 12, Hn, "~w: reconfig signal received", [?MODULE]),
            {ok, NewState} = do_reconfig(State),
            do_log(NewState#state.loglevel, 12, NewState#state.handle,
                   "~w: reconfig succeeded", [?MODULE]),
            {reply, ok, NewState};
        _ ->
            do_log(LL, 7, Hn, "~w: unknown call ~9999p from ~9999p",
                   [?MODULE, Request, From]),
            {noreply, State}
    end.

handle_cast(Request, State) ->
    do_log(State#state.loglevel, 7, State#state.handle,
           "~w: unknown cast ~9999p", [?MODULE, Request]),
    {noreply, State}.

terminate(Reason, State) ->
    LL = State#state.loglevel,
    Hn = State#state.handle,
    do_log(LL, 10, Hn, "~w:terminate(~9999p, ~9999p)",
           [?MODULE, Reason, State]),
    file:close(Hn).

code_change(OldVsn, State, Extra) ->
    LL = State#state.loglevel,
    Hn = State#state.handle,
    do_log(LL, 12, Hn, "~w:code_change(~9999p, ~9999p, ~9999p)",
           [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

% --------------------------------------------------------------------
% Internal functions
% --------------------------------------------------------------------

do_reconfig() ->
    do_reconfig(#state{loglevel = ?CONF_LOGFILE_DEFAULT}).
do_reconfig(State) ->
    {ok, LF} = erjik_cfg:get(?CONF_LOGFILE, ?CONF_LOGFILE_DEFAULT),
    catch file:close(State#state.handle),
    {ok, Hn} = open(State#state.loglevel),
    {ok, LL} = erjik_cfg:get(?CONF_LOGLEVEL, ?CONF_LOGLEVEL_DEFAULT),
    if
        State#state.loglevel /= LL ->
            do_log(State#state.loglevel, 12, State#state.handle,
                   "~w: loglevel changed from ~9999p to ~9999p",
                   [?MODULE, State#state.loglevel, LL]);
        true ->
            ok
    end,
    {ok,
     State#state{
       loglevel = LL,
       logfile  = LF,
       handle   = Hn
      }}.

open(LL) ->
    {ok, Filename} =
        erjik_cfg:get(?CONF_LOGFILE, ?CONF_LOGFILE_DEFAULT),
    case file:open(Filename, ?FILE_OPTS) of
        {ok, Handle} = Ok ->
            do_log(LL, 12, Handle, "~w: log opened", [?MODULE]),
            Ok;
        {error, Reason} = Error ->
            error("~w: unable to open logfile ~9999p: ~9999p",
                  [?MODULE, Filename, Reason]),
            Error
    end.

do_log(LogLevel, MsgLogLevel, _, _, _)
  when MsgLogLevel > LogLevel -> ok;
do_log(_, _, Handle, Format, Args) ->
    do_log(Handle, Format, Args).

do_log(Handle, Format, Args) ->
    Data =
        io_lib:format(
          erjik_lib:timestamp(now()) ++ " " ++
              Format ++ "~n", Args),
    file:write(Handle, Data).

