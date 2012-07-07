%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Jul 2012
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc Replaces OTP error logger to not mess stdout with
%%%      various extra reports.

-module(erjik_error_logger).

%% API exports
-export([install/0]).

%% gen_event callback exports
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Installs itself as main error logger, removing all other
%%      error logger event listeners.
%% @spec install() -> ok
install() ->
    ErrorLogger = error_logger,
    lists:foreach(
      fun(Listener) ->
              gen_event:delete_handler(ErrorLogger, Listener, [])
      end, gen_event:which_handlers(ErrorLogger)),
    ok = gen_event:add_handler(ErrorLogger, ?MODULE, []).

%% ----------------------------------------------------------------------
%% gen_event callbacks
%% ----------------------------------------------------------------------

%% state record
-record(state, {}).

%% @hidden
init(_Args) ->
    ?logdbg("~w> installed", [?MODULE]),
    {ok, #state{}}.

%% @hidden
handle_event({info_report, _Pid, _Data}, State) ->
    %% silently discard all informational messages
    {ok, State};
handle_event(Event, State) ->
    %% relay all other messages to our file logger
    ?logwrn("~w> ~9999999999p", [?MODULE, Event]),
    {ok, State}.

%% @hidden
handle_call(Request, State) ->
    ?logwrn("~w> unknown call: ~9999p", [?MODULE, Request]),
    {ok, _Reply = ignore, State}.

%% @hidden
handle_info(Info, State) ->
    ?logwrn("~w> unknown info: ~9999p", [?MODULE, Info]),
    {ok, State}.

%% @hidden
terminate(_Arg, _State) ->
    ?logdbg("~w> terminating", [?MODULE]),
    ok.

%% @hidden
code_change(OldVsn, State, Extra) ->
    ?logdbg(
       "~w> code_change(~9999p, ~9999p, ~9999p)",
       [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

