%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 18 Apr 2012
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc Keeper of regexps

-module(erjik_re).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    hup/0,
    match/1,
    state/0
   ]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start process as part of supervision tree.
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends signal to process to reread its configurations.
%% @spec hup() -> ok
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% @doc Finds ClassName matching for URL specified.
%% @spec match(URL) -> {ok, ClassName} | undefined
%%     URL = string(),
%%     ClassName = string()
match(URL) ->
    gen_server:call(?MODULE, {match, URL}).

%% @doc Return process state term.
%% @hidden
%% @spec state() -> {ok, State}
%%     State = term()
state() ->
    gen_server:call(?MODULE, state).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

%% state record
-record(state, {regexps = []}).

%% @hidden
init(_Args) ->
    ok = hup(),
    {ok, #state{}}.

%% @hidden
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call({match, URL}, From, State) ->
    spawn_link(
      fun() ->
              gen_server:reply(
                From,
                do_match(URL, State#state.regexps))
      end),
    {noreply, State};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(?SIG_RECONFIG, State) ->
    {noreply, do_reconfig(State)};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

do_reconfig(State) ->
    List =
        lists:flatmap(
          fun({ClassName, Regexps}) ->
                  [{ClassName, Re} || Re <- Regexps]
          end, erjik_cfg:regexps()),
    Len = length(List),
    ThreadsCount = erlang:system_info(schedulers) * 2,
    ChunkLenFloat = Len / ThreadsCount,
    ChunkLenTrunc = trunc(ChunkLenFloat),
    ChunkLen =
        if ChunkLenFloat == ChunkLenTrunc ->
                ChunkLenTrunc;
           true ->
                ChunkLenTrunc + 1
        end,
    if Len == 0 -> State#state{regexps = []};
       true ->
            State#state{
              regexps = erjik_lib:split_by_len(List, ChunkLen)
             }
    end.

do_match(URL, ListOfLists) ->
    erjik_lib:challenge(
      fun(List) ->
              matcher(URL, List)
      end, ListOfLists).

matcher(_URL, []) -> throw(no_more_left);
matcher(URL, [{ClassName, Regexp} | Tail]) ->
    case re:run(URL, Regexp, []) of
        {match, _} -> ClassName;
        _ -> matcher(URL, Tail)
    end.

