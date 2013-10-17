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

%% ----------------------------------------------------------------------
%% Types definitions
%% ----------------------------------------------------------------------

-type regexps() :: [regexps_chunk()].

-type regexps_chunk() :: [regexps_item()].

-type regexps_item() ::
        {ClassName :: nonempty_string(),
         Regexp :: nonempty_string()}.

%% state record
-record(state, {regexps = [] :: regexps()}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start process as part of supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends signal to process to reread its configurations.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% @doc Finds ClassName matching for URL specified.
-spec match(URL :: nonempty_string()) ->
                   {ok, ClassName :: nonempty_string()} | undefined.
match(URL) ->
    gen_server:call(?MODULE, {match, URL}).

%% @doc Return process state term.
%% @hidden
-spec state() -> State :: #state{}.
state() ->
    gen_server:call(?MODULE, ?SIG_STATE).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ok = hup(),
    {ok, #state{}}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {reply, Reply :: any(), NewState :: #state{}} |
                         {noreply, NewState :: #state{}}.
handle_call({match, URL}, From, State) ->
    spawn_link(
      fun() ->
              gen_server:reply(
                From,
                do_match(URL, State#state.regexps))
      end),
    {noreply, State};
handle_call(?SIG_STATE, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?SIG_RECONFIG, State) ->
    {noreply, do_reconfig(State)};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec do_reconfig(State :: #state{}) -> NewState :: #state{}.
do_reconfig(State) ->
    List =
        [{ClassName, Regexp} ||
            {ClassName, Regexps} <- erjik_cfg:regexps(),
            Regexp <- Regexps],
    Len = length(List),
    ThreadsCount = erlang:system_info(schedulers) * 4,
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

-spec do_match(URL :: nonempty_string(), ListOfLists :: regexps()) ->
                      {ok, ClassName :: nonempty_string()} |
                      undefined.
do_match(URL, ListOfLists) ->
    erjik_lib:challenge(
      fun(List) ->
              matcher(URL, List)
      end, ListOfLists).

-spec matcher(URL :: nonempty_string(), Pairs :: regexps_chunk()) ->
                     ClassName :: nonempty_string().
matcher(_URL, []) ->
    exit(no_more_left);
matcher(URL, [{ClassName, Regexp} | Tail]) ->
    case re:run(URL, Regexp, []) of
        {match, _} ->
            ClassName;
        _ ->
            matcher(URL, Tail)
    end.

