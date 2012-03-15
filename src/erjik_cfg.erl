%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Configuration facility library

-module(erjik_cfg).

-behaviour(gen_server).

%% API functions
-export([start_link/0, get/1, get/2, lookup/1, match/1, reconfig/0]).

%% Callback functions
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

get(ParamName) when is_atom(ParamName) ->
    gen_server:call(?MODULE, {get, ParamName}).

get(ParamName, Default) when is_atom(ParamName) ->
    case ?MODULE:get(ParamName) of
        {ok, _Value} = Ok ->
            Ok;
        _ ->
            {ok, Default}
    end.

lookup(Domain) when is_list(Domain) ->
    gen_server:call(?MODULE, {lookup, Domain}).

match(Url) when is_list(Url) ->
    gen_server:call(?MODULE, {match, Url}).

reconfig() ->
    gen_server:call(?MODULE, reconfig).

%% --------------------------------------------------------------------
%% Callback functions
%% --------------------------------------------------------------------

%% server state record
-record(state, {logready}).

-define(FAC_MAIN_CONF, erjik_fac_main_conf).
-define(FAC_BLACKLIST, erjik_fac_blacklist).
-define(FAC_REGEXP, erjik_fac_regexps).

init(_Args) ->
    ets:new(?FAC_MAIN_CONF, [set, public, named_table]),
    ets:new(?FAC_BLACKLIST, [set, public, named_table]),
    ets:new(?FAC_REGEXP, [set, public, named_table]),
    ok = do_reconfig(),
    {ok, #state{}}.

handle_info(logready, State) when State#state.logready ->
    {noreply, State};
handle_info(logready, State) ->
    ?log(15, "~w: second phase of configuration", [?MODULE]),
    NewState = State#state{logready = true},
    ok = do_reconfig2(NewState),
    ?log(12, "~w: second phase of configuration done", [?MODULE]),
    {noreply, NewState};
handle_info(Request, State) ->
    ?log(7, "~w: unknown info ~9999p", [?MODULE, Request]),
    {noreply, State}.

handle_call(reconfig, _From, State) ->
    ?log(15, "~w: reconfig requested", [?MODULE]),
    ok = do_reconfig(),
    ok = do_reconfig2(State),
    ?log(12, "~w: reconfig done", [?MODULE]),
    {reply, ok, State};
handle_call({lookup, Domain}, From, State) ->
    ?log(30, "~w: domain lookup for ~s", [?MODULE, Domain]),
    spawn(fun() -> gen_server:reply(From, do_lookup(Domain)) end),
    {noreply, State};
handle_call({match, Url}, From, State) ->
    ?log(30, "~w: domain match for ~s", [?MODULE, Url]),
    spawn(fun() -> gen_server:reply(From, do_match(Url)) end),
    {noreply, State};
handle_call({get, ParamName}, From, State) ->
    ?log(30, "~w: requested value for ~w", [?MODULE, ParamName]),
    spawn(fun() -> gen_server:reply(From, do_get(ParamName)) end),
    {noreply, State};
handle_call(Request, From, State) ->
    ?log(7, "~w: unknown call ~9999p from ~9999p",
         [?MODULE, Request, From]),
    {noreply, State}.

handle_cast(Request, State) ->
    ?log(7, "~w: unknown cast ~9999p", [?MODULE, Request]),
    {noreply, State}.

terminate(Reason, State) ->
    ?log(10, "~w:terminate(~9999p, ~9999p)",
         [?MODULE, Reason, State]).

code_change(OldVsn, State, Extra) ->
    ?log(12, "~w:code_change(~9999p, ~9999p, ~9999p)",
         [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

do_get(Key) ->
    case ets:lookup(?FAC_MAIN_CONF, Key) of
        [{Key, Value} | _] ->
            {ok, Value};
        _ ->
            undefined
    end.

do_get(Key, Default) ->
    case do_get(Key) of
        undefined -> {ok, Default};
        Ok -> Ok
    end.

%% @spec do_reconfig() -> ok | {error, Reason}
%%     Reason = term()
do_reconfig() ->
    try
        {ok, Terms} = file:consult(filename()),
        ets:delete_all_objects(?FAC_MAIN_CONF),
        lists:foreach(
          fun({Key, Value} = X) when is_atom(Key) ->
                  case validate(Key, Value) of
                      ok ->
                          ets:insert(?FAC_MAIN_CONF, X);
                      {error, Reason} ->
                          report_error(Reason),
                          throw(Reason)
                  end;
             ({class, ClassName, ClassOptions}) ->
                  case validate_class(ClassName, ClassOptions) of
                      ok ->
                          ets:insert(
                            ?FAC_MAIN_CONF,
                            {{class, ClassName}, ClassOptions});
                      {error, Reason} ->
                          report_error(Reason),
                          throw(Reason)
                  end;
             (Some) ->
                  fatal("~w: unrecognized term in "
                        "configuration: ~9999p",
                        [?MODULE, Some]),
                  throw({badkey, Some})
          end, Terms)
    catch
        Type:Reason ->
            fatal("~w: reconfig failed: ~9999p",
                  [?MODULE, {Type, Reason, erlang:get_stacktrace()}]),
            {error, Reason}
    end.

do_reconfig2(State) when State#state.logready ->
    {ok, BLDir} =
        do_get(?CONF_BLACKLISTS_DIR, def_blacklists_dir()),
    DomainFiles =
        [{Class, F} ||
            {{class, Class}, Opts} <-
                ets:tab2list(?FAC_MAIN_CONF), {domains, F} <- Opts],
    ets:delete_all_objects(?FAC_BLACKLIST),
    set(
      lists:flatmap(
        fun({Class, Filename}) ->
                [{D, Class} ||
                    D <- read_bl_file(
                           filename:join([BLDir, Filename])), D /= []]
        end, DomainFiles)),
    RegexpFiles =
        [{Class, F} ||
            {{class, Class}, Opts} <- ets:tab2list(?FAC_MAIN_CONF),
            {regexps, F} <- Opts],
    ets:delete_all_objects(?FAC_REGEXP),
    Regexps =
        lists:flatmap(
          fun({Class, Filename}) ->
                  [{R, Class} ||
                      R <- read_bl_file(
                             filename:join([BLDir, Filename])), R /= []]
          end, RegexpFiles),
    lists:foreach(fun(X) -> ets:insert(?FAC_REGEXP, X) end, Regexps);
do_reconfig2(_) ->
    ok.

read_bl_file(Filename) ->
    try
        {ok, Binary} = file:read_file(Filename),
        {ok, Domains} = regexp:split(binary_to_list(Binary), "\n"),
        ?log(15, "~w: file ~s ok",
             [?MODULE, filename:absname(Filename)]),
        Domains
    catch
        Type:Reason ->
            ?log(4, "~w: file ~s failed due to ~9999p",
                 [?MODULE, filename:absname(Filename),
                  {Type, Reason, erlang:get_stacktrace()}]),
            []
    end.

%% @spec set(DomainBinds) -> ok | {error, Reason}
%%     DomainBinds = [DomainBind],
%%     DomainBind = {Domain, Class},
%%     Domain = string(),
%%     Class = atom(),
%%     Reason = term()
set(DomainBinds) ->
    lists:foreach(
      fun({Domain, Class}) ->
              Lowered = string:to_lower(Domain),
              ets:insert(?FAC_BLACKLIST, {Lowered, Class})
      end, DomainBinds).

%% @spec do_lookup(Domain) -> undefined | {ok, Class, Destination} | {error, Reason}
%%     Domain = string(),
%%     Class = atom(),
%%     Destination = pass | url(),
%%     Reason = term()
do_lookup(Domain) ->
    case inet_parse:address(Domain) of
        {ok, _} ->
            case ets:lookup(?FAC_BLACKLIST, Domain) of
                [R | _] ->
                    Class = element(2, R),
                    {ok, Class, get_redirect_url(Class)};
                _ ->
                    undefined
            end;
        _ ->
            lookup_hostname(string:to_lower(Domain))
    end.

lookup_hostname(Domain) ->
    case ets:lookup(?FAC_BLACKLIST, Domain) of
        [Rec | _] ->
            Class = element(2, Rec),
            {ok, Class, get_redirect_url(Class)};
        _ ->
            Splitted = string:tokens(Domain, "."),
            if
                length(Splitted) > 1 ->
                    [_ | MoreCommon] = Splitted,
                    lookup_hostname1(MoreCommon);
                true ->
                    undefined
            end
    end.

lookup_hostname1(Domain) ->
    JoinedDomain = string:join(Domain, "."),
    case ets:lookup(?FAC_BLACKLIST, JoinedDomain) of
        [Rec | _] ->
            Class = element(2, Rec),
            {ok, Class, get_redirect_url(Class)};
        _ ->
            if
                length(Domain) > 1 ->
                    [_ | MoreCommonDomain] = Domain,
                    lookup_hostname1(MoreCommonDomain);
                true ->
                    undefined
            end
    end.

get_redirect_url(Class) ->
    case ets:lookup(?FAC_MAIN_CONF, {class, Class}) of
        [Rec | _] ->
            case [U || {redirect, U} <- element(2, Rec)] of
                [URL | _] ->
                    URL;
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

%% @spec do_match(Url) -> undefined | {ok, Class, Destination} | {error, Reason}
%%     Url = string(),
%%     Class = atom(),
%%     Destination = pass | url(),
%%     Reason = term()
do_match(Url) ->
    do_match1(Url, ets:tab2list(?FAC_REGEXP)).

do_match1(_, []) -> undefined;
do_match1(Url, [{Regexp, Class} | T]) ->
    case regexp:match(Url, Regexp) of
        {match, _, _} -> {ok, Class, get_redirect_url(Class)};
        _ -> do_match1(Url, T)
    end.

report_error({badval, Key, Value}) ->
    fatal("bad value for '~w' option: ~p", [Key, Value]);
report_error({badkey, Key}) ->
    fatal("bad option: ~p", [Key]);
report_error({badclass, Name, Options}) ->
    fatal("bad class: ~p, ~p", [Name, Options]);
report_error(Other) ->
    fatal("FATAL: ~p", [Other]).

filename() ->
    {ok, App} = application:get_application(),
    filename:absname(
      filename:join(
        ["erjik", ?PRIV_DIR, atom_to_list(App) ++ ".conf"])).

%% @spec validate(Key, Value) -> ok | {error, Reason}
validate(?CONF_LOGLEVEL, Value) ->
    if is_integer(Value) andalso
       Value >= 0 andalso
       Value =< 50 -> ok;
        true -> {error, {badval, ?CONF_LOGLEVEL, Value}}
    end;
validate(?CONF_LOGFILE, Value) ->
    case erjik_lib:is_string(Value) of
        true -> ok;
        _ -> {error, {badval, ?CONF_LOGFILE, Value}}
    end;
validate(?CONF_BLACKLISTS_DIR, Value) ->
    case erjik_lib:is_string(Value) of
        true -> ok;
        _ -> {error, {badval, ?CONF_BLACKLISTS_DIR, Value}}
    end;
validate(?CONF_PRIVILEGED, Value) ->
    case erjik_lib:is_ippool(Value) of
        true -> ok;
        false -> {error, {badval, ?CONF_PRIVILEGED, Value}}
    end;
validate(?CONF_ALLOW, Value) ->
    case erjik_lib:is_ippool(Value) of
        true -> ok;
        false -> {error, {badval, ?CONF_ALLOW, Value}}
    end;
validate(?CONF_DENY, Value) ->
    case erjik_lib:is_ippool(Value) of
        true -> ok;
        false -> {error, {badval, ?CONF_DENY, Value}}
    end;
validate(?CONF_ORDER, Value) ->
    try
        true = (length(lists:usort(Value)) == length(Value)),
        lists:foreach(
          fun(X) ->
                  true =
                      lists:member(
                        X, [?CONF_PRIVILEGED, ?CONF_ALLOW,
                            ?CONF_DENY])
          end, Value)
    catch
        _:_ ->
            {error, {badval, ?CONF_ORDER, Value}}
    end;
validate(?CONF_IP_DENY_URL, Value) ->
    case erjik_lib:is_string(Value) of
        true -> ok;
        false -> {error, {badval, ?CONF_IP_DENY_URL, Value}}
    end;
validate(Key, _Value) ->
    {error, {badkey, Key}}.

validate_class(Name, Options) when is_atom(Name), is_list(Options) ->
    ValidOption =
        fun({domains, Filename}) ->
                erjik_lib:is_string(Filename);
           ({regexps, Filename}) ->
                erjik_lib:is_string(Filename);
           ({redirect, URL}) ->
                erjik_lib:is_string(URL);
           (_) ->
                false
        end,
    case lists:all(ValidOption, Options) of
        true ->
            ok;
        _ ->
            {error, {badclass, Name, Options}}
    end;
validate_class(Name, Options) ->
    {error, {badclass, Name, Options}}.

fatal(Format, Args) ->
    ?error(Format, Args),
    ?log(1, Format, Args).

def_blacklists_dir() ->
    filename:join(["erjik", ?PRIV_DIR, "blacklists"]).

