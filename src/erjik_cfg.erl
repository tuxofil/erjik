%%% @doc
%%% Configuration facility library.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 15 Mar 2012
%%% @copyright 2009-2012, Aleksey Morarash

-module(erjik_cfg).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    get/1,
    classify/1,
    regexps/0,
    mime_type/1,
    hup/0
   ]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% ETS names
-define(FAC_CONFIGS, erjik_fac_configs).
-define(FAC_DOMAINS, erjik_fac_domains).
-define(FAC_REGEXPS, erjik_fac_regexps).
-define(FAC_MIME_TYPES, erjik_fac_mime_types).

-define(default_mime_type, "application/octet-stream").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start configuration facility process as part of supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Fetch configuration param value. In case when no configuration
%%      available yet (on system start for example) default value
%%      will be returned.
-spec get(Key :: atom()) -> Value :: any().
get(Key) ->
    try ets:lookup(?FAC_CONFIGS, Key) of
        [{Key, Value} | _] ->
            Value;
        _ ->
            erjik_config_parser:get_default_value(Key)
    catch
        _:_ ->
            erjik_config_parser:get_default_value(Key)
    end.

%% @doc Classify URL by defined in configuration destination classes.
-spec classify(URL :: nonempty_string()) ->
                      {ok, ClassName :: nonempty_string(),
                       RedirectURL :: nonempty_string() | undefined} |
                      undefined.
classify(URL) ->
    case erjik_lib:parse_uri(URL) of
        {ok, List} ->
            {Hostname, IsIP} =
                case proplists:get_value(hostname, List) of
                    [_ | _] = Domain ->
                        {Domain, false};
                    IP when is_tuple(IP) ->
                        {erjik_lib:ip_to_list(IP), true}
                end,
            case classify(URL, Hostname, IsIP) of
                {ok, ClassName} ->
                    {ok, ClassName,
                     get_class_redirect_url(ClassName)};
                _ -> undefined
            end;
        _ -> undefined
    end.

%% @doc Return all regexps defined in configurations.
-spec regexps() -> [{ClassName :: nonempty_string(),
                     Regexps :: [nonempty_string()]}].
regexps() ->
    try ets:tab2list(?FAC_REGEXPS)
    catch _:_ -> []
    end.

%% @doc Resolves mime type for supplied filename by its suffix.
-spec mime_type(Filename :: file:filename()) -> MimeType :: nonempty_string().
mime_type(Filename) ->
    case lists:reverse(
           string:tokens(
             filename:basename(Filename), ".")) of
        [Ext, _ | _] ->
            LoweredExt = string:to_lower(Ext),
            try ets:lookup(?FAC_MIME_TYPES, LoweredExt) of
                [{_, MimeType} | _] -> MimeType;
                _ -> ?default_mime_type
            catch
                _:_ -> ?default_mime_type
            end;
        _ -> ?default_mime_type
    end.

%% @doc Sends 'reconfig' signal to configuration facility process.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% ----------------------------------------------------------------------
%% Callback functions
%% ----------------------------------------------------------------------

%% state record
-record(state, {}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?FAC_CONFIGS = ets:new(?FAC_CONFIGS, [named_table]),
    ?FAC_DOMAINS = ets:new(?FAC_DOMAINS, [named_table]),
    ?FAC_REGEXPS = ets:new(?FAC_REGEXPS, [named_table]),
    ?FAC_MIME_TYPES = ets:new(?FAC_MIME_TYPES, [named_table]),
    hup(),
    ?loginf("~w> started", [?MODULE]),
    {ok, #state{}}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(Request, State) ->
    ?logwrn("~w> unknown info ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_call(Request, From, State) ->
    ?logwrn("~w> unknown call ~9999p from ~9999p",
            [?MODULE, Request, From]),
    {noreply, State}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?SIG_RECONFIG, State) ->
    ?loginf("~w> reconfig requested", [?MODULE]),
    read(),
    {noreply, State};
handle_cast(Request, State) ->
    ?logwrn("~w> unknown cast ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(Reason, State) ->
    ?loginf("~w> terminate(~9999p, ~9999p)",
            [?MODULE, Reason, State]).

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(OldVsn, State, Extra) ->
    ?loginf("~w> code_change(~9999p, ~9999p, ~9999p)",
            [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Reads configurations from main config and loads
%%      all related domain and regexp databases.
-spec read() -> ok.
read() ->
    {ok, Path} = application:get_env(?CFG_ERJIK_CONFIG),
    apply_config(erjik_config_parser:read(Path)),
    ?loginf("~w> reconfig done", [?MODULE]),
    ok = erjik_log:hup(),
    ok = erjik_re:hup(),
    ok = erjik_httpd_warden:hup().

%% @doc Inserts configuration data to database (ETSes)
-spec apply_config({Config :: erjik_config_parser:config(),
                    Classes :: erjik_config_parser:classes(),
                    MimeTypes :: erjik_config_parser:mime_types()}) -> ok.
apply_config({Config, Classes, MimeTypes}) ->
    IpDefaultPolicy = proplists:get_value(?CFG_IP_DEFAULT_POLICY, Config),
    UrlDefaultPolicy = proplists:get_value(?CFG_URL_DEFAULT_POLICY, Config),
    TmpConfig =
        [{?CFG_IP_DEFAULT_POLICY, ?CFG_ALLOW},
         {?CFG_URL_DEFAULT_POLICY, ?CFG_ALLOW} |
         [I || {K, _} = I <- Config,
               K /= ?CFG_IP_DEFAULT_POLICY,
               K /= ?CFG_URL_DEFAULT_POLICY]],
    ets:insert(?FAC_CONFIGS, TmpConfig),
    ets:delete_all_objects(?FAC_DOMAINS),
    ets:delete_all_objects(?FAC_REGEXPS),
    lists:foreach(
      fun({ClassName, Domains, Regexps, URL}) ->
              ets:insert(
                ?FAC_DOMAINS,
                [{{url, ClassName}, URL} |
                 [{D, ClassName} || D <- Domains]]),
              ets:insert(?FAC_REGEXPS, {ClassName, Regexps})
      end, Classes),
    ets:insert(
      ?FAC_CONFIGS,
      [{?CFG_IP_DEFAULT_POLICY, IpDefaultPolicy},
       {?CFG_URL_DEFAULT_POLICY, UrlDefaultPolicy}]),
    ets:delete_all_objects(?FAC_MIME_TYPES),
    ets:insert(?FAC_MIME_TYPES, MimeTypes),
    ok.

%% ----------------------------------------------------------------------
%% URL classification routines

-spec classify(URL :: nonempty_string(), Hostname :: nonempty_string(),
               IsIP :: boolean()) ->
                      {ok, ClassName :: nonempty_string()} |
                      undefined.
classify(URL, Hostname, IsIP) ->
    case classify_(URL, Hostname, IsIP) of
        {ok, _ClassName} = Ok -> Ok;
        _ ->
            case erjik_re:match(URL) of
                {ok, _ClassName} = Ok -> Ok;
                _ -> undefined
            end
    end.

-spec classify_(URL :: nonempty_string(), Hostname :: nonempty_string(),
                IsIP :: boolean()) ->
                       {ok, ClassName :: nonempty_string()} |
                       undefined.
classify_(_URL, StrIP, true) ->
    case ets:lookup(?FAC_DOMAINS, StrIP) of
        [{_, ClassName}] -> {ok, ClassName};
        _ -> undefined
    end;
classify_(_URL, Domain, _IsIP = false) ->
    classify_domain(string:tokens(Domain, ".")).

-spec classify_domain(DomainTokens :: [nonempty_string()]) ->
                             {ok, ClassName :: nonempty_string()} |
                             undefined.
classify_domain([_ | Tail] = Tokens) ->
    Domain = string:join(Tokens, "."),
    case ets:lookup(?FAC_DOMAINS, Domain) of
        [{_, ClassName}] -> {ok, ClassName};
        _ -> classify_domain(Tail)
    end;
classify_domain(_) -> undefined.

-spec get_class_redirect_url(ClassName :: nonempty_string()) ->
                                    (URL :: nonempty_string()) | undefined.
get_class_redirect_url(ClassName) ->
    case ets:lookup(?FAC_DOMAINS, {url, ClassName}) of
        [{_, [_ | _] = URL}] -> URL;
        _ -> undefined
    end.

%% ----------------------------------------------------------------------
%% eunit tests
%% ----------------------------------------------------------------------

-ifdef(TEST).

-endif.

