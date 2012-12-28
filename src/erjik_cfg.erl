%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 15 Mar 2012
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc Configuration facility library

-module(erjik_cfg).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    get/1,
    classify/1,
    regexps/0,
    mime_type/1,
    hup/0,
    state/0
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
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Fetch configuration param value. In case when no configuration
%%      available yet (on system start for example) default value
%%      will be returned.
%% @spec get(Key) -> Value
%%     Key = atom(),
%%     Value = term()
get(Key) ->
    try ets:lookup(?FAC_CONFIGS, Key) of
        [{Key, Value} | _] -> Value;
        _ -> get_default_value(Key)
    catch
        _:_ -> get_default_value(Key)
    end.

%% @doc Classify URL by defined in configuration destination classes.
%% @spec classify(URL) -> {ok, ClassName, RedirectURL} | undefined
%%     URL = string(),
%%     ClassName = string(),
%%     RedirectURL = string()
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
%% @spec regexps() -> List
%%     List = [{ClassName, Regexps}],
%%     ClassName = string(),
%%     Regexps = [string()]
regexps() ->
    try ets:tab2list(?FAC_REGEXPS)
    catch _:_ -> []
    end.

%% @doc Resolves mime type for supplied filename by its suffix.
%% @spec mime_type(Filename) -> MimeType
%%     Filename = string(),
%%     MimeType = string()
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
%% @spec hup() -> ok
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% @doc Return process state term.
%% @hidden
%% @spec state() -> {ok, State}
%%     State = term()
state() ->
    gen_server:call(?MODULE, ?SIG_STATE).

%% ----------------------------------------------------------------------
%% Callback functions
%% ----------------------------------------------------------------------

%% state record
-record(state, {}).

%% @hidden
init(_Args) ->
    ?FAC_CONFIGS = ets:new(?FAC_CONFIGS, [named_table]),
    ?FAC_DOMAINS = ets:new(?FAC_DOMAINS, [named_table]),
    ?FAC_REGEXPS = ets:new(?FAC_REGEXPS, [named_table]),
    ?FAC_MIME_TYPES = ets:new(?FAC_MIME_TYPES, [named_table]),
    hup(),
    ?loginf("~w> started", [?MODULE]),
    {ok, #state{}}.

%% @hidden
handle_info(Request, State) ->
    ?logwrn("~w> unknown info ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
handle_call(?SIG_STATE, _From, State) ->
    {reply, {ok, State}, State};
handle_call(Request, From, State) ->
    ?logwrn("~w> unknown call ~9999p from ~9999p",
            [?MODULE, Request, From]),
    {noreply, State}.

%% @hidden
handle_cast(?SIG_RECONFIG, State) ->
    ?loginf("~w> reconfig requested", [?MODULE]),
    read(),
    {noreply, State};
handle_cast(Request, State) ->
    ?logwrn("~w> unknown cast ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
terminate(Reason, State) ->
    ?loginf("~w> terminate(~9999p, ~9999p)",
            [?MODULE, Reason, State]).

%% @hidden
code_change(OldVsn, State, Extra) ->
    ?loginf("~w> code_change(~9999p, ~9999p, ~9999p)",
            [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Reads configurations from main config and loads
%%      all related domain and regexp databases.
%% @spec read() -> ok
read() ->
    Filename =
        case proplists:get_value(erjik_config, init:get_arguments()) of
            [_ | _] = Filename0 -> Filename0;
            _ ->
                Default = "./erjik.conf",
                ?logwrn(
                   "~w> No config filename specified. "
                   "\"~s\" will be used as default. You may define "
                   "configuration file location with '-erjik_config' "
                   "option.", [?MODULE, Default]),
                Default
        end,
    Content =
        case file:read_file(Filename) of
            {ok, Binary} ->
                binary_to_list(Binary);
            {error, Reason} ->
                ?logerr(
                   "~w> Failed to read configuration file "
                   "\"~s\": ~9999p", [?MODULE, Filename, Reason]),
                []
        end,
    apply_config(
      assemble_config(
        parse_config(Content))),
    ?loginf("~w> reconfig done", [?MODULE]),
    catch erjik_log:hup(),
    catch erjik_re:hup(),
    catch erjik_httpd:hup(),
    ok.

%% @doc Returns default value for configuration parameter.
%% @spec get_default_value(Key) -> Value
%%     Key = atom(),
%%     Value = term()
get_default_value(?CFG_LOGLEVEL) -> ?LOGLEVEL_WARNING;
get_default_value(?CFG_IP_DENY_REDIRECT) ->
    "http://127.0.0.1:8888/denied-by-ip.html";
get_default_value(?CFG_URL_DENY_REDIRECT) ->
    "http://127.0.0.1:8888/denied-by-url.html";
get_default_value(?CFG_ORDER) -> [?CFG_ALLOW, ?CFG_DENY];
get_default_value(?CFG_PRIVILEGED) -> [];
get_default_value(?CFG_ALLOW) -> [any];
get_default_value(?CFG_DENY) -> [];
get_default_value(?CFG_IP_DEFAULT_POLICY) -> ?CFG_ALLOW;
get_default_value(?CFG_URL_DEFAULT_POLICY) -> ?CFG_ALLOW;
get_default_value(?CFG_BIND_IP) -> {127,0,0,1};
get_default_value(?CFG_BIND_PORT) -> 8888;
get_default_value(?CFG_WWW_ROOT) -> "./www/";
get_default_value(?CFG_MIME_TYPES) -> "/etc/mime.types";
get_default_value(_) -> undefined.

%% @doc Returns parsed all valid key-value pairs from
%%      configuration file body.
%% @spec parse_config(String) -> ConfigItems
%%     String = string(),
%%     ConfigItems = [ConfigItem],
%%     ConfigItem = {Key, Value},
%%     Key = atom() | {class, ClassName, ClassParam},
%%     ClassName = string(),
%%     ClassParam = atom(),
%%     Value = term()
parse_config(String) ->
    lists:flatmap(
      fun({LineNo, StrKey, StrValue}) ->
              case parse_key(StrKey) of
                  {ok, Key} ->
                      case parse_val(Key, StrValue) of
                          {ok, Value} ->
                              [{Key, Value}];
                          _ ->
                              ?logerr(
                                 "~w> config: bad value on line ~w "
                                 "for key '~s': \"~s\"",
                                 [?MODULE, LineNo, StrKey, StrValue]),
                              []
                      end;
                  _ ->
                      ?logerr(
                         "~w> config: bad key on line ~w: \"~s\"",
                         [?MODULE, LineNo, StrKey]),
                      []
              end
      end, erjik_lib:preparse_config(String)).

%% @doc Processes parsed key-value pairs and produces final
%%      application configuration database.
%% @spec assemble_config(ConfigItems) -> {Config, Classes, MimeTypes}
%%     ConfigItems = [ConfigItem],
%%     ConfigItem = {Key, Value},
%%         Key = atom() | {class, ClassName, ClassParam},
%%         ClassName = string(),
%%         ClassParam = atom(),
%%         Value = term(),
%%     Config = [{CfgKey, CfgValue}],
%%         CfgKey = atom(),
%%         CfgValue = term(),
%%     Classes = [{ClassName, Domains, Regexps, RedirectURL}],
%%         Domains = [string()],
%%         Regexps = [string()],
%%         RedirectURL = undefined | string(),
%%     MimeTypes = [{FileSuffix, MimeType}],
%%         FileSuffix = string(),
%%         MimeType = string()
assemble_config(CfgItems) ->
    Config =
        [{?CFG_LOGLEVEL,
          case [V || {?CFG_LOGLEVEL, V} <- CfgItems] of
              [LogLevel | _] ->
                  logcfg(?CFG_LOGLEVEL, LogLevel),
                  LogLevel;
              _ -> set_default(?CFG_LOGLEVEL)
          end},
         {?CFG_IP_DENY_REDIRECT,
          case [V || {?CFG_IP_DENY_REDIRECT, [_ | _] = V} <- CfgItems] of
              [IpDenyRedirect | _] ->
                  logcfg(?CFG_IP_DENY_REDIRECT, IpDenyRedirect),
                  IpDenyRedirect;
              _ -> set_default(?CFG_IP_DENY_REDIRECT)
          end},
         {?CFG_URL_DENY_REDIRECT,
          case [V || {?CFG_URL_DENY_REDIRECT, [_ | _] = V} <- CfgItems] of
              [UrlDenyRedirect | _] ->
                  logcfg(?CFG_URL_DENY_REDIRECT, UrlDenyRedirect),
                  UrlDenyRedirect;
              _ -> set_default(?CFG_URL_DENY_REDIRECT)
          end},
         {?CFG_ORDER,
          case [V || {?CFG_ORDER, V} <- CfgItems] of
              [Order | _] ->
                  logcfg(?CFG_ORDER, Order),
                  Order;
              _ -> set_default(?CFG_ORDER)
          end},
         {?CFG_PRIVILEGED,
          case [V || {?CFG_PRIVILEGED, V} <- CfgItems] of
              [_ | _] = Privileged0 ->
                  Privileged = erjik_lib:flatten_ip_pool(Privileged0),
                  logcfg(?CFG_PRIVILEGED, Privileged),
                  Privileged;
              _ -> set_default(?CFG_PRIVILEGED)
          end},
         {?CFG_ALLOW,
          case [V || {?CFG_ALLOW, V} <- CfgItems] of
              [_ | _] = Allow0 ->
                  Allow = erjik_lib:flatten_ip_pool(Allow0),
                  logcfg(?CFG_ALLOW, Allow),
                  Allow;
              _ -> set_default(?CFG_ALLOW)
          end},
         {?CFG_DENY,
          case [V || {?CFG_DENY, V} <- CfgItems] of
              [_ | _] = Deny0 ->
                  Deny = erjik_lib:flatten_ip_pool(Deny0),
                  logcfg(?CFG_DENY, Deny),
                  Deny;
              _ -> set_default(?CFG_DENY)
          end},
         {?CFG_IP_DEFAULT_POLICY,
          case [V || {?CFG_IP_DEFAULT_POLICY, V} <- CfgItems] of
              [IpDefaultPolicy | _] ->
                  logcfg(?CFG_IP_DEFAULT_POLICY, IpDefaultPolicy),
                  IpDefaultPolicy;
              _ -> set_default(?CFG_IP_DEFAULT_POLICY)
          end},
         {?CFG_URL_DEFAULT_POLICY,
          case [V || {?CFG_URL_DEFAULT_POLICY, V} <- CfgItems] of
              [UrlDefaultPolicy | _] ->
                  logcfg(?CFG_URL_DEFAULT_POLICY, UrlDefaultPolicy),
                  UrlDefaultPolicy;
              _ -> set_default(?CFG_URL_DEFAULT_POLICY)
          end},
         {?CFG_BIND_IP,
          case [V || {?CFG_BIND_IP, V} <- CfgItems] of
              [BindIP | _] ->
                  logcfg(?CFG_BIND_IP, BindIP),
                  BindIP;
              _ -> set_default(?CFG_BIND_IP)
          end},
         {?CFG_BIND_PORT,
          case [V || {?CFG_BIND_PORT, V} <- CfgItems] of
              [BindPort | _] ->
                  logcfg(?CFG_BIND_PORT, BindPort),
                  BindPort;
              _ -> set_default(?CFG_BIND_PORT)
          end},
         {?CFG_WWW_ROOT,
          case [V || {?CFG_WWW_ROOT, V} <- CfgItems] of
              [WwwRoot | _] ->
                  logcfg(?CFG_WWW_ROOT, WwwRoot),
                  WwwRoot;
              _ -> set_default(?CFG_WWW_ROOT)
          end},
         {?CFG_MIME_TYPES,
          case [V || {?CFG_MIME_TYPES, V} <- CfgItems] of
              [MimeTypes | _] ->
                  logcfg(?CFG_MIME_TYPES, MimeTypes),
                  MimeTypes;
              _ -> set_default(?CFG_MIME_TYPES)
          end}],
    %% read blacklists...
    Classes0 =
        lists:map(
          fun(ClassName) ->
                  PL = [{K, V} ||
                           {{class, C, K}, V} <- CfgItems,
                           C == ClassName],
                  {ClassName,
                   case [V || {?CFG_CLASS_DOMAINS, V} <- PL] of
                       [[_ | _] = DomainsFilename | _] ->
                           read_blacklist(DomainsFilename);
                       _ -> []
                   end,
                   case [V || {?CFG_CLASS_REGEXPS, V} <- PL] of
                       [[_ | _] = RegexpsFilename | _] ->
                           read_blacklist(RegexpsFilename);
                       _ -> []
                   end,
                   case [V || {?CFG_CLASS_REDIRECT, V} <- PL] of
                       [[_ | _] = URL | _] -> URL;
                       _ -> undefined
                   end}
          end, erjik_lib:uniq([N || {{class, N, _}, _} <- CfgItems])),
    %% drop classes with empty lists...
    Classes =
        lists:filter(
          fun({ClassName, [], [], _}) ->
                  ?loginf(
                     "~w> config: class \"~s\" dropped off becouse of "
                     "empty list of domains and regexps",
                     [?MODULE, ClassName]),
                  false;
             (_) -> true
          end, Classes0),
    case Classes of
        [] ->
            ?logwrn(
               "~w> There is no meaning destination "
               "classes at all! Check your configuration.",
               [?MODULE]);
        _ -> nop
    end,
    {Config, Classes, read_mime_types()}.

%% @doc Inserts configuration data to database (ETSes)
%% @spec apply_config(Configs) -> ok
%%     Configs = term()
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
%% low level parsers and data processing tools

logcfg(Key, Value) ->
    ?loginf(
       "~w> config: '~w' set to: ~s",
       [?MODULE, Key, cfg_to_list(Key, Value)]).

set_default(Key) ->
    Value = get_default_value(Key),
    ?loginf(
       "~w> config: '~w' set to default: ~s",
       [?MODULE, Key, cfg_to_list(Key, Value)]),
    Value.

cfg_to_list(?CFG_LOGLEVEL, LogLevel) -> atom_to_list(LogLevel);
cfg_to_list(?CFG_IP_DENY_REDIRECT, URL) -> URL;
cfg_to_list(?CFG_URL_DENY_REDIRECT, URL) -> URL;
cfg_to_list(?CFG_ORDER, Order) ->
    string:join([atom_to_list(A) || A <- Order], ",");
cfg_to_list(?CFG_PRIVILEGED, []) -> "none";
cfg_to_list(?CFG_PRIVILEGED, IpRange) ->
    erjik_lib:ip_pool_to_list(IpRange);
cfg_to_list(?CFG_ALLOW, []) -> "none";
cfg_to_list(?CFG_ALLOW, IpRange) ->
    erjik_lib:ip_pool_to_list(IpRange);
cfg_to_list(?CFG_DENY, []) -> "none";
cfg_to_list(?CFG_DENY, IpRange) ->
    erjik_lib:ip_pool_to_list(IpRange);
cfg_to_list(?CFG_IP_DEFAULT_POLICY, DefaultPolicy) ->
    atom_to_list(DefaultPolicy);
cfg_to_list(?CFG_URL_DEFAULT_POLICY, DefaultPolicy) ->
    atom_to_list(DefaultPolicy);
cfg_to_list(?CFG_BIND_IP, BindIP) ->
    erjik_lib:ip_to_list(BindIP);
cfg_to_list(?CFG_BIND_PORT, BindPort) ->
    integer_to_list(BindPort);
cfg_to_list(?CFG_WWW_ROOT, WwwRoot) -> WwwRoot;
cfg_to_list(?CFG_MIME_TYPES, MimeTypes) -> MimeTypes.

read_blacklist(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            lists:flatmap(
              fun(Line) ->
                      case erjik_lib:strip(Line, " \t") of
                          [_ | _] = Item -> [Item];
                          _ -> []
                      end
              end, string:tokens(binary_to_list(Binary), "\r\n"));
        {error, Reason} ->
            ?logerr(
               "~w> config: unable to read file \"~s\": ~9999p",
               [?MODULE, Filename, Reason]),
            []
    end.

parse_key("class." ++ String) ->
    case parse_class_key(String, []) of
        {ok, ClassName, ClassParam} ->
            case erjik_lib:list_to_atom(ClassParam, ?CFGS_CLASS) of
                {ok, Key} ->
                    {ok, {class, ClassName, Key}};
                _ -> error
            end;
        _ -> error
    end;
parse_key(String) ->
    erjik_lib:list_to_atom(String, ?CFGS_SIMPLE).

parse_class_key([], _) -> error;
parse_class_key([$. | _], []) -> error;
parse_class_key([$. | ConfigName], ClassName) ->
    {ok, lists:reverse(ClassName), ConfigName};
parse_class_key([C | Tail], ClassName)
  when (C >= $a andalso C =< $z) orelse
       (C >= $0 andalso C =< $9) orelse
       (C == $_ orelse C == $- orelse $:) ->
    parse_class_key(Tail, [C | ClassName]);
parse_class_key(_, _) -> error.

parse_val(Key, StrValue) ->
    try {ok, _Value} = parse_val_(Key, StrValue)
    catch _:_ -> error end.
parse_val_(?CFG_LOGLEVEL, String) ->
    erjik_lib:list_to_atom(
      string:to_lower(String), ?LOGLEVELS);
parse_val_(?CFG_PRIVILEGED, String) ->
    parse_ip_range(String);
parse_val_(?CFG_ALLOW, String) ->
    parse_ip_range(String);
parse_val_(?CFG_DENY, String) ->
    parse_ip_range(String);
parse_val_(?CFG_IP_DENY_REDIRECT, [_ | _] = String) ->
    {ok, String};
parse_val_(?CFG_URL_DENY_REDIRECT, [_ | _] = String) ->
    {ok, String};
parse_val_(?CFG_IP_DEFAULT_POLICY, String) ->
    erjik_lib:list_to_atom(
      string:to_lower(String), [?CFG_ALLOW, ?CFG_DENY]);
parse_val_(?CFG_URL_DEFAULT_POLICY, String) ->
    erjik_lib:list_to_atom(
      string:to_lower(String), [?CFG_ALLOW, ?CFG_DENY]);
parse_val_(?CFG_ORDER, String) ->
    {ok,
     erjik_lib:uniq(
       lists:map(
         fun(S) ->
                 {ok, A} =
                     erjik_lib:list_to_atom(
                       string:to_lower(S),
                       [?CFG_ALLOW, ?CFG_DENY]),
                 A
         end, string:tokens(String, " \t,")))};
parse_val_(?CFG_BIND_IP, String) ->
    erjik_lib:list_to_ip(String);
parse_val_(?CFG_BIND_PORT, String) ->
    Int = list_to_integer(String),
    true = 0 < Int andalso Int < 16#ffff,
    {ok, Int};
parse_val_(?CFG_WWW_ROOT, String) ->
    {ok, String};
parse_val_(?CFG_MIME_TYPES, [_ | _] = String) ->
    {ok, String};
parse_val_({class, [_ | _] = _ClassName, ?CFG_CLASS_DOMAINS},
           [_ | _] = String) ->
    {ok, String};
parse_val_({class, [_ | _] = _ClassName, ?CFG_CLASS_REGEXPS},
           [_ | _] = String) ->
    {ok, String};
parse_val_({class, [_ | _] = _ClassName, ?CFG_CLASS_REDIRECT},
           [_ | _] = String) ->
    {ok, String}.

parse_ip_range(String) ->
    case string:to_lower(String) of
        "any" -> {ok, any};
        "all" -> {ok, any};
        "none" -> {ok, none};
        _ -> erjik_lib:list_to_ip_range(String)
    end.

%% ----------------------------------------------------------------------
%% mime types

default_mime_types() ->
    [{"text/html", ["htm", "html"]},
     {"text/plain", ["txt"]},
     {"text/css", ["css"]}].

read_mime_types() ->
    [{string:to_lower(Ext), string:to_lower(Name)} ||
        {Name, List} <- read_mime_types_(),
        Ext <- List].
read_mime_types_() ->
    Filename = erjik_cfg:get(?CFG_MIME_TYPES),
    case erjik_lib:read_mime_types(Filename) of
        {ok, List} -> List;
        {error, Reason} ->
            ?logerr(
               "~w> failed to read mime types from \"~s\": ~9999p. "
               "Default mime table will be used.",
               [?MODULE, Filename, Reason]),
            default_mime_types()
    end.

%% ----------------------------------------------------------------------
%% URL classification routines

classify(URL, Hostname, IsIP) ->
    case classify_(URL, Hostname, IsIP) of
        {ok, _ClassName} = Ok -> Ok;
        _ ->
            case erjik_re:match(URL) of
                {ok, _ClassName} = Ok -> Ok;
                _ -> undefined
            end
    end.
classify_(_URL, StrIP, true) ->
    case ets:lookup(?FAC_DOMAINS, StrIP) of
        [{_, ClassName}] -> {ok, ClassName};
        _ -> undefined
    end;
classify_(_URL, Domain, _IsIP = false) ->
    classify_domain(string:tokens(Domain, ".")).

classify_domain([_ | Tail] = Tokens) ->
    Domain = string:join(Tokens, "."),
    case ets:lookup(?FAC_DOMAINS, Domain) of
        [{_, ClassName}] -> {ok, ClassName};
        _ -> classify_domain(Tail)
    end;
classify_domain(_) -> undefined.

get_class_redirect_url(ClassName) ->
    case ets:lookup(?FAC_DOMAINS, {url, ClassName}) of
        [{_, [_ | _] = URL}] -> URL;
        _ -> undefined
    end.

