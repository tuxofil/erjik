%%% @doc
%%% Erjik configuration file parser tools.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Nov 2013
%%% @copyright 2009-2013, Aleksey Morarash

-module(erjik_config_parser).

%% API exports
-export(
   [read/1,
    read_instance_cfg/1,
    read_logger_cfg/1,
    get_default_value/1
   ]).

-include("erjik.hrl").

%% ----------------------------------------------------------------------
%% Types definitions
%% ----------------------------------------------------------------------

-export_type(
   [raw_config/0,
    raw_config_item/0,
    class_param/0,
    config/0,
    config_item/0,
    classes/0,
    class/0,
    mime_types/0,
    mime_type/0,
    loglevel/0
   ]).

-type raw_config() :: [raw_config_item()].

-type raw_config_item() ::
        config_item() |
        {Key :: {class, ClassName :: nonempty_string(),
                 ClassParam :: ?CFG_CLASS_DOMAINS},
         Filename :: file:filename()} |
        {Key :: {class, ClassName :: nonempty_string(),
                 ClassParam :: ?CFG_CLASS_REGEXPS},
         Filename :: file:filename()} |
        {Key :: {class, ClassName :: nonempty_string(),
                 ClassParam :: ?CFG_CLASS_REDIRECT},
         URL :: nonempty_string() | undefined}.

-type raw_config_item_key() ::
        config_simple_key() |
        {class, ClassName :: nonempty_string(),
         ClassParam :: class_param()}.

-type class_param() ::
        ?CFG_CLASS_DOMAINS | ?CFG_CLASS_REGEXPS | ?CFG_CLASS_REDIRECT.

-type config() :: [config_item()].

-type config_item() ::
        {?CFG_LOGLEVEL, LogLevel :: loglevel()} |
        {?CFG_IP_DENY_REDIRECT, URL :: string()} |
        {?CFG_URL_DENY_REDIRECT, URL :: string()} |
        {?CFG_ORDER, Order :: [?CFG_ALLOW | ?CFG_DENY]} |
        {?CFG_PRIVILEGED, [IP :: inet:ip_address()]} |
        {?CFG_ALLOW, AllowedAddrs :: erjik_lib:ip_pool()} |
        {?CFG_DENY, RestrictedAddrs :: erjik_lib:ip_pool()} |
        {?CFG_IP_DEFAULT_POLICY, Policy :: ?CFG_ALLOW | ?CFG_DENY} |
        {?CFG_URL_DEFAULT_POLICY, Policy :: ?CFG_ALLOW | ?CFG_DENY} |
        {?CFG_BIND_IP, inet:ip_address()} |
        {?CFG_BIND_PORT, inet:port_number()} |
        {?CFG_WWW_ROOT, WwwRootPath :: string()} |
        {?CFG_MIME_TYPES, MymeTypesPath :: string()} |
        {?CFG_LOG_PATH, LogPath :: file:filename()} |
        {?CFG_INSTANCE_ID, InstanceID :: atom()} |
        {?CFG_COOKIE, Cookie :: atom()}.

-type config_simple_key() ::
        ?CFG_LOGLEVEL | ?CFG_PRIVILEGED | ?CFG_ALLOW | ?CFG_DENY |
        ?CFG_ORDER | ?CFG_IP_DENY_REDIRECT | ?CFG_URL_DENY_REDIRECT |
        ?CFG_MIME_TYPES | ?CFG_IP_DEFAULT_POLICY | ?CFG_URL_DEFAULT_POLICY |
        ?CFG_BIND_IP | ?CFG_BIND_PORT | ?CFG_WWW_ROOT | ?CFG_LOG_PATH |
        ?CFG_INSTANCE_ID | ?CFG_COOKIE.

-type classes() :: [class()].

-type class() ::
        {ClassName :: nonempty_string(),
         Domains :: [nonempty_string()],
         Regexps :: [nonempty_string()],
         RedirectURL :: nonempty_string() | undefined}.

-type mime_types() :: [mime_type()].

-type mime_type() ::
        {FilenameExtension :: nonempty_string(),
         MimeType :: nonempty_string()}.

-type loglevel() ::
        ?LOGLEVEL_NONE | ?LOGLEVEL_ERROR | ?LOGLEVEL_WARNING |
        ?LOGLEVEL_INFO | ?LOGLEVEL_DEBUG.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Read configurations from main config and loads
%%      all related domain and regexp databases.
-spec read(ConfigPath :: file:filename()) ->
                  {Config :: config(),
                   Classes :: classes(),
                   MimeTypes :: mime_types()}.
read(ConfigPath) ->
    assemble_config(parse_config(read_config_file(ConfigPath))).

%% @doc Read Erjik Instance ID and Cookie from the main configuration file.
-spec read_instance_cfg(ConfigPath :: file:filename()) ->
                               {DaemonID :: atom(),
                                Cookie :: atom()}.
read_instance_cfg(ConfigPath) ->
    Config = assemble_simple(parse_config(read_config_file(ConfigPath))),
    {proplists:get_value(?CFG_INSTANCE_ID, Config),
     proplists:get_value(?CFG_COOKIE, Config)}.

%% @doc Read log file path and current loglevel from
%% the main configuration file.
-spec read_logger_cfg(ConfigPath :: file:filename()) ->
                             {LogPath :: file:filename(),
                              LogLevel :: loglevel()}.
read_logger_cfg(ConfigPath) ->
    Config = assemble_simple(parse_config(read_config_file(ConfigPath))),
    {proplists:get_value(?CFG_LOG_PATH, Config),
     proplists:get_value(?CFG_LOGLEVEL, Config)}.

%% @doc Returns default value for configuration parameter.
-spec get_default_value(Key :: atom()) ->
                               (Value :: any()) | undefined.
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
get_default_value(?CFG_WWW_ROOT) -> "./www";
get_default_value(?CFG_MIME_TYPES) -> "/etc/mime.types";
get_default_value(?CFG_ERJIK_CONFIG) -> "./erjik.conf";
get_default_value(?CFG_LOG_PATH) -> "./erjik.log";
get_default_value(?CFG_INSTANCE_ID) -> erjik;
get_default_value(?CFG_COOKIE) -> erjik;
get_default_value(_) -> undefined.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

-spec read_config_file(Filename :: file:filename()) -> string().
read_config_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            binary_to_list(Binary);
        {error, Reason} ->
            ?logerr(
               "~w> Failed to read configuration file "
               "\"~s\": ~9999p", [?MODULE, Filename, Reason]),
            []
    end.

%% @doc Returns parsed all valid key-value pairs from
%%      configuration file body.
-spec parse_config(String :: string()) -> raw_config().
parse_config(String) ->
    lists:flatmap(
      fun parse_key_val_pair/1,
      erjik_lib:preparse_config(String)).

%% @doc Processes parsed key-value pairs and produces final
%%      application configuration database.
-spec assemble_config(CfgItems :: raw_config()) ->
                             {Config :: config(),
                              Classes :: classes(),
                              MimeTypes :: mime_types()}.
assemble_config(RawConfig) ->
    Config = assemble_simple(RawConfig),
    MimeTypesFilename = proplists:get_value(?CFG_MIME_TYPES, Config),
    {Config,
     _Classes = assemble_classes(RawConfig),
     _MimeTypes = read_mime_types(MimeTypesFilename)}.

%% @doc
-spec assemble_simple(RawConfig :: raw_config()) -> Config :: config().
assemble_simple(RawConfig) ->
    [begin
         ValuesFound = [V || {K, V} <- RawConfig, K == Key],
         {Key,
          case assemble_simple_key(Key, ValuesFound) of
              {ok, Value} ->
                  ?loginf(
                     "~w> config: '~w' set to: ~s",
                     [?MODULE, Key, cfg_to_list(Key, Value)]),
                  Value;
              undefined ->
                  Value = get_default_value(Key),
                  ?loginf(
                     "~w> config: '~w' set to default: ~s",
                     [?MODULE, Key, cfg_to_list(Key, Value)]),
                  Value
          end}
     end || Key <- ?CFGS_SIMPLE].

-spec assemble_simple_key(Key :: config_simple_key(), Values :: list()) ->
                                 {ok, Value :: any()} | undefined.
assemble_simple_key(_Key, _NoValues = []) ->
    undefined;
assemble_simple_key(Key, Values)
  when Key == ?CFG_IP_DENY_REDIRECT; Key == ?CFG_URL_DENY_REDIRECT ->
    case [NotEmpty || NotEmpty = [_ | _] <- Values] of
        [FirstNotEmpty | _] ->
            {ok, FirstNotEmpty};
        [] ->
            undefined
    end;
assemble_simple_key(Key, Values)
  when Key == ?CFG_PRIVILEGED; Key == ?CFG_ALLOW; Key == ?CFG_DENY ->
    {ok, erjik_lib:flatten_ip_pool(Values)};
assemble_simple_key(_Key, [FirstFound | _]) ->
    {ok, FirstFound}.

%% @doc Assemble classes configurations.
-spec assemble_classes(RawConfig :: raw_config()) ->
                              [{ClassName :: nonempty_string(),
                                Domains :: [nonempty_string()],
                                Regexps :: [nonempty_string()],
                                RedirectURL :: nonempty_string() | undefined
                               }].
assemble_classes(RawConfig) ->
    Classes =
        lists:flatmap(
          fun(ClassName) ->
                  case assemble_class(RawConfig, ClassName) of
                      {ClassName, [], [], _RedirectURL} ->
                          ?loginf(
                             "~w> config: class \"~s\" dropped off because"
                             " of empty list of domains and regexps",
                             [?MODULE, ClassName]),
                          [];
                      ClassData ->
                          [ClassData]
                  end
          end, erjik_lib:uniq([N || {{class, N, _}, _} <- RawConfig])),
    case Classes of
        [] ->
            ?logwrn(
               "~w> There is no meaning destination "
               "classes at all! Check your configuration.",
               [?MODULE]);
        _ -> nop
    end,
    Classes.

%% @doc
-spec assemble_class(RawConfig :: raw_config(),
                     ClassName :: nonempty_string()) ->
                            {ClassName :: nonempty_string(),
                             Domains :: [nonempty_string()],
                             Regexps :: [nonempty_string()],
                             RedirectURL :: nonempty_string() | undefined}.
assemble_class(RawConfig, ClassName) ->
    PL = [{K, V} || {{class, C, K}, V} <- RawConfig, C == ClassName],
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
     end}.

-spec parse_key_val_pair({LineNo :: non_neg_integer(),
                          StrKey :: nonempty_string(),
                          StrValue :: string()}) ->
                                [{Key :: raw_config_item_key(),
                                  Value :: any()}].
parse_key_val_pair({LineNo, StrKey, StrValue}) ->
    case parse_key(StrKey) of
        {ok, Key} ->
            case parse_val(Key, StrValue) of
                {ok, Value} ->
                    [{Key, Value}];
                error ->
                    ?logerr(
                       "~w> config: bad value on line ~w "
                       "for key '~s': \"~s\"",
                       [?MODULE, LineNo, StrKey, StrValue]),
                    []
            end;
        error ->
            ?logerr(
               "~w> config: bad key on line ~w: \"~s\"",
               [?MODULE, LineNo, StrKey]),
            []
    end.

-spec parse_key(String :: nonempty_string()) ->
                       {ok, Key :: raw_config_item_key()} | error.
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

-spec parse_class_key(String :: string(), Acc :: list()) ->
                             {ok, ClassName :: nonempty_string(),
                              ClassParam :: nonempty_string()} |
                             error.
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

-spec parse_val(Key :: raw_config_item_key(), Value :: string()) ->
                       {ok, Value :: any()} | error.
parse_val(Key, StrValue) ->
    try {ok, _Value} = parse_val_(Key, StrValue)
    catch _:_ -> error end.

-spec parse_val_(Key :: atom(), String :: string()) ->
                        {ok, Value :: any()}.
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
    {ok, erjik_lib:list_to_port_number(String)};
parse_val_(?CFG_WWW_ROOT, String) ->
    {ok, String};
parse_val_(?CFG_LOG_PATH, String) ->
    {ok, String};
parse_val_(?CFG_INSTANCE_ID, String) ->
    {ok, list_to_atom(String)};
parse_val_(?CFG_COOKIE, String) ->
    {ok, list_to_atom(String)};
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

-spec parse_ip_range(String :: string()) ->
                            {ok, any | none | erjik_lib:ip_range()}.
parse_ip_range(String) ->
    case string:to_lower(String) of
        "any" -> {ok, any};
        "all" -> {ok, any};
        "none" -> {ok, none};
        _ -> erjik_lib:list_to_ip_range(String)
    end.

%% ----------------------------------------------------------------------
%% low level parsers and data processing tools

-spec cfg_to_list(Key :: atom(), Value :: any()) -> string().
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
cfg_to_list(?CFG_MIME_TYPES, MimeTypes) -> MimeTypes;
cfg_to_list(?CFG_LOG_PATH, LogPath) -> LogPath;
cfg_to_list(?CFG_INSTANCE_ID, InstanceID) -> atom_to_list(InstanceID);
cfg_to_list(?CFG_COOKIE, Cookie) -> atom_to_list(Cookie).

-spec read_blacklist(Filename :: file:filename()) ->
                            [Line :: nonempty_string()].
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

%% ----------------------------------------------------------------------
%% mime types

%% @doc
-spec read_mime_types(Filename :: file:filename()) -> mime_types().
read_mime_types(Filename) ->
    [{string:to_lower(Ext), string:to_lower(Name)} ||
        {Name, List} <- read_mime_types_file(Filename),
        Ext <- List].

%% @doc
-spec read_mime_types_file(Filename :: file:filename()) ->
                                  [{MimeType :: nonempty_string(),
                                    Extensions :: [nonempty_string()]}].
read_mime_types_file(Filename) ->
    case erjik_lib:read_mime_types(Filename) of
        {ok, List} ->
            List;
        {error, Reason} ->
            ?logerr(
               "~w> failed to read mime types from \"~s\": ~9999p. "
               "Default mime table will be used.",
               [?MODULE, Filename, Reason]),
            default_mime_types()
    end.

%% @doc
-spec default_mime_types() -> [{MimeType :: nonempty_string(),
                                Extensions :: [nonempty_string()]}].
default_mime_types() ->
    [{"text/html", ["htm", "html"]},
     {"text/plain", ["txt"]},
     {"text/css", ["css"]}].

