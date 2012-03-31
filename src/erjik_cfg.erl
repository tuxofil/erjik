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
            classify(
              URL, Hostname, IsIP,
              ?MODULE:get(?CFG_CLASS_NAMES));
        _ -> undefined
    end.
classify(_URL, _Hostname, _IsIP, []) -> undefined;
classify(URL, Hostname, IsIP, [{ClassName, RedirectURL} | Tail]) ->
    case classify_hostname(ClassName, Hostname, IsIP) of
        true -> {ok, ClassName, RedirectURL};
        _ ->
            case classify_url(ClassName, URL) of
                true -> {ok, ClassName, RedirectURL};
                _ ->
                    classify(URL, Hostname, IsIP, Tail)
            end
    end.

%% @doc Sends 'reconfig' signal to configuration facility process.
%% @spec hup() -> ok
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% ----------------------------------------------------------------------
%% Callback functions
%% ----------------------------------------------------------------------

%% state record
-record(state, {}).

%% @hidden
init(_Args) ->
    ets:new(?FAC_CONFIGS, [named_table]),
    ets:new(?FAC_DOMAINS, [named_table]),
    ets:new(?FAC_REGEXPS, [named_table, ordered_set]),
    hup(),
    ?loginf("~w> started", [?MODULE]),
    {ok, #state{}}.

%% @hidden
handle_info(Request, State) ->
    ?logwrn("~w> unknown info ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
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
    catch erjik_srv:hup(),
    catch erjik_httpd:hup(),
    ok.

%% @doc Returns default value for configuration parameter.
%% @spec get_default_value(Key) -> Value
%%     Key = atom(),
%%     Value = term()
get_default_value(?CFG_LOGLEVEL) -> ?LOGLEVEL_WARNING;
get_default_value(?CFG_IP_DENY_REDIRECT) ->
    "http://127.0.0.1:8888/denied.html";
get_default_value(?CFG_ORDER) -> [?CFG_ALLOW, ?CFG_DENY];
get_default_value(?CFG_PRIVILEGED) -> [any];
get_default_value(?CFG_ALLOW) -> [any];
get_default_value(?CFG_DENY) -> [];
get_default_value(?CFG_DEFAULT_POLICY) -> ?CFG_ALLOW;
get_default_value(?CFG_BIND_IP) -> {127,0,0,1};
get_default_value(?CFG_BIND_PORT) -> 8888;
get_default_value(?CFG_WWW_ROOT) -> "./www/";
get_default_value(?CFG_CLASS_NAMES) -> [];
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
%% @spec assemble_config(ConfigItems) -> {Config, Classes}
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
%%         RedirectURL = undefined | string()
assemble_config(CfgItems) ->
    Config =
        [{?CFG_LOGLEVEL,
          case [V || {?CFG_LOGLEVEL, V} <- CfgItems] of
              [LogLevel | _] -> LogLevel;
              _ -> set_default(?CFG_LOGLEVEL)
          end},
         {?CFG_IP_DENY_REDIRECT,
          case [V || {?CFG_IP_DENY_REDIRECT, [_ | _] = V} <- CfgItems] of
              [IpDenyRedirect | _] -> IpDenyRedirect;
              _ -> set_default(?CFG_IP_DENY_REDIRECT)
          end},
         {?CFG_ORDER,
          case [V || {?CFG_ORDER, V} <- CfgItems] of
              [Order | _] -> Order;
              _ -> set_default(?CFG_ORDER)
          end},
         {?CFG_PRIVILEGED,
          case [V || {?CFG_PRIVILEGED, V} <- CfgItems] of
              [_ | _] = Privileged ->
                  erjik_lib:flatten_ip_pool(Privileged);
              _ -> set_default(?CFG_PRIVILEGED)
          end},
         {?CFG_ALLOW,
          case [V || {?CFG_ALLOW, V} <- CfgItems] of
              [_ | _] = Allow ->
                  erjik_lib:flatten_ip_pool(Allow);
              _ -> set_default(?CFG_ALLOW)
          end},
         {?CFG_DENY,
          case [V || {?CFG_DENY, V} <- CfgItems] of
              [_ | _] = Deny ->
                  erjik_lib:flatten_ip_pool(Deny);
              _ -> set_default(?CFG_DENY)
          end},
         {?CFG_DEFAULT_POLICY,
          case [V || {?CFG_DEFAULT_POLICY, V} <- CfgItems] of
              [DefaultPolicy | _] -> DefaultPolicy;
              _ -> set_default(?CFG_DEFAULT_POLICY)
          end},
         {?CFG_BIND_IP,
          case [V || {?CFG_BIND_IP, V} <- CfgItems] of
              [BindIP | _] -> BindIP;
              _ -> set_default(?CFG_BIND_IP)
          end},
         {?CFG_BIND_PORT,
          case [V || {?CFG_BIND_PORT, V} <- CfgItems] of
              [BindPort | _] -> BindPort;
              _ -> set_default(?CFG_BIND_PORT)
          end},
         {?CFG_WWW_ROOT,
          case [V || {?CFG_WWW_ROOT, V} <- CfgItems] of
              [WwwRoot | _] -> WwwRoot;
              _ -> set_default(?CFG_WWW_ROOT)
          end},
         {?CFG_CLASS_NAMES, []}],
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
    {Config, Classes}.

%% @doc Inserts configuration data to database (ETSes)
%% @spec apply_config(Configs) -> ok
%%     Configs = term()
apply_config({Config, Classes}) ->
    DefaultPolicy = proplists:get_value(?CFG_DEFAULT_POLICY, Config),
    TmpConfig =
        [{?CFG_DEFAULT_POLICY, ?CFG_ALLOW} |
         [I || {K, _} = I <- Config, K /= ?CFG_DEFAULT_POLICY]],
    ets:insert(?FAC_CONFIGS, TmpConfig),
    ets:delete_all_objects(?FAC_DOMAINS),
    ets:delete_all_objects(?FAC_REGEXPS),
    lists:foreach(
      fun({ClassName, Domains, Regexps, _URL}) ->
              ets:insert(
                ?FAC_DOMAINS,
                [{{ClassName, D}, ok} || D <- Domains]),
              ets:insert(?FAC_REGEXPS, {ClassName, Regexps})
      end, Classes),
    ClassNames = [{N, U} || {N, _, _, U} <- Classes],
    ets:insert(
      ?FAC_CONFIGS,
      [{?CFG_DEFAULT_POLICY, DefaultPolicy},
       {?CFG_CLASS_NAMES, ClassNames}]),
    ok.

%% ----------------------------------------------------------------------
%% low level parsers and data processing tools

set_default(Key) ->
    Value = get_default_value(Key),
    ?loginf(
       "~w> config: '~w' set to default: ~9999p",
       [?MODULE, Key, Value]),
    Value.

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
parse_val_(?CFG_DEFAULT_POLICY, String) ->
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
    {ok, String}.

parse_ip_range(String) ->
    case string:to_lower(String) of
        "any" -> {ok, any};
        "all" -> {ok, any};
        "none" -> {ok, none};
        _ -> erjik_lib:list_to_ip_range(String)
    end.

classify_hostname(ClassName, StrIP, true) ->
    case ets:lookup(?FAC_DOMAINS, {ClassName, StrIP}) of
        [_] -> true;
        _ -> false
    end;
classify_hostname(ClassName, Domain, _) ->
    classify_domain(
      ClassName, lists:reverse(string:tokens(Domain, "."))).

classify_domain(ClassName, [_ | Tail] = Tokens) ->
    Domain = string:join(lists:reverse(Tokens), "."),
    case ets:lookup(?FAC_DOMAINS, {ClassName, Domain}) of
        [_] -> true;
        _ -> classify_domain(ClassName, Tail)
    end;
classify_domain(_, _) -> false.

classify_url(ClassName, URL) ->
    case ets:lookup(?FAC_REGEXPS, ClassName) of
        [{_, [_ | _] = List}] ->
            lists:any(
              fun(Regexp) ->
                      case re:run(URL, Regexp, []) of
                          {match, _} -> true;
                          _ -> false
                      end
              end, List);
        _ -> false
    end.

