%%% @doc
%%% Main process.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash

-module(erjik_srv).

%% API exports
-export([start_link/0]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start main process as part of supervision tree.
-spec start_link() -> {ok, Pid :: pid()}.
start_link() ->
    {ok, spawn_link(fun init/0)}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec init() -> no_return().
init() ->
    register(?MODULE, self()),
    ?loginf("~w> started", [?MODULE]),
    loop().

-spec loop() -> no_return().
loop() ->
    case io:get_line("") of
        eof ->
            ?loginf("~w> input ends", [?MODULE]),
            ok = erjik_log:flush(),
            halt(0);
        {error, Reason} ->
            ?logerr("~w> read error: ~9999p", [?MODULE, Reason]),
            ok = erjik_log:flush(),
            halt(1);
        Request ->
            spawn(
              fun() ->
                      io:format("~s", [handle_request(Request)])
              end),
            loop()
    end.

%% @doc
-spec handle_request(Request :: string()) -> Reply :: iolist().
handle_request(Request) ->
    case parse_request(Request) of
        {ok, ReqID, RequestedURL, UserIP} ->
            [ReqID, $\s,
             get_destination_url(UserIP, RequestedURL), $\n];
        error ->
            Request
    end.

%% @doc
-spec get_destination_url(UserIP :: inet:ip_address(),
                          RequestedURL :: nonempty_string()) ->
                                 DestinationURL :: nonempty_string().
get_destination_url(UserIP, RequestedURL) ->
    case process(UserIP, RequestedURL) of
        ok ->
            ?logdbg(
               "~w> pass ~s to ~s",
               [?MODULE, erjik_lib:ip_to_list(UserIP), RequestedURL]),
            RequestedURL;
        {redirect, RedirectURL} ->
            ?logdbg(
               "~w> redirecting ~s to ~s",
               [?MODULE, erjik_lib:ip_to_list(UserIP), RedirectURL]),
            RedirectURL
    end.

%% @doc
-spec parse_request(Request :: string()) ->
                           {ok,
                            ReqID :: nonempty_string(),
                            URL :: nonempty_string(),
                            IP :: inet:ip_address()} |
                           error.
parse_request(Request) ->
    case string:tokens(Request, " \t\r\n") of
        [[_ | _] = ReqID, [_ | _] = URL, [_ | _] = StrIP | _] ->
            case erjik_lib:list_to_ip(strip_suffix("/-", StrIP)) of
                {ok, IP} ->
                    ?logdbg(
                       "~w> request from ~s to ~s",
                       [?MODULE, erjik_lib:ip_to_list(IP), URL]),
                    {ok, ReqID, URL, IP};
                _ ->
                    ?logerr(
                       "~w> failed to parse IP: \"~s\"",
                       [?MODULE, StrIP]),
                    error
            end;
        _ ->
            ?logerr(
               "~w> malformed request: ~99999p",
               [?MODULE, Request]),
            error
    end.

%% @doc
-spec strip_suffix(Suffix :: string(), String :: string()) ->
                          NewString :: string().
strip_suffix(Suffix, String) ->
    case lists:suffix(Suffix, String) of
        true ->
            lists:sublist(String, length(String) - length(Suffix));
        false ->
            String
    end.

%% @doc
-spec process(UserIP :: inet:ip_address(),
              RequestedURL :: nonempty_string()) ->
                     ok | {redirect, RedirectUrl :: nonempty_string()}.
process(UserIP, RequestedURL) ->
    case is_privileged(UserIP) of
        true ->
            %% privileged IP found. Allow all.
            ok;
        false ->
            case is_ip_allowed(UserIP) of
                ok ->
                    classify(RequestedURL);
                {redirect, _RedirectURL} = Redirect ->
                    Redirect
            end
    end.

%% @doc
-spec is_privileged(IP :: inet:ip_address()) -> boolean().
is_privileged(IP) ->
    erjik_lib:is_in_ip_pool(IP, erjik_cfg:get(?CFG_PRIVILEGED)).

%% @doc
-spec is_ip_allowed(IP :: inet:ip_address()) ->
                           ok | {redirect, URL :: nonempty_string()}.
is_ip_allowed(IP) ->
    is_ip_allowed_loop(erjik_cfg:get(?CFG_ORDER), IP).

%% @doc
-spec is_ip_allowed_loop(CheckOrder :: [?CFG_ALLOW | ?CFG_DENY],
                         IP :: inet:ip_address()) ->
                                ok | {redirect, URL :: nonempty_string()}.
is_ip_allowed_loop([?CFG_ALLOW | Tail], IP) ->
    case erjik_lib:is_in_ip_pool(IP, erjik_cfg:get(?CFG_ALLOW)) of
        true ->
            ok;
        false ->
            is_ip_allowed_loop(Tail, IP)
    end;
is_ip_allowed_loop([?CFG_DENY | Tail], IP) ->
    case erjik_lib:is_in_ip_pool(IP, erjik_cfg:get(?CFG_DENY)) of
        true ->
            {redirect, erjik_cfg:get(?CFG_IP_DENY_REDIRECT)};
        false ->
            is_ip_allowed_loop(Tail, IP)
    end;
is_ip_allowed_loop([], _IP) ->
    case erjik_cfg:get(?CFG_IP_DEFAULT_POLICY) of
        ?CFG_ALLOW ->
            ok;
        ?CFG_DENY ->
            {redirect, erjik_cfg:get(?CFG_IP_DENY_REDIRECT)}
    end.

%% @doc
-spec classify(URL :: nonempty_string()) ->
                      ok | {redirect, RedirectURL :: nonempty_string()}.
classify(URL) ->
    case erjik_cfg:classify(URL) of
        {ok, _ClassName, [_ | _] = RedirectURL} ->
            {redirect, RedirectURL};
        {ok, _ClassName, _} ->
            ok;
        undefined ->
            case erjik_cfg:get(?CFG_URL_DEFAULT_POLICY) of
                ?CFG_DENY ->
                    {redirect, erjik_cfg:get(?CFG_URL_DENY_REDIRECT)};
                ?CFG_ALLOW ->
                    ok
            end
    end.

