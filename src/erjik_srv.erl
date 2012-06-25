%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc Main process

-module(erjik_srv).

%% API exports
-export([start_link/0]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start main process as part of supervision tree.
%% @spec start_link() -> {ok, pid()}
start_link() ->
    {ok, spawn_link(fun init/0)}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

init() ->
    register(?MODULE, self()),
    ?loginf("~w> started", [?MODULE]),
    loop().

loop() ->
    case io:get_line("") of
        eof ->
            ?loginf("~w> input ends", [?MODULE]),
            erjik:shutdown(0);
        {error, Reason} ->
            ?logerr("~w> read error: ~9999p", [?MODULE, Reason]),
            erjik:shutdown(1);
        Request ->
            spawn(
              fun() ->
                      io:format("~s", [handle_request(Request)])
              end),
            loop()
    end.

%% @spec handle_request(Request) -> Reply
%%     Request = string(),
%%     Reply = string()
handle_request(Request) ->
    case parse_request(Request) of
        {ok, ReqID, URL, IP} ->
            try throw(process(URL, IP))
            catch
                _:ok ->
                    ?logdbg(
                       "~w> pass ~s to ~s",
                       [?MODULE, erjik_lib:ip_to_list(IP), URL]),
                    ReqID ++ " " ++ URL ++ "\n";
                _:{redirect, NewURL} ->
                    ?logdbg(
                       "~w> redirecting ~s to ~s",
                       [?MODULE, erjik_lib:ip_to_list(IP), NewURL]),
                    io_lib:format("~s ~s~n", [ReqID, NewURL]);
                Type:Reason ->
                    FinalReason =
                        {Type, Reason, erlang:get_stacktrace()},
                    ?logerr(
                       "~w> failed process request \"~s\" due to ~w",
                       [?MODULE, Request, FinalReason]),
                    Request
            end;
        _ -> Request
    end.

parse_request(Request) ->
    case string:tokens(Request, " \t\r\n") of
        [[_ | _] = ReqID, [_ | _] = URL, [_ | _] = StrIP | _] ->
            case erjik_lib:list_to_ip(StrIP) of
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
%% @spec process(URL, IP) -> ok | {redirect, NewUrl}
%%     URL = string(),
%%     IP = inet:ip_address(),
%%     NewUrl = string()
process(URL, IP) ->
    case erjik_lib:is_in_ip_pool(
           IP, erjik_cfg:get(?CFG_PRIVILEGED)) of
        true ->
            %% privileged IP found. Allow all.
            throw(ok);
        _ -> nop
    end,
    Matched =
        lists:any(
          fun(?CFG_ALLOW) ->
                  Pool = erjik_cfg:get(?CFG_ALLOW),
                  erjik_lib:is_in_ip_pool(IP, Pool);
             (?CFG_DENY) ->
                  Pool = erjik_cfg:get(?CFG_DENY),
                  case erjik_lib:is_in_ip_pool(IP, Pool) of
                      true ->
                          throw(
                            {redirect,
                             erjik_cfg:get(?CFG_IP_DENY_REDIRECT)});
                      _ -> false
                  end
          end, erjik_cfg:get(?CFG_ORDER)),
    case {erjik_cfg:get(?CFG_IP_DEFAULT_POLICY), Matched} of
        {?CFG_DENY, false} ->
            %% no matches found but default policy is 'deny'
            throw({redirect,
                   erjik_cfg:get(?CFG_IP_DENY_REDIRECT)});
        _ -> nop
    end,
    %% IP filtering done.
    %% Examine destination URL...
    case erjik_cfg:classify(URL) of
        {ok, _ClassName, [_ | _] = RedirectURL} ->
            {redirect, RedirectURL};
        {ok, _ClassName, _} ->
            ok;
        _ ->
            case erjik_cfg:get(?CFG_URL_DEFAULT_POLICY) of
                ?CFG_DENY ->
                    {redirect,
                     erjik_cfg:get(?CFG_URL_DENY_REDIRECT)};
                _ -> ok
            end
    end.

