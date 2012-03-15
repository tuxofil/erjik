%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Main process

-module(erjik_srv).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% --------------------------------------------------------------------
%% Callback functions
%% --------------------------------------------------------------------

%% server state record
-record(state, {}).

init(_Init) ->
    ?log(10, "~w started", [?MODULE]),
    {ok, #state{}}.

handle_info(Request, State) ->
    ?log(7, "~w unknown info: ~9999p", [?MODULE, Request]),
    {noreply, State}.

handle_call({request, Data}, From, State) ->
    ?log(30, "~w got request: ~9999p", [?MODULE, Data]),
    spawn_link(fun() -> gen_server:reply(From, handle_data(Data)) end),
    {noreply, State};
handle_call(Request, From, State) ->
    ?log(7, "~w unknown call ~9999p from ~9999p",
         [?MODULE, Request, From]),
    {noreply, State}.

handle_cast(Request, State) ->
    ?log(7, "~w unknown cast: ~9999p", [?MODULE, Request]),
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

handle_data(Data) ->
    [ReqId, Url, Ip | _] = string:tokens(Data, " "),
    try
        {ok, ParsedIp} = parse_address(Ip),
        ?log(25, "~w: request from ~w to ~s",
             [?MODULE, ParsedIp, Url]),
        case process(Url, ParsedIp) of
            ok ->
                Data;
            {redirect, NewUrl} ->
                io_lib:format("~s ~s~n", [ReqId, NewUrl])
        end
    catch
        _:Reason ->
            ?log(5, "~w: failed process request \"~s\" due to ~w",
                 [?MODULE, Data, Reason]),
            Data
    end.

parse_address(Ip) ->
    [Ip2 | _] = string:tokens(Ip, "/"),
    inet_parse:address(Ip2).

%% @spec process(Url, IP) -> ok | {redirect, NewUrl}
%%     Url = string(),
%%     NewUrl = string(),
%%     IP = ip_address()
process(Url, IP) ->
    case classify_ip(IP) of
        ?CONF_PRIVILEGED ->
            ?log(25, "~w: ~w -> ~s -> ok", [?MODULE, IP, Url]),
            ok;
        ?CONF_DENY ->
            {ok, Destination} =
                erjik_cfg:get(?CONF_IP_DENY_URL, Url),
            ?log(7, "~w: ~w -> ~s -> ~s",
                 [?MODULE, IP, Url, Destination]),
            {redirect, Destination};
        ?CONF_ALLOW ->
	    Domain = strip_hostname(strip_proto(Url)),
            case erjik_cfg:lookup(Domain) of
                undefined ->
                    case erjik_cfg:match(Url) of
                        undefined ->
                            ?log(30, "~w: ~w -> ~s -> ok",
                                 [?MODULE, IP, Url]),
                            ok;
                        {ok, _Class, pass} ->
                            ?log(25, "~w: ~w -> ~s -> ok",
                                 [?MODULE, IP, Url]),
                            ok;
                        {ok, _Class, Destination} ->
                            ?log(7, "~w: ~w -> ~s -> ~s (by regexp)",
                                 [?MODULE, IP, Url, Destination]),
                            {redirect, Destination}
                    end;
                {ok, _Class, pass} ->
                    ?log(25, "~w: ~w -> ~s -> ok",
                         [?MODULE, IP, Url]),
                    ok;
                {ok, _Class, Destination} ->
                    ?log(7, "~w: ~w -> ~s -> ~s",
                         [?MODULE, IP, Url, Destination]),
                    {redirect, Destination}
            end
    end.

classify_ip(Ip) ->
    {ok, Order} = erjik_cfg:get(?CONF_ORDER, []),
    classify_ip(Ip, Order).

classify_ip(_Ip, []) ->
    ?CONF_DENY;
classify_ip(Ip, [Class | T]) ->
    {ok, Value} = erjik_cfg:get(Class, []),
    case erjik_lib:in_ippool(Ip, Value) of
        true -> Class;
        false -> classify_ip(Ip, T)
    end.

strip_proto([]) -> [];
strip_proto("://" ++ Url) -> Url;
strip_proto([_H | Tail]) -> strip_proto(Tail).

strip_hostname(String) ->
    lists:reverse(strip_hostname(String, [])).
strip_hostname([], Result) -> Result;
strip_hostname([$/ | _Tail], Result) -> Result;
strip_hostname([Char | T], Result) ->
    strip_hostname(T, [Char | Result]).

