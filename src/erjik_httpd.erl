%%% @doc
%%% Callback module for httpd behaviour.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 17 Oct 2013
%%% @copyright 2009-2012, Aleksey Morarash

-module(erjik_httpd).

%% This module implements httpd behaviour, but httpd module
%% does not defined the behaviour. Commented out to supress
%% compiler warning.
%%-behaviour(httpd).

%% API exports
-export([start/3]).

%% httpd callbacks
-export([do/1, load/2, store/2, remove/1]).

-include("erjik.hrl").

-ifndef(WITHOUT_INETS_HEADER).
-include_lib("inets/include/httpd.hrl").
-else.
-record(mod,
        {init_data,
         data=[],
         socket_type=ip_comm,
         socket,
         config_db,
         method,
         absolute_uri=[],
         request_uri,
         http_version,
         request_line,
         parsed_header=[],
         entity_body,
         connection}).
-endif.

%% @doc Start the server.
-spec start(IP :: inet:ip_address(),
            Port :: inet:port_number(),
            WwwRoot :: file:filename()) ->
                   {ok, Pid :: pid()} | {error, Reason :: any()}.
start(IP, Port, WwwRoot) ->
    try
        IpFamily = erjik_lib:ip_family(IP),
        Result =
            inets:start(
              httpd,
              [{port, Port},
               {bind_address,
                if IP == {0,0,0,0} -> any;
                   IP == {0,0,0,0,0,0,0,0} -> any;
                   true -> IP
                end},
               {server_name, "erjik"},
               {server_root, "."},
               {document_root, WwwRoot},
               {ipfamily, IpFamily},
               {modules, [?MODULE]}
              ], stand_alone),
        case Result of
            {ok, Pid} = Ok when is_pid(Pid) ->
                Ok;
            Other ->
                {error, {bad_return, Other}}
        end
    catch
        Type:Reason ->
            {error, {Type, Reason, erlang:get_stacktrace()}}
    end.

%% ----------------------------------------------------------------------
%% httpd callback functions
%% ----------------------------------------------------------------------

%% @hidden
-spec do(ModData :: #mod{}) ->
                {proceed, list()} |
                {break, list()}.
do(ModData) ->
    {RelFilename0, _} = split4pathNquery(ModData#mod.request_uri),
    RelFilename = string:strip(RelFilename0, left, $/),
    ?logdbg("~w> file requested: '~s'", [?MODULE, RelFilename]),
    Filename =
        filename:join(erjik_cfg:get(?CFG_WWW_ROOT), RelFilename),
    MimeType = erjik_cfg:mime_type(Filename),
    case file:read_file_info(Filename) of
        {ok, FileInfo} ->
            Headers =
                [{content_type, MimeType},
                 {content_length,
                  integer_to_list(FileInfo#file_info.size)},
                 {last_modified,
                  httpd_util:rfc1123_date(FileInfo#file_info.mtime)}
                ],
            case file:read_file(Filename) of
                {ok, Binary} ->
                    httpd_response:send_header(ModData, 200, Headers),
                    httpd_socket:deliver(
                      ModData#mod.socket_type,
                      ModData#mod.socket, Binary),
                    {proceed,
                     [{response,
                       {already_sent, 200, FileInfo#file_info.size}},
                      {mime_type, MimeType} |
                      ModData#mod.data]};
                {error, _Reason} ->
                    ?logdbg("~w> '~s' not found", [?MODULE, Filename]),
                    {break, [{response, {404, "Not Found"}}]}
            end;
        {error, _Reason} ->
            ?logdbg("~w> '~s' not found", [?MODULE, Filename]),
            {break, [{response, {404, "Not Found"}}]}
    end.

%% @hidden
-spec load(Line :: string(),
           AccIn :: [{Option :: httpd:property(), Value :: any()}]) -> ok.
load(_Line, _AccIn) ->
    ok.

%% @hidden
-spec store(OptVal :: {Option :: httpd:property(), Value :: any()},
            Config :: [{Option :: httpd:property(), Value :: any()}]) ->
                   {ok, {Option :: httpd:property(), Value :: any()}}.
store(OptVal, _Config) ->
    {ok, OptVal}.

%% @hidden
-spec remove(ConfigDB :: ets:tab()) -> ok.
remove(_ConfigDB) ->
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% @doc Splits request URI to Path and Query strings (delimited by '?').
-spec split4pathNquery(RequestURI :: string()) ->
                              {Path :: string(), Query :: string()}.
split4pathNquery(RequestURI) ->
    split4pathNquery(RequestURI, []).

-spec split4pathNquery(String :: string(), Acc :: string()) ->
                              {Path :: string(), Query :: string()}.
split4pathNquery([$? | Tail], Path) ->
    {lists:reverse(Path), Tail};
split4pathNquery([H | Tail], Path) ->
    split4pathNquery(Tail, [H | Path]);
split4pathNquery(_, Path) ->
    {lists:reverse(Path), []}.

