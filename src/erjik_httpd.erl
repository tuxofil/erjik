%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 30 Mar 2012
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc HTTP server process warden.

-module(erjik_httpd).

-behaviour(gen_server).

%% API exports
-export([start_link/0, hup/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

%% httpd callbacks
-export([do/1, load/2, store/2, remove/1]).

-include("erjik.hrl").
-include_lib("kernel/include/file.hrl").

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

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start HTTP server warden process as part of supervision tree.
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends signal to HTTP server warden process to
%%      reread its configurations.
%% @spec hup() -> ok
hup() ->
    gen_server:cast(?MODULE, ?SIG_RECONFIG).

%% --------------------------------------------------------------------
%% Callback functions
%% --------------------------------------------------------------------

%% state record
-record(state,
        {bind_ip,
         bind_port,
         www_root,
         server
        }).

%% @hidden
init(_Init) ->
    process_flag(trap_exit, true),
    hup(),
    ?loginf("~w> started", [?MODULE]),
    {ok, #state{}}.

%% @hidden
handle_info({'EXIT', From, Reason}, State)
  when is_pid(From), From == State#state.server ->
    ?logwrn("~w> died: ~99999p", [?MODULE, Reason]),
    schedule_restart(),
    {noreply, State#state{server = undefined}};
handle_info({'EXIT', _From, _Reason}, State) ->
    {noreply, State};
handle_info(Request, State) ->
    ?logwrn("~w> unknown info: ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
handle_call(Request, From, State) ->
    ?logwrn("~w> unknown call ~9999p from ~9999p",
            [?MODULE, Request, From]),
    {noreply, State}.

%% @hidden
handle_cast(?SIG_RECONFIG, State) ->
    ?loginf("~w> reconfig requested", [?MODULE]),
    BindIP = erjik_cfg:get(?CFG_BIND_IP),
    BindPt = erjik_cfg:get(?CFG_BIND_PORT),
    WwwRoot = erjik_cfg:get(?CFG_WWW_ROOT),
    NeedToRestart =
        not is_pid(State#state.server) orelse
        BindIP /= State#state.bind_ip orelse
        BindPt /= State#state.bind_port orelse
        WwwRoot /= State#state.www_root,
    if NeedToRestart ->
            catch exit(State#state.server, kill),
            case start_server(BindIP, BindPt, WwwRoot) of
                {ok, Pid} ->
                    ?loginf(
                       "~w> listening on ~s",
                       [?MODULE,
                        erjik_lib:socket_to_list(BindIP, BindPt)]),
                    {noreply,
                     State#state{
                       bind_ip = BindIP,
                       bind_port = BindPt,
                       www_root = WwwRoot,
                       server = Pid
                      }};
                {error, Reason} ->
                    ?logerr(
                       "~w> failed to listen on ~s: ~9999p",
                       [?MODULE,
                        erjik_lib:socket_to_list(BindIP, BindPt),
                        Reason]),
                    schedule_restart(),
                    {noreply, State#state{server = undefined}}
            end;
       true ->
            ?logdbg("~w> configuration remains the same",
                    [?MODULE]),
            {noreply, State}
    end;
handle_cast(Request, State) ->
    ?logwrn("~w> unknown cast: ~9999p", [?MODULE, Request]),
    {noreply, State}.

%% @hidden
terminate(Reason, State) ->
    catch exit(State#state.server, kill),
    ?loginf("~w> terminate(~9999p, ~9999p)",
            [?MODULE, Reason, State]).

%% @hidden
code_change(OldVsn, State, Extra) ->
    ?loginf("~w> code_change(~9999p, ~9999p, ~9999p)",
            [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

%% ----------------------------------------------------------------------
%% httpd callback functions
%% ----------------------------------------------------------------------

-define(mime_text_plain, "text/plain").
-define(mime_text_html, "text/html").

%% @hidden
do(ModData) ->
    {RelFilename, _} = split4pathNquery(ModData#mod.request_uri),
    Filename =
        filename:join(erjik_cfg:get(?CFG_WWW_ROOT), RelFilename),
    MimeType =
        %% todo: get mime type by filename suffix
        ?mime_text_html,
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
                    {break, [{response, {404, "Not Found"}}]}
            end;
        {error, _Reason} ->
            {break, [{response, {404, "Not Found"}}]}
    end.

%% @hidden
load(_Line, _AccIn) -> ok.

%% @hidden
store(OptVal, _Config) -> {ok, OptVal}.

%% @hidden
remove(_ConfigDB) -> ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

start_server(IP, Port, WwwRoot) ->
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

schedule_restart() ->
    Millis = 5000,
    spawn(
      fun() ->
              ?logdbg(
                 "~w> scheduling respawn after ~w millis...",
                 [?MODULE, Millis]),
              timer:sleep(Millis),
              hup()
      end).

%% @doc Splits request URI to Path and Query strings (delimited by '?').
%% @spec split4pathNquery(RequestURI) -> {Path, Query}
%%     RequestURI = string(),
%%     Path = string(),
%%     Query = string()
split4pathNquery(RequestURI) ->
    split4pathNquery(RequestURI, []).
split4pathNquery([$? | Tail], Path) ->
    {lists:reverse(Path), Tail};
split4pathNquery([H | Tail], Path) ->
    split4pathNquery(Tail, [H | Path]);
split4pathNquery(_, Path) ->
    {lists:reverse(Path), []}.

