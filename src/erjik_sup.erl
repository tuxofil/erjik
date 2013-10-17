%%% @doc
%%% Main supervisor module.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash

-module(erjik_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("erjik.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start supervisor process as part of supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link(?MODULE, no_args).

%% ----------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------

%% @doc Creates supervisor configuration.
%% @hidden
-spec init(Args :: any()) ->
                  {ok,
                   {{RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                    [ChildSpec :: supervisor:child_spec()]}}.
init(_Args) ->
    {ok,
     {{one_for_one, 5, 1},
      [
       %% logger
       {erjik_log, {erjik_log, start_link, []},
        permanent, 100, worker, [erjik_log]},
       
       %% configuration facility handler
       {erjik_cfg, {erjik_cfg, start_link, []},
        permanent, 100, worker, [erjik_cfg]},
       
       %% regexps keeper
       {erjik_re, {erjik_re, start_link, []},
        permanent, 100, worker, [erjik_re]},

       %% httpd
       {erjik_httpd_warden, {erjik_httpd_warden, start_link, []},
        permanent, 100, worker, [erjik_httpd_warden]},

       %% main process
       {erjik_srv, {erjik_srv, start_link, []},
        permanent, 100, worker, [erjik_srv]}
      ]
     }}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

