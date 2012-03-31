%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Main supervisor module

-module(erjik_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("erjik.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start supervisor process as part of supervision tree.
%% @spec start_link() -> {ok, pid()}
start_link() ->
    supervisor:start_link(?MODULE, no_args).

%% ----------------------------------------------------------------------
%% supervisor callbacks
%% ----------------------------------------------------------------------

%% @doc Creates supervisor configuration.
%% @hidden
%% @spec init(Args) -> {ok, SupervisorSpec}
%%     Args = term(),
%%     SupervisorSpec = term()
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
       
       %% httpd
       {erjik_httpd, {erjik_httpd, start_link, []},
        permanent, 100, worker, [erjik_httpd]},

       %% main process
       {erjik_srv, {erjik_srv, start_link, []},
        permanent, 100, worker, [erjik_srv]}
      ]
     }}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

