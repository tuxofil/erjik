%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Main supervisor module

-module(erjik_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, no_args).

%% --------------------------------------------------------------------
%% Callback functions
%% --------------------------------------------------------------------

%% creates erjik supervisor configuration
init(_Args) ->
    {ok,
     {
       {rest_for_one, 5, 1},
       [
        %% Configuration facility
        {erjik_cfg, {erjik_cfg, start_link, []},
         permanent, 100, worker, [erjik_cfg]},

        %% logger
        {erjik_log, {erjik_log, start_link, []},
         permanent, 100, worker, [erjik_log]},

        %% erjik main process
        {erjik_srv, {erjik_srv, start_link, []},
         permanent, 100, worker, [erjik_srv]}
       ]
     }}.

