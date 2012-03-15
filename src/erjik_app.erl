%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Application callback

-module(erjik_app).

-behaviour(application).

%% callback functions
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

%% --------------------------------------------------------------------
%% Callback functions
%% --------------------------------------------------------------------

start(_Type, _StartArgs) ->
    erjik_sup:start_link().

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

prep_stop(_State) ->
    ok.

stop(_State) ->
    ok.

config_change(_Changed, _New, _Removed) ->
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

