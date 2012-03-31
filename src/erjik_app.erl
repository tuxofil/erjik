%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Application callback

-module(erjik_app).

-behaviour(application).

%% callback exports
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

%% --------------------------------------------------------------------
%% application callbacks
%% --------------------------------------------------------------------

%% @hidden
start(_Type, _StartArgs) ->
    erjik_sup:start_link().

%% @hidden
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

%% @hidden
prep_stop(_State) ->
    ok.

%% @hidden
stop(_State) ->
    ok.

%% @hidden
config_change(_Changed, _New, _Removed) ->
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

