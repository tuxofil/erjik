%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc erjik start/stop interface

-module(erjik).

-export(
   [start/0,
    stop/0,
    restart/0,
    redirector/0,
    redirector2/0,
    reconf/0
   ]).

-export([process_data/1]).

-include("erjik.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

restart() ->
    stop(),
    start().

redirector() ->
    redirector_loop().

redirector2() ->
    timer:sleep(2500),
    redirector("reqs").

redirector(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    {ok, Requests} = regexp:split(binary_to_list(Binary), "\n"),
    lists:foreach(
      fun([]) ->
              ok;
         (Request) ->
              spawn(
                fun() ->
                        process_data(Request ++ "\n")
                end)
      end, Requests).

%% @spec reconf() -> ok | {error, Reason}
%%     Reason = term()
reconf() ->
    erjik_cfg:reconfig(),
    erjik_log:reconfig(),
    ok.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

redirector_loop() ->
    case io:get_line("") of
        eof ->
            ?log(10, "redirector input ends", []);
        {error, Reason} ->
            ?log(5, "redirector read error: ~p", [Reason]);
        Data ->
            spawn(fun() -> process_data(Data) end),
            redirector_loop()
    end.

process_data(Data) ->
    try
	{T, Response} =
            timer:tc(gen_server, call, [erjik_srv, {request, Data}]),
	?log(30, "redirector answer time: ~w micros", [T]),
        io:format("~s", [Response])
    catch
        _:Reason ->
            ?log(6, "redirector unable to process request \"~p\": ~p",
                 [Data, Reason]),
            io:format(Data)
    end.

