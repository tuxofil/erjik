%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009, Aleksey Morarash
%%% @doc Library module

-module(erjik_lib).

-export(
   [consultr/1,
    unconsult/2,
    is_string/1,
    is_ip/1,
    is_ipv4/1,
    is_ipv6/1,
    is_ippool/1,
    in_ippool/2,
    str_replace/3,
    timestamp/1,
    timestamp/2
   ]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Consult with includes.
%%      Include applyed if term is of {include, Wildcard}.
%% @spec consultr(Wildcard) -> {ok, Terms}
%%     Wildcard = string(),
%%     Terms = [term()]
consultr(Wildcard) ->
    {ok,
     lists:flatmap(
       fun(F) ->
               case filelib:is_regular(F) of
                   true ->
                       {ok, Terms} = file:consult(F),
                       lists:flatmap(
                         fun({include, F2}) when is_list(F2) ->
                                 {ok, Trms} =
                                     consultr(
                                       filename:join(
                                         [filename:dirname(F), F2])),
                                 Trms;
                            (T) ->
                                 [T]
                         end, Terms);
                   _ ->
                       []
               end
       end, filelib:wildcard(Wildcard))}.

unconsult(Filename, Terms) ->
    file:write_file(
      Filename, [io_lib:format("~p.~n", [T]) || T <- Terms]).

is_string(Term) when is_list(Term) ->
    lists:all(
      fun(X) when is_integer(X), X >= 0, X < 256 -> true;
         (_) -> false
      end, Term).

%% @spec is_ip(term()) -> boolean()
is_ip(Ip) ->
    is_ipv4(Ip) orelse is_ipv6(Ip).

%% @spec is_ipv4(term()) -> boolean()
is_ipv4(IPv4) when is_tuple(IPv4), size(IPv4) == 4 ->
    lists:all(
      fun(X) when is_integer(X), X >= 0, X < 256 ->
              true;
         (_) -> false
      end, tuple_to_list(IPv4));
is_ipv4(_) -> false.

%% @spec is_ipv6(term()) -> boolean()
is_ipv6(IPv6) when is_tuple(IPv6), size(IPv6) == 8 ->
    lists:all(
      fun(X) when is_integer(X), X >= 0, X < 65536 ->
              true;
         (_) -> false
      end, tuple_to_list(IPv6));
is_ipv6(_) -> false.

is_ippool(IpPool) ->
    lists:all(
      fun({Min, Max}) ->
              erjik_lib:is_ip(Min)
                  andalso erjik_lib:is_ip(Max)
                  andalso Min < Max;
         (Ip) -> erjik_lib:is_ip(Ip)
      end, IpPool).

in_ippool(Ip, IpPool) ->
    lists:any(
      fun({Min, Max}) when Ip >= Min, Ip =< Max -> true;
         ({_Min, _Max}) -> false;
         (X) -> Ip == X
      end, IpPool).

str_replace(String, Regexp, Replacement) ->
    case regexp:gsub(String, Regexp, Replacement) of
        {ok, Result, _} -> Result;
        _Error -> String
    end.

%% @doc Formats time as text.
%% @spec timestamp(timestamp()) -> string()
timestamp(Time) ->
    {{Year, Month, Day}, {Hour, Minutes, Seconds}} =
        calendar:now_to_local_time(Time),
    {OffSign, OffHours, OffMinutes} = local_offset(Time),
    Sign =
        if OffSign > 0 -> "+";
           true -> "-"
        end,
    io_lib:format(
      "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s~2..0B~2..0B",
      [Year, Month, Day, Hour, Minutes, Seconds,
       Sign, OffHours, OffMinutes]).

%% @doc Formats time as text with specified time offset.
%% @spec timestamp(timestamp(), administrative_offset()) -> string()
timestamp(Timestamp, {OffsetSign, OffsetHours, OffsetMinutes}) ->
    {{Year, Month, Day}, {Hour, Minutes, Seconds}} =
        calendar:gregorian_seconds_to_datetime(
          calendar:datetime_to_gregorian_seconds(
            calendar:now_to_universal_time(Timestamp)) +
              OffsetSign * (OffsetHours * 60 + OffsetMinutes) * 60),
    Sign =
        if OffsetSign > 0 -> "+";
           true -> "-"
        end,
    io_lib:format(
      "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s~2..0B~2..0B",
      [Year, Month, Day, Hour, Minutes, Seconds,
       Sign, OffsetHours, OffsetMinutes]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% @doc Return local offset (on server).
%% @spec local_offset(Time) -> Offset
%%     Time = timestamp(),
%%     Offset = administrative_offset()
local_offset(Time) ->
    SecondsDiff =
        calendar:datetime_to_gregorian_seconds(
          calendar:now_to_local_time(Time)) -
        calendar:datetime_to_gregorian_seconds(
          calendar:now_to_universal_time(Time)),
    OffsetMinutes0 = round(abs(SecondsDiff) / (15 * 60)) * 15,
    OffsetHours = OffsetMinutes0 div 60,
    OffsetMinutes = OffsetMinutes0 rem 60,
    Sign =
        if SecondsDiff > 0 -> 1;
           true -> -1
        end,
    {Sign, OffsetHours, OffsetMinutes}.

