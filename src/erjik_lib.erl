%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash
%%% @doc Library module

-module(erjik_lib).

%% string utilities
-export(
   [is_string/1,
    uniq/1,
    strip/2,
    list_to_atom/2,
    split_lines/1,
    split_to_key_value/1,
    preparse_config/1,
    preparse_config/2,
    parse_uri/1
   ]).

%% IP address utilities
-export(
   [is_ip/1,
    is_ip4/1,
    is_ip6/1,
    is_same_ip_type/2,
    ip_family/1,
    is_ip_range/1,
    is_in_ip_range/2,
    cidr_to_ip_range/2,
    ip_to_binary/1,
    binary_to_ip/1,
    ip_to_integer/1,
    integer_to_ip4/1,
    integer_to_ip6/1,
    ip_add/2,
    is_ip_pool/1,
    is_in_ip_pool/2,
    flatten_ip_pool/1,
    list_to_ip/1,
    list_to_ip4/1,
    list_to_ip6/1,
    list_to_cidr/1,
    list_to_ip_range/1,
    ip_to_list/1,
    socket_to_list/1,
    socket_to_list/2,
    ip_range_to_list/1,
    ip_pool_to_list/1
   ]).

%% date/time utilities
-export(
   [timestamp/1,
    timestamp/2
   ]).

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [ip_range/0,
    ip_pool/0,
    administrative_offset/0
   ]).

-type ip_range() :: ip4_range() | ip6_range().

-type ip4_range() :: {inet:ip4_address(), inet:ip4_address()}.

-type ip6_range() :: {inet:ip6_address(), inet:ip6_address()}.

-type ip_pool() :: [ip_pool_item()].

-type ip_pool_item() :: any | none | inet:ip_address() | ip_range().

-type administrative_offset() ::
        {Sign    :: -1 | 1,
         Hours   :: 0..14,
         Minutes :: 0 | 30 | 45
        }.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% ----------------------------------------------------------------------
%% string utilities

%% @doc Return true if supplied term is plain ASCII text.
%% @spec is_string(term()) -> boolean()
is_string(Term) when is_list(Term) ->
    lists:all(
      fun(X) when is_integer(X), X >= 0, X < 256 -> true;
         (_) -> false
      end, Term);
is_string(_) -> false.

%% @doc Removes all duplicate elements from List except first.
%%      Elements order will be preserved.
%% @spec uniq(List) -> NewList
%%     List = list(),
%%     NewList = list()
uniq([]) -> [];
uniq([I]) -> [I];
uniq([H | Tail]) ->
    [H | uniq([I || I <- Tail, I /= H])].

%% @doc Removes Characters from beginning and ending of String.
%% @spec strip(String, Characters) -> StrippedString
%%     String = string(),
%%     Characters = string(),
%%     StrippedString = string()
strip(String, Characters) ->
    lists:reverse(
      strip_(
        lists:reverse(
          strip_(String, Characters)),
        Characters)).
strip_([], _Chars) -> [];
strip_([Char | Tail] = String, Chars) ->
    case lists:member(Char, Chars) of
        true ->
            strip_(Tail, Chars);
        _ ->
            String
    end.

%% @doc "Converts" string to atom only if result atom contained
%%      in list, supplied as second argument.
%% @spec list_to_atom(String, Atoms) -> {ok, Atom} | error
%%     String = string(),
%%     Atoms = [atom()],
%%     Atom = atom()
list_to_atom(_, []) -> error;
list_to_atom(String, [Atom | Tail]) -> 
    case atom_to_list(Atom) of
        String -> {ok, Atom};
        _ -> list_to_atom(String, Tail)
    end.

%% @doc Splits text to lines. Result lines will be stripped
%%      of newline characters.
%% @spec split_lines(String) -> Lines
%%     String = string(),
%%     Lines = [Line],
%%     Line = string()
split_lines(String) ->
    split_lines(String, [], []).
split_lines([], [], Lines) ->
    lists:reverse(Lines);
split_lines([], Line, Lines) ->
    split_lines([], [], [lists:reverse(Line) | Lines]);
split_lines("\r\n" ++ Tail, Line, Lines) ->
    split_lines(Tail, [], [lists:reverse(Line) | Lines]);
split_lines("\n" ++ Tail, Line, Lines) ->
    split_lines(Tail, [], [lists:reverse(Line) | Lines]);
split_lines("\r" ++ Tail, Line, Lines) ->
    split_lines(Tail, Line, Lines);
split_lines([C | Tail], Line, Lines) ->
    split_lines(Tail, [C | Line], Lines).

%% @doc Splits line to key and value. First token separated from
%%      others with space characters, will be returned as key in
%%      lower case (so, keys are case insensitive); rest of
%%      line will be returned as value. Character case of value
%%      string will be preserved.
%% @spec split_to_key_value(String) -> {Key, Value}
%%     String = string(),
%%     Key = string(),
%%     Value = string()
split_to_key_value(String) ->
    split_to_key_value_(strip(String, " \t\r\n"), []).
split_to_key_value_([], Key) ->
    {string:to_lower(lists:reverse(Key)), []};
split_to_key_value_([C | Tail], Key) ->
    case lists:member(C, " \t") of
        true ->
            {string:to_lower(lists:reverse(Key)),
             strip(Tail, " \t")};
        _ ->
            split_to_key_value_(Tail, [C | Key])
    end.

%% @doc Makes preparse of key-value configuration file.
%% @spec preparse_config(String) -> List
%%     String = string(),
%%     List = [MeaningLine],
%%     MeaningLine = {LineNo, Key, Value},
%%     LineNo = integer(),
%%     Key = string(),
%%     Value = string()
preparse_config(String) ->
    preparse_config(String, [{comment_chars, "#%!"}]).

%% @doc Makes preparse of key-value configuration file.
%% @spec preparse_config(String, Options) -> List
%%     String = string(),
%%     Options = [Option],
%%     Option = {comment_chars, CharList},
%%     CharList = string(),
%%     List = [MeaningLine],
%%     MeaningLine = {LineNo, Key, Value},
%%     LineNo = integer(),
%%     Key = string(),
%%     Value = string()
preparse_config(String, Options) ->
    CommentChars = proplists:get_value(comment_chars, Options, ""),
    Lines = split_lines(String),
    {_, List} =
        lists:foldl(
          fun(Line, {LineNo, Acc}) ->
                  case strip(Line, " \t\r\n") of
                      [] -> {LineNo + 1, Acc};
                      [C | _] = Stripped ->
                          case lists:member(C, CommentChars) of
                              true -> {LineNo + 1, Acc};
                              _ ->
                                  case split_to_key_value(Stripped) of
                                      {[_ | _] = Key, Value} ->
                                          {LineNo + 1,
                                           [{LineNo, Key, Value} | Acc]};
                                      _ -> {LineNo + 1, Acc}
                                  end
                          end
                  end
          end, {1, []}, Lines),
    lists:reverse(List).

%% @doc Parses URI to components.
%% @spec parse_uri(URI) -> {ok, List} | {error, Reason}
%%     URI = string(),
%%     List = [Element],
%%     Element = {proto, Proto} | {username, string()} |
%%               {hostname, Hostname} | {port, integer()} |
%%               {request, string()},
%%     Proto = http | https | ftp,
%%     Hostname = string() | inet:ip_address(),
%%     Reason = term()
parse_uri(URI) ->
    ReURL =
        %% proto
        "^(([a-z]+)://)?"
        %% username
        "(([a-z0-9-_\\.]+)@)?"
        %% hostname
        "(([a-z0-9\\.-]+)|\\[([0-9a-f:\\.]+)\\])"
        %% port number
        "(:([0-9]+))?"
        %% request
        "(/.*)?\$",
    ReOpts =
        [{capture, all_but_first, list},
         caseless],
    case re:run(URI, ReURL, ReOpts) of
        {match, Match} ->
            try parse_uri_(Match)
            catch
                _:{error, _Reason} = Error ->
                    Error;
                Type:Reason ->
                    {error, {Type, Reason,
                             erlang:get_stacktrace()}}
            end;
        _ -> {error, bad_uri}
    end.
parse_uri_([_, StrProto, _, Username, _, Hostname0, StrIP,
           _, StrPort, Request]) ->
    Proto =
        case list_to_atom(StrProto, [http, https, ftp]) of
            {ok, Proto0} -> Proto0;
            _ ->
                throw({error, bad_proto})
        end,
    Hostname =
        if length(Hostname0) > 0 ->
                case list_to_ip4(Hostname0) of
                    {ok, IP} -> IP;
                    _ -> Hostname0
                end;
           length(StrIP) > 0 ->
                case list_to_ip(StrIP) of
                    {ok, IP} -> IP;
                    _ -> throw({error, bad_ip})
                end;
           true -> throw({error, no_hostname})
        end,
    {ok,
     [{proto, Proto},
      {hostname, Hostname},
      {request, Request}] ++
         case StrPort of
             [_ | _] ->
                 [{port, list_to_integer(StrPort)}];
             _ -> []
         end ++
         case Username of
             [_ | _] -> [{username, Username}];
             _ -> []
         end};
parse_uri_(_) ->
    throw({error, bad_uri}).

%% ----------------------------------------------------------------------
%% IP address utilities

%% @doc Return true if supplied term is valid IP address.
%% @spec is_ip(term()) -> boolean()
is_ip(Term) ->
    is_ip4(Term) orelse is_ip6(Term).

%% @doc Return true if supplied term is valid IPv4 address.
%% @spec is_ip4(term()) -> boolean()
is_ip4({A, B, C, D})
  when is_integer(A), A >= 0, A < 256,
       is_integer(B), B >= 0, B < 256,
       is_integer(C), C >= 0, C < 256,
       is_integer(D), D >= 0, D < 256 -> true;
is_ip4(_) -> false.

%% @doc Return true if supplied term is valid IPv6 address.
%% @spec is_ip6(term()) -> boolean()
is_ip6({A, B, C, D, E, F, G, H})
  when is_integer(A), A >= 0, A =< 16#ffff,
       is_integer(B), B >= 0, B =< 16#ffff,
       is_integer(C), C >= 0, C =< 16#ffff,
       is_integer(D), D >= 0, D =< 16#ffff,
       is_integer(E), E >= 0, E =< 16#ffff,
       is_integer(F), F >= 0, F =< 16#ffff,
       is_integer(G), G >= 0, G =< 16#ffff,
       is_integer(H), H >= 0, H =< 16#ffff -> true;
is_ip6(_) -> false.

%% @doc Return true if both IP adresses specified is
%%      of same type (IPv4 or IPv6).
%% @spec is_same_ip_type(IP1, IP2) -> boolean()
%%     IP1 = inet:ip_address(),
%%     IP2 = inet:ip_address()
is_same_ip_type(IP1, IP2) ->
    (is_ip4(IP1) andalso is_ip4(IP2)) orelse
        (is_ip6(IP1) andalso is_ip6(IP2)).

%% @doc Returns IP family type for supplied IP address.
%% @spec ip_family(IP) -> Family
%%     IP = inet:ip_address(),
%%     Family = inet:address_family()
ip_family(IP) ->
    case is_ip4(IP) of
        true -> inet;
        _ ->
            case is_ip6(IP) of
                true -> inet6
            end
    end.

%% @doc Return true if supplied term is valid IP range.
%% @spec is_ip_range(term()) -> boolean()
is_ip_range({IP1, IP2}) ->
    is_same_ip_type(IP1, IP2) andalso IP1 =< IP2;
is_ip_range(_) -> false.

%% @doc Return true if supplied IP range contains IP specified
%%      as first argument.
%% @spec is_in_ip_range(IP, IpRange) -> boolean()
%%     IP = inet:ip_address(),
%%     IpRange = ip_range()
is_in_ip_range(IP, {IP, IP}) -> true;
is_in_ip_range(IP, {Min, Max})
  when size(IP) == size(Min), size(IP) == size(Max),
       Min =< Max, IP >= Min, IP =< Max -> true;
is_in_ip_range(_, _) -> false.

%% @doc Makes IP range from CIDR supplied.
%% @spec cidr_to_ip_range(IP, Prefix) -> ip_range()
%%     IP = inet:ip_address(),
%%     Prefix = integer()
cidr_to_ip_range(IP, Prefix) ->
    Bin = ip_to_binary(IP),
    AddrLen = size(Bin) * 8,
    HostAddrLen = AddrLen - Prefix,
    MaxHostAddr = trunc(math:pow(2, HostAddrLen)) - 1,
    <<NetAddr:Prefix/big-unsigned, _:HostAddrLen>> = Bin,
    Min = <<NetAddr:Prefix/big-unsigned,
            0:HostAddrLen/big-unsigned>>,
    Max = <<NetAddr:Prefix/big-unsigned,
            MaxHostAddr:HostAddrLen/big-unsigned>>,
    {binary_to_ip(Min), binary_to_ip(Max)}.

%% @doc Adds number to IP address.
%% @spec ip_add(IP, Number) -> NewIP
%%     IP = inet:ip_address(),
%%     Number = integer(),
%%     NewIP = inet:ip_address()
ip_add(IP, Number) ->
    Bin = ip_to_binary(IP),
    Size = size(Bin) * 8,
    <<Int:Size/big-unsigned>> = Bin,
    Int2 = Int + Number,
    binary_to_ip(<<Int2:Size/big-unsigned>>).

%% @doc Converts IP to binary.
%% @spec ip_to_binary(IP) -> Binary
%%     IP = inet:ip_address(),
%%     Binary = binary()
ip_to_binary(IP) ->
    case is_ip4(IP) of
        true ->
            list_to_binary(tuple_to_list(IP));
        _ ->
            list_to_binary(
              [<<I:16/big-unsigned>> || I <- tuple_to_list(IP)])
    end.

%% @doc Converts binary to IP address.
%% @spec binary_to_ip(Binary) -> IP
%%     Binary = binary(),
%%     IP = inet:ip_address()
binary_to_ip(Binary) when size(Binary) == 4 ->
    list_to_tuple(binary_to_list(Binary));
binary_to_ip(Binary) when size(Binary) == 16 ->
    <<A:16/big-unsigned, B:16/big-unsigned,
      C:16/big-unsigned, D:16/big-unsigned,
      E:16/big-unsigned, F:16/big-unsigned,
      G:16/big-unsigned, H:16/big-unsigned>> = Binary,
    {A, B, C, D, E, F, G, H}.

%% @doc Converts IP address to integer.
%% @spec ip_to_integer(inet:ip_address()) -> integer()
ip_to_integer(IP) ->
    <<Int/big-unsigned>> = ip_to_binary(IP),
    Int.

%% @doc Converts integer to IPv4 address.
%% @spec integer_to_ip4(integer()) -> inet:ip4_address()
integer_to_ip4(Integer) ->
    binary_to_ip(<<Integer:32/big-unsigned>>).

%% @doc Converts integer to IPv6 address.
%% @spec integer_to_ip6(integer()) -> inet:ip6_address()
integer_to_ip6(Integer) ->
    binary_to_ip(<<Integer:128/big-unsigned>>).

%% @doc Returns true if supplied term is of type ip_pool().
%% @spec is_ip_pool(term()) -> boolean()
is_ip_pool(List) when is_list(List) ->
    lists:all(
      fun(Term) ->
              is_ip_range(Term) orelse is_ip(Term)
      end, List);
is_ip_pool(_) -> false.

%% @doc Returns true if supplied pool contains IP specified
%%      as first argument.
%% @spec is_in_ip_pool(IP, IpPool) -> boolean()
%%     IP = inet:ip_address(),
%%     IpPool = ip_pool()
is_in_ip_pool(IP, IpPool) ->
    lists:any(
      fun(none) -> false;
         (any) -> true;
         ({_Min, _Max} = IpRange) ->
              is_in_ip_range(IP, IpRange);
         (X) -> IP == X
      end, IpPool).

%% @doc Flattens IP pool.
%% @spec flatten_ip_pool(IpPool) -> NewIpPool
%%     IpPool = ip_pool(),
%%     NewIpPool = ip_pool()
flatten_ip_pool(IpPool) ->
    case lists:member(any, IpPool) of
        true -> [any];
        _ ->
            flatten_ip_pool_(
              [I || I <- IpPool, I /= none])
    end.
flatten_ip_pool_(IpPool) ->
    IpPool1 =
        lists:map(
          fun({A, A}) -> A;
             (Other) -> Other
          end, IpPool),
    {Ranges, IPs} =
        lists:partition(fun is_ip_range/1, lists:usort(IpPool1)),
    {Ranges4, Ranges6} =
        lists:partition(
          fun({A, _}) ->
                  is_ip4(A)
          end, Ranges),
    {IPv4s, IPv6s} = lists:partition(fun is_ip4/1, IPs),
    IntRanges4 =
        lists:map(
          fun({A1, A2}) ->
                  {ip_to_integer(A1), ip_to_integer(A2)}
          end, Ranges4),
    FlatRanges4 =
        lists:map(
          fun({A1, A2}) ->
                  {integer_to_ip4(A1), integer_to_ip4(A2)}
          end, flatten_int_ranges(IntRanges4)),
    IntRanges6 =
        lists:map(
          fun({A1, A2}) ->
                  {ip_to_integer(A1), ip_to_integer(A2)}
          end, Ranges6),
    FlatRanges6 =
        lists:map(
          fun({A1, A2}) ->
                  {integer_to_ip6(A1), integer_to_ip6(A2)}
          end, flatten_int_ranges(IntRanges6)),
    V4Pool =
        FlatRanges4 ++
        lists:filter(
          fun(IP) ->
                  not is_in_ip_pool(IP, FlatRanges4)
          end, IPv4s),
    V6Pool =
        FlatRanges6 ++
        lists:filter(
          fun(IP) ->
                  not is_in_ip_pool(IP, FlatRanges6)
          end, IPv6s),
    Sorter =
        fun({A1, _}, {A2, _}) -> A1 =< A2;
           ({A1, _}, A2) -> A1 =< A2;
           (A1, {A2, _}) -> A1 =< A2;
           (A1, A2) -> A1 =< A2
        end,
    lists:sort(Sorter, V4Pool) ++ lists:sort(Sorter, V6Pool).

%% @doc Convert IP address from text to Erlang representation.
%% @spec list_to_ip(String) -> {ok, IP} | error
%%     String = string(),
%%     IP = inet:ip_address()
list_to_ip(String) ->
    case list_to_ip4(String) of
        {ok, _IP} = Ok -> Ok;
        _ -> list_to_ip6(String)
    end.

%% @doc Convert IPv4 address from text to Erlang representation.
%% @spec list_to_ip4(String) -> {ok, IP} | error
%%     String = string(),
%%     IP = inet:ip4_address()
list_to_ip4(String) ->
    case re:run(String, "^[0-9]+(\\.[0-9]+)+\$") of
        {match, _} ->
            List = string:tokens(String, "."),
            IP =
                list_to_tuple(
                  list_to_ip4_([list_to_integer(S) || S <- List])),
            case is_ip4(IP) of
                true -> {ok, IP};
                _ -> error
            end;
        _ -> error
    end.
list_to_ip4_([A, HostAddr]) ->
    <<B, C, D>> = <<HostAddr:24/big-unsigned>>,
    [A, B, C, D];
list_to_ip4_([A, B, HostAddr]) ->
    <<C, D>> = <<HostAddr:16/big-unsigned>>,
    [A, B, C, D];
list_to_ip4_(Other) -> Other.

%% @doc Convert IPv6 address from text to Erlang representation.
%% @spec list_to_ip6(String) -> {ok, IP} | error
%%     String = string(),
%%     IP = inet:ip6_address()
list_to_ip6("::") ->
    {ok, list_to_tuple(lists:duplicate(8, 0))};
list_to_ip6(String) ->
    case string:to_lower(String) of
        "::ffff:" ++ Tail ->
            case list_to_ip4(Tail) of
                {ok, {A, B, C, D}} ->
                    {ok,
                     binary_to_ip(
                       <<0:80/big-unsigned,
                         65535:16/big-unsigned,
                         A:8/big-unsigned, B:8/big-unsigned,
                         C:8/big-unsigned, D:8/big-unsigned>>)};
                _ -> list_to_ip6_normal(String)
            end;
        "::" ++ Tail ->
            case list_to_ip4(Tail) of
                {ok, {A, B, C, D}} ->
                    %% deprecated notation
                    {ok,
                     binary_to_ip(
                       <<0:96/big-unsigned,
                         A:8/big-unsigned, B:8/big-unsigned,
                         C:8/big-unsigned, D:8/big-unsigned>>)};
                _ -> list_to_ip6_normal(String)
            end;
        _ -> list_to_ip6_normal(String)
    end.
list_to_ip6_normal(String) ->
    list_to_ip6_(string:to_lower(String), [], [], false, 0).
list_to_ip6_(_, Elem, _, _, _) when length(Elem) > 4 -> error;
list_to_ip6_(_, _, _, _, LC) when LC > 8 -> error;
list_to_ip6_("::" ++ Tail, [], Elems, false, LC) ->
    list_to_ip6_(Tail, [], [zeros | Elems], true, LC);
list_to_ip6_("::" ++ Tail, Elem, Elems, false, LC) ->
    Int = erlang:list_to_integer(lists:reverse(Elem), 16),
    list_to_ip6_(Tail, [], [zeros, Int | Elems], true, LC + 1);
list_to_ip6_("::" ++ _, _, _, _, _) -> error;
list_to_ip6_(":" ++ _, [], _, _, _) -> error;
list_to_ip6_(":" ++ Tail, Elem, Elems, ZG, LC) ->
    Int = erlang:list_to_integer(lists:reverse(Elem), 16),
    list_to_ip6_(Tail, [], [Int | Elems], ZG, LC + 1);
list_to_ip6_([C | Tail], Elem, Elems, ZG, LC)
  when (C >= $a andalso C =< $f)
       orelse (C >= $0 andalso C =< $9) ->
    list_to_ip6_(Tail, [C | Elem], Elems, ZG, LC);
list_to_ip6_([_ | _], _, _, _, _) -> error;
list_to_ip6_([], [_ | _] = Elem, Elems, ZG, LC) ->
    Int = erlang:list_to_integer(lists:reverse(Elem), 16),
    list_to_ip6_([], [], [Int | Elems], ZG, LC + 1);
list_to_ip6_([], [], _, true, 8) -> error;
list_to_ip6_([], [], _, false, LC) when LC < 8 -> error;
list_to_ip6_([], [], Elems, _, 8) ->
    {ok, list_to_tuple(lists:reverse(Elems))};
list_to_ip6_([], [], Elems, _, LC) ->
    {ok,
     list_to_tuple(
       lists:flatmap(
         fun(zeros) -> lists:duplicate(8 - LC, 0);
            (Int) -> [Int]
         end, lists:reverse(Elems)))}.

%% @doc Converts CIDR notation to Erlang representation.
%% @spec list_to_cidr(String) -> {ok, IP, PrefixLen} | error
%%     String = string(),
%%     IP = inet:ip_address(),
%%     PrefixLen = integer()
list_to_cidr(String) ->
    case re:run(String, "^([^/]+)/([0-9]+)\$", [{capture, all, list}]) of
        {match, [_All, StrIP, StrPrefix]} ->
            case list_to_integer(StrPrefix) of
                Prefix when Prefix =< 128 ->
                    case list_to_ip6(StrIP) of
                        {ok, IP} ->
                            {ok, IP, Prefix};
                        _ when Prefix =< 32 ->
                            case parse_cidr_ip4(StrIP) of
                                {ok, IP} ->
                                    {ok, IP, Prefix};
                                _ -> error
                            end;
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

%% @doc Parses string representation of IP range.
%%      Source data can be one of these forms:
%%          "IP/PrefixLength";
%%          "MinIP MaxIP", separated by spaces, hyphen or coma;
%%          "IP".
%% @spec list_to_ip_range(String) -> {ok, {MinIP, MaxIP}} | error
%%     String = string(),
%%     MinIP = inet:ip_address(),
%%     MaxIP = inet:ip_address()
list_to_ip_range(String) ->
    try list_to_ip_range_(String)
    catch
        _:{ok, _Value} = Ok -> Ok;
        _:_ -> error
    end.
list_to_ip_range_(String) ->
    case list_to_ip(String) of
        {ok, IP1} ->
            throw({ok, {IP1, IP1}});
        _ -> nop
    end,
    case list_to_cidr(String) of
        {ok, IP2, PrefixLen} ->
            throw({ok, cidr_to_ip_range(IP2, PrefixLen)});
        _ -> nop
    end,
    case string:tokens(String, " \t-,") of
        [StrA1, StrA2] ->
            {ok, A1} = list_to_ip(StrA1),
            {ok, A2} = list_to_ip(StrA2),
            true = is_same_ip_type(A1, A2),
            true = A1 =< A2,
            {ok, {A1, A2}};
        _ -> error
    end.

%% @doc Converts IP address in Erlang representation to text.
%% @spec ip_to_list(IP) -> String
%%     IP = inet:ip_address(),
%%     String = string()
ip_to_list({A, B, C, D}) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B", [A, B, C, D]));
ip_to_list({0, 0, 0, 0, 0, 16#ffff, G, H}) ->
    <<A:8/big-unsigned, B:8/big-unsigned>> = <<G:16/big-unsigned>>,
    <<C:8/big-unsigned, D:8/big-unsigned>> = <<H:16/big-unsigned>>,
    "::ffff:" ++ ip_to_list({A, B, C, D});
ip_to_list({0, 0, 0, 0, 0, 0, 0, 0}) -> "::";
ip_to_list({_A, _B, _C, _D, _E, _F, _G, _H} = IP) ->
    List = tuple_to_list(IP),
    lists:flatten(
      lists:reverse(
        lists:foldl(
          fun(zeroes, Acc) -> ["::" | Acc];
             (Elem, ["::" | _] = Acc) -> [Elem | Acc];
             (Elem, []) -> [Elem];
             (Elem, Acc) -> [Elem, ":" | Acc]
          end, [],
          lists:map(
            fun(Int) when is_integer(Int) ->
                    string:to_lower(erlang:integer_to_list(Int, 16));
               (Other) -> Other
            end,
            case max_zero_group(List) of
                {Pos, Len} ->
                    lists:sublist(List, 1, Pos - 1) ++ [zeroes] ++
                        lists:sublist(List, Pos + Len, 8);
                _ -> List
            end)))).

%% @doc Converts IP address and port number to text.
%% @spec socket_to_list(Socket) -> String
%%     Socket = {IP, Port},
%%     IP = inet:ip_address(),
%%     Port = inet:port_number(),
%%     String = string()
socket_to_list({IP, Port}) ->
    socket_to_list(IP, Port).

%% @doc Converts IP address and port number to text.
%% @spec socket_to_list(IP, Port) -> String
%%     IP = inet:ip_address(),
%%     Port = inet:port_number(),
%%     String = string()
socket_to_list(IP, Port) ->
    StrIP = ip_to_list(IP),
    Format =
        case is_ip4(IP) of
            true -> "~s:~B";
            _ -> "[~s]:~B"
        end,
    lists:flatten(io_lib:format(Format, [StrIP, Port])).

%% @doc Formats IP range as text.
%% @spec ip_range_to_list(IpRange) -> string()
%%     IpRange = ip_range()
ip_range_to_list({Min, Max}) ->
    ip_to_list(Min) ++ "-" ++ ip_to_list(Max).

%% @doc Formats IP pool as text.
%% @spec ip_pool_to_list(IpPool) -> string()
%%     IpPool = ip_pool()
ip_pool_to_list(IpPool) ->
    string:join(
      lists:map(
        fun(any) -> "any";
           ({_Min, _Max} = IpRange) ->
                ip_range_to_list(IpRange);
           (IP) -> ip_to_list(IP)
        end, flatten_ip_pool(IpPool)), ";").

%% ----------------------------------------------------------------------
%% date/time utilities

%% @doc Formats time as text.
%% @spec timestamp(timestamp()) -> io_list()
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
%% @spec timestamp(timestamp(), administrative_offset()) -> io_list()
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

%% @doc Return local offset.
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

%% @doc Flattens integer ranges list. Source list must be
%%      unique sorted for this function to work.
%% @spec flatten_int_ranges(IntRanges) -> NewIntRanges
%%     IntRanges = [{integer(), integer()}],
%%     NewIntRanges = [{integer(), integer()}]
flatten_int_ranges([{A1, A2}, {B1, B2} | Tail]) when A2 >= B1 - 1 ->
    flatten_int_ranges([{A1, lists:max([A2, B2])} | Tail]);
flatten_int_ranges([A | [_ | _] = Tail]) ->
    [A | flatten_int_ranges(Tail)];
flatten_int_ranges(Other) -> Other.

%% @doc Parses IPv4 address from CIDR.
%% @spec parse_cidr_ip4(String) -> {ok, IP} | error
%%     String = string(),
%%     IP = inet:ip4_address()
parse_cidr_ip4(String) ->
    case re:run(String, "^[0-9]+(\\.[0-9]+)*\$") of
        {match, _} ->
            List = string:tokens(String, "."),
            Integers =
                case [list_to_integer(S) || S <- List] of
                    List1 when length(List1) < 4 ->
                        List1 ++
                            lists:duplicate(
                              4 - length(List1), 0);
                    List1 -> List1
                end,
            IP = list_to_tuple(Integers),
            case is_ip4(IP) of
                true -> {ok, IP};
                _ -> error
            end;
        _ -> error
    end.

%% @doc Helper function for ip_to_list/1 in case of IPv6 address.
%%      Finds group of zeroes with maximum length.
%% @spec max_zero_group(List) -> {Pos, Len} | undefined
%%     List = [integer()],
%%     Pos = integer(),
%%     Len = integer()
max_zero_group(List) ->
    Sorted =
        lists:sort(
          fun({A, B}, {A, C}) -> C =< B;
             (A, B) -> A =< B
          end,
          zero_groups(1, List)),
    case lists:reverse(Sorted) of
        [{Len, Pos} | _] -> {Pos, Len};
        _ -> undefined
    end.
zero_groups(N, List) ->
    case zero_group(N, List) of
        {Pos, Len, Tail} ->
            [{Len, Pos} | zero_groups(Pos + Len, Tail)];
        _ -> []
    end.
zero_group(_Pos, []) -> undefined;
zero_group(Pos, [0 | _] = List) ->
    {List1, Tail} =
        lists:splitwith(fun(0) -> true; (_) -> false end, List),
    {Pos, length(List1), Tail};
zero_group(Pos, [_ | Tail]) -> zero_group(Pos + 1, Tail).

