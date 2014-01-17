%%% @doc
%%% Library module.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 7 Mar 2009
%%% @copyright 2009-2012, Aleksey Morarash

-module(erjik_lib).

-export(
   [challenge/2,
    pmap/2,
    read_mime_types/1
   ]).

%% list and string utilities
-export(
   [split_by_len/2,
    is_string/1,
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
    list_to_port_number/1,
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

-include("erjik.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type(
   [ip_range/0,
    ip_pool/0,
    ip_prefix/0,
    ip4_prefix/0,
    ip6_prefix/0,
    administrative_offset/0
   ]).

-type ip_range() :: ip4_range() | ip6_range().

-type ip4_range() :: {inet:ip4_address(), inet:ip4_address()}.

-type ip6_range() :: {inet:ip6_address(), inet:ip6_address()}.

-type ip_pool() :: [ip_pool_item()].

-type ip_pool_item() :: any | none | inet:ip_address() | ip_range().

-type ip_prefix() :: ip4_prefix() | ip6_prefix().

-type ip4_prefix() :: 0..32.

-type ip6_prefix() :: 0..128.

-type administrative_offset() ::
        {Sign    :: -1 | 1,
         Hours   :: 0..14,
         Minutes :: 0 | 30 | 45
        }.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Like lists:map/2, but:
%%      <ol>
%%      <li>spawns each fun in separate process;</li>
%%      <li>returns only result of first successfully finished fun
%%         (rest of processes spawned will be killed).</li>
%%      </ol>
%%      If supplied list is empty or no functions finished normally,
%%      it will return 'undefined' atom.
%%      Example:
%%      <ul>
%%      <li>challenge(fun(T) -> timer:sleep(T), T end, [2000, 1000, 3000])
%%          will return {ok, 1000};</li>
%%      <li>challenge(fun(_) -> exit(oops) end, [1, 2, 3, 4])
%%          will return 'undefined'.</li>
%%      </ul>
-spec challenge(Fun :: fun((A :: any()) -> B :: any()),
                List :: [A :: any()]) ->
                       {ok, B :: any()} | undefined.
challenge(_Fun, []) ->
    undefined;
challenge(Fun, [_ | _] = List) when is_function(Fun, 1) ->
    {Pid, MonRef} = spawn_monitor(fun() -> challenge_init(Fun, List) end),
    receive
        {'DOWN', MonRef, process, Pid, {ok, _Value} = Ok} ->
            Ok;
        {'DOWN', MonRef, process, Pid, _Reason} ->
            undefined
    end.

-spec challenge_init(Fun :: fun((A :: any()) -> B :: any()),
                     List :: [A :: any()]) -> no_return().
challenge_init(Fun, List) ->
    process_flag(trap_exit, true),
    Master = self(),
    challenge_loop(
      sets:from_list(
        [spawn_link(fun() -> challenge_work(Master, Fun, Elem) end) ||
            Elem <- List])).

-spec challenge_loop(Workers :: set()) -> no_return().
challenge_loop(Workers) ->
    case sets:size(Workers) of
        0 -> exit(no_answers);
        _ -> nop
    end,
    receive
        {'#chall', Value} ->
            exit({ok, Value});
        {'EXIT', Pid, _Reason} ->
            case sets:is_element(Pid, Workers) of
                true ->
                    challenge_loop(sets:del_element(Pid, Workers));
                _ ->
                    challenge_loop(Workers)
            end;
        _ ->
            challenge_loop(Workers)
    end.

-spec challenge_work(MasterPid :: pid(),
                     Fun :: fun((A :: any()) -> B :: any()),
                     Elem :: any()) -> ok.
challenge_work(MasterPid, Fun, Elem) ->
    catch MasterPid ! {'#chall', Fun(Elem)},
    ok.

%% @doc Like lists:map/2, but each fun will be applied in parallel.
%%      Results order will be preserved.
-spec pmap(Fun :: fun((A :: any()) -> B :: any()),
           List1 :: [A :: any()]) ->
                  List2 :: [B :: any()].
pmap(Fun, List) ->
    Master = self(),
    [receive
         {Pid, Result} ->
             Result
     end ||
        Pid <-
            [spawn_link(
               fun() ->
                       Master ! {self(), Fun(Elem)}
               end) || Elem <- List]].

%% @doc Reads and parses mime types file.
-spec read_mime_types(Filename :: file:filename()) ->
                             {ok, [{MimeType :: nonempty_string(),
                                    Extensions :: [nonempty_string()]}]} |
                             {error, Reason :: any()}.
read_mime_types(Filename) ->
    case file:open(Filename, [read, raw, read_ahead]) of
        {ok, FH} ->
            Result =
                try read_mime_types_loop(FH, [])
                catch
                    Type:Reason ->
                        {error,
                         {Type, Reason,
                          erlang:get_stacktrace()}}
                end,
            catch file:close(FH),
            Result;
        Error -> Error
    end.
read_mime_types_loop(FH, Results) ->
    case file:read_line(FH) of
        eof ->
            {ok, lists:reverse(Results)};
        {ok, Line} ->
            case strip(Line, " \t\r\n") of
                [C | _] = Stripped when C /= $# ->
                    [MimeType | Extensions] =
                        string:tokens(Stripped, " \t"),
                    read_mime_types_loop(
                      FH, [{MimeType, Extensions} | Results]);
                _ ->
                    read_mime_types_loop(FH, Results)
            end;
        Error -> Error
    end.

%% ----------------------------------------------------------------------
%% list and string utilities

%% @doc Splits apart List to chunks of Len length.
-spec split_by_len(List :: list(), Len :: pos_integer()) ->
                          ListOfLists :: [list()].
split_by_len(List, Len) when is_list(List), is_integer(Len), Len > 0 ->
    split_by_len(Len, List, Len, [], []).
split_by_len(Len, Source, 0, List, Lists) ->
    split_by_len(Len, Source, Len, [], [lists:reverse(List) | Lists]);
split_by_len(Len, [H | Tail], N, List, Lists) ->
    split_by_len(Len, Tail, N - 1, [H | List], Lists);
split_by_len(_, [], _, [], Lists) ->
    lists:reverse(Lists);
split_by_len(Len, [], N, [_ | _] = List, Lists) ->
    split_by_len(Len, [], N, [], [lists:reverse(List) | Lists]).

%% @doc Return true if supplied term is plain ASCII text.
-spec is_string(Term :: any()) -> boolean().
is_string(Term) when is_list(Term) ->
    lists:all(
      fun(X) when is_integer(X), X >= 0, X < 256 -> true;
         (_) -> false
      end, Term);
is_string(_) -> false.

%% @doc Removes all duplicate elements from List except first.
%%      Elements order will be preserved.
-spec uniq(List :: list()) -> NewList :: list().
uniq([]) -> [];
uniq([I]) -> [I];
uniq([H | Tail]) ->
    [H | uniq([I || I <- Tail, I /= H])].

%% @doc Removes Characters from beginning and ending of String.
-spec strip(String :: string(), Characters :: string()) ->
                   StrippedString :: string().
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

%% @doc "Convert" string to an atom only if the result atom
%% is present in the list, supplied as the second argument.
-spec list_to_atom(String :: string(), Atoms :: [atom()]) ->
                          {ok, Atom :: atom()} | error.
list_to_atom(_, []) ->
    error;
list_to_atom(String, [Atom | Tail]) -> 
    case atom_to_list(Atom) of
        String ->
            {ok, Atom};
        _ ->
            list_to_atom(String, Tail)
    end.

%% @doc Split the text to lines. Result lines will be stripped
%% of newline characters.
-spec split_lines(String :: string()) -> Lines :: [string()].
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

%% @doc Split the line to a key and a value. The first token separated
%% from the others with space characters, will be returned as the key in
%% lower case (so, keys are case insensitive); rest of the line will be
%% returned as the value. Character case of the value will be preserved.
-spec split_to_key_value(String :: string()) ->
                                {Key :: string(), Value :: string()}.
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

%% @equiv preparse_config(String, [{comment_chars, "#%!"}])
%% @doc Makes preparse of key-value configuration file.
-spec preparse_config(String :: string()) ->
                             MeaningLines ::
                               [{LineNo :: pos_integer(),
                                 Key :: nonempty_string(),
                                 Value :: string()}].
preparse_config(String) ->
    preparse_config(String, [{comment_chars, "#%!"}]).

%% @doc Makes preparse of key-value configuration file.
-spec preparse_config(String :: string(),
                      Options ::
                        [{comment_chars, Chars :: string()}]) ->
                             MeaningLines ::
                               [{LineNo :: pos_integer(),
                                 Key :: nonempty_string(),
                                 Value :: string()}].
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
-spec parse_uri(URI :: nonempty_string()) ->
                       {ok,
                        Components ::
                          [{proto, Protocol :: http | https | ftp} |
                           {username, nonempty_string()} |
                           {hostname, nonempty_string() | inet:ip_address()} |
                           {port, inet:port_number()} |
                           {request, string()}
                          ]} |
                       {error, Reason :: any()}.
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
-spec is_ip(Term :: any()) -> boolean().
is_ip(Term) ->
    is_ip4(Term) orelse is_ip6(Term).

%% @doc Return true if supplied term is valid IPv4 address.
-spec is_ip4(Term :: any()) -> boolean().
is_ip4({A, B, C, D})
  when is_integer(A), A >= 0, A < 256,
       is_integer(B), B >= 0, B < 256,
       is_integer(C), C >= 0, C < 256,
       is_integer(D), D >= 0, D < 256 -> true;
is_ip4(_) -> false.

%% @doc Return true if supplied term is valid IPv6 address.
-spec is_ip6(Term :: any()) -> boolean().
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
-spec is_same_ip_type(IP1 :: inet:ip_address(), IP2 :: inet:ip_address()) ->
                             boolean().
is_same_ip_type(IP1, IP2) ->
    (is_ip4(IP1) andalso is_ip4(IP2)) orelse
        (is_ip6(IP1) andalso is_ip6(IP2)).

%% @doc Returns IP family type for supplied IP address.
-spec ip_family(IP :: inet:ip_address()) -> Family :: inet:address_family().
ip_family(IP) ->
    case is_ip4(IP) of
        true -> inet;
        _ ->
            case is_ip6(IP) of
                true -> inet6
            end
    end.

%% @doc Return true if supplied term is valid IP range.
-spec is_ip_range(Term :: any()) -> boolean().
is_ip_range({IP1, IP2}) ->
    is_same_ip_type(IP1, IP2) andalso IP1 =< IP2;
is_ip_range(_) -> false.

%% @doc Return true if supplied IP range contains IP specified
%%      as first argument.
-spec is_in_ip_range(IP :: inet:ip_address(), IpRange :: ip_range()) ->
                            boolean().
is_in_ip_range(IP, {IP, IP}) -> true;
is_in_ip_range(IP, {Min, Max})
  when size(IP) == size(Min), size(IP) == size(Max),
       Min =< Max, IP >= Min, IP =< Max -> true;
is_in_ip_range(_, _) -> false.

%% @doc Makes IP range from CIDR supplied.
-spec cidr_to_ip_range(IP :: inet:ip_address(), Prefix :: ip_prefix()) ->
                              IpRange :: ip_range().
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
-spec ip_add(IP :: inet:ip_address(), Number :: integer()) ->
                    NewIP :: inet:ip_address().
ip_add(IP, Number) ->
    Bin = ip_to_binary(IP),
    Size = size(Bin) * 8,
    <<Int:Size/big-unsigned>> = Bin,
    Int2 = Int + Number,
    binary_to_ip(<<Int2:Size/big-unsigned>>).

%% @doc Converts IP to binary.
-spec ip_to_binary(IP :: inet:ip_address()) -> Binary :: binary().
ip_to_binary(IP) ->
    case is_ip4(IP) of
        true ->
            list_to_binary(tuple_to_list(IP));
        _ ->
            list_to_binary(
              [<<I:16/big-unsigned>> || I <- tuple_to_list(IP)])
    end.

%% @doc Converts binary to IP address.
-spec binary_to_ip(Binary :: binary()) -> IP :: inet:ip_address().
binary_to_ip(Binary) when size(Binary) == 4 ->
    list_to_tuple(binary_to_list(Binary));
binary_to_ip(Binary) when size(Binary) == 16 ->
    <<A:16/big-unsigned, B:16/big-unsigned,
      C:16/big-unsigned, D:16/big-unsigned,
      E:16/big-unsigned, F:16/big-unsigned,
      G:16/big-unsigned, H:16/big-unsigned>> = Binary,
    {A, B, C, D, E, F, G, H}.

%% @doc Converts IP address to integer.
-spec ip_to_integer(IP :: inet:ip_address()) -> non_neg_integer().
ip_to_integer(IP) ->
    case ip_to_binary(IP) of
        Binary when size(Binary) == 4 ->
            <<Int:32/big-unsigned>> = Binary,
            Int;
        Binary ->
            <<Int:128/big-unsigned>> = Binary,
            Int
    end.

%% @doc Converts integer to IPv4 address.
-spec integer_to_ip4(Int :: non_neg_integer()) -> IPv4 :: inet:ip4_address().
integer_to_ip4(Integer) ->
    binary_to_ip(<<Integer:32/big-unsigned>>).

%% @doc Converts integer to IPv6 address.
-spec integer_to_ip6(Int :: non_neg_integer()) -> IPv6 :: inet:ip6_address().
integer_to_ip6(Integer) ->
    binary_to_ip(<<Integer:128/big-unsigned>>).

%% @doc Returns true if supplied term is of type ip_pool().
-spec is_ip_pool(Term :: any()) -> boolean().
is_ip_pool(List) when is_list(List) ->
    lists:all(
      fun(Term) ->
              is_ip_range(Term) orelse is_ip(Term)
      end, List);
is_ip_pool(_) -> false.

%% @doc Returns true if supplied pool contains IP specified
%%      as first argument.
-spec is_in_ip_pool(IP :: inet:ip_address(), IpPool :: ip_pool()) ->
                           boolean().
is_in_ip_pool(IP, IpPool) ->
    lists:any(
      fun(none) -> false;
         (any) -> true;
         ({_Min, _Max} = IpRange) ->
              is_in_ip_range(IP, IpRange);
         (X) -> IP == X
      end, IpPool).

%% @doc Flattens IP pool.
-spec flatten_ip_pool(IpPool :: ip_pool()) -> NewIpPool :: ip_pool().
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
-spec list_to_ip(String :: nonempty_string()) ->
                        {ok, IP :: inet:ip_address()} | error.
list_to_ip(String) ->
    case list_to_ip4(String) of
        {ok, _IP} = Ok -> Ok;
        error -> list_to_ip6(String)
    end.

%% @doc Convert IPv4 address from text to Erlang representation.
-spec list_to_ip4(String :: nonempty_string()) ->
                         {ok, IP :: inet:ip4_address()} | error.
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
-spec list_to_ip6(String :: nonempty_string()) ->
                         {ok, IP :: inet:ip6_address()} | error.
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
-spec list_to_cidr(String :: nonempty_string()) ->
                          {ok, IP :: inet:ip_address(),
                           PrefixLen :: ip_prefix()} |
                          error.
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
-spec list_to_ip_range(String :: nonempty_string()) ->
                              {ok, {MinIP :: inet:ip_address(),
                                    MaxIP :: inet:ip_address()}} |
                              error.
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

%% @doc Converts string to port number.
-spec list_to_port_number(String :: nonempty_string()) ->
                                 PortNumber :: inet:port_number().
list_to_port_number(String) ->
    Int = list_to_integer(String),
    true = 0 < Int andalso Int =< 16#ffff,
    Int.

%% @doc Converts IP address in Erlang representation to text.
-spec ip_to_list(IP :: inet:ip_address()) -> String :: nonempty_string().
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
-spec socket_to_list(Socket :: {IP :: inet:ip_address(),
                                PortNumber :: inet:port_number()}) ->
                            String :: nonempty_string().
socket_to_list({IP, Port}) ->
    socket_to_list(IP, Port).

%% @equiv socket_to_list({IP, Port})
%% @doc Converts IP address and port number to text.
-spec socket_to_list(IP :: inet:ip_address(),
                     PortNumber :: inet:port_number()) ->
                            String :: nonempty_string().
socket_to_list(IP, Port) ->
    StrIP = ip_to_list(IP),
    Format =
        case is_ip4(IP) of
            true -> "~s:~B";
            _ -> "[~s]:~B"
        end,
    lists:flatten(io_lib:format(Format, [StrIP, Port])).

%% @doc Formats IP range as text.
-spec ip_range_to_list(IpRange :: ip_range()) -> nonempty_string().
ip_range_to_list({Min, Max}) ->
    ip_to_list(Min) ++ "-" ++ ip_to_list(Max).

%% @doc Formats IP pool as text.
-spec ip_pool_to_list(IpPool :: ip_pool()) -> nonempty_string().
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
-spec timestamp(erlang:timestamp()) -> iolist().
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
-spec timestamp(erlang:timestamp(), administrative_offset()) -> iolist().
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
-spec local_offset(erlang:timestamp()) -> administrative_offset().
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
-spec flatten_int_ranges(IntRanges ::
                           [{Min :: integer(), Max :: integer()}]) ->
                                NewIntRanges ::
                                  [{Min :: integer(), Max :: integer()}].
flatten_int_ranges([{A1, A2}, {B1, B2} | Tail]) when A2 >= B1 - 1 ->
    flatten_int_ranges([{A1, lists:max([A2, B2])} | Tail]);
flatten_int_ranges([A | [_ | _] = Tail]) ->
    [A | flatten_int_ranges(Tail)];
flatten_int_ranges(Other) -> Other.

%% @doc Parses IPv4 address from CIDR.
-spec parse_cidr_ip4(String :: nonempty_string()) ->
                            {ok, IP :: inet:ip4_address()} | error.
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
-spec max_zero_group(List :: [integer()]) ->
                            {Pos :: non_neg_integer(),
                             Len :: non_neg_integer()} |
                            undefined.
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

%% ----------------------------------------------------------------------
%% eunit tests
%% ----------------------------------------------------------------------

-ifdef(TEST).

-define(TMP_FILE, "tmp_file").

-spec sleepNecho(Millis :: timer:time()) -> Millis :: timer:time().
sleepNecho(Millis) when is_integer(Millis) ->
    ok = timer:sleep(Millis),
    Millis.

challenge_test_() ->
    [?_assertMatch(
        {ok, 1000},
        challenge(fun sleepNecho/1, [1000, 2000, 3000])),
     ?_assertMatch(
        {ok, 500},
        challenge(fun sleepNecho/1, [500, 1000, 1500])),
     ?_assertMatch(
        undefined,
        challenge(fun sleepNecho/1, [])),
     ?_assertMatch(
        undefined,
        challenge(fun sleepNecho/1, [a, b, c])),
     ?_assertMatch(
        {ok, 500},
        challenge(fun sleepNecho/1, [a, 500, c])),
     ?_assertMatch(
        {ok, 500},
        challenge(fun sleepNecho/1, [a, c, 1000, 500]))
    ].

pmap_test_() ->
    Pids = pmap(fun(_) -> self() end, [1, 2, 3, 4]),
    [?_assertMatch(Pids, uniq(Pids)),
     ?_assertMatch([1.0, 2.0, 3.0, 4.0, 5.0],
                   pmap(fun math:sqrt/1, [1, 4, 9, 16, 25]))
    ].

read_mime_types_test() ->
    ok = file:write_file(?TMP_FILE,
                         "# comment line\n"
                         "a e1 e2 e3\n"
                         "b e4 e5\n"
                         "c e6\n"
                         "d\n"
                         "\n"
                         "f e7\n"),
    ?assertMatch(
       {error, _},
       read_mime_types(?TMP_FILE "z")),
    ?assertMatch(
       {ok, [{"a", ["e1", "e2", "e3"]},
             {"b", ["e4", "e5"]},
             {"c", ["e6"]},
             {"d", []},
             {"f", ["e7"]}]},
       read_mime_types(?TMP_FILE)).

split_by_len_test_() ->
    [?_assertMatch([[1], [2], [3]], split_by_len([1, 2, 3], 1)),
     ?_assertMatch([[1, 2], [3, 4], [5, 6], [7]],
                   split_by_len([1, 2, 3, 4, 5, 6, 7], 2))
    ].

is_string_test_() ->
    [?_assertMatch(true, is_string("")),
     ?_assertMatch(true, is_string("abc")),
     ?_assertMatch(false, is_string(abc)),
     ?_assertMatch(false, is_string(2)),
     ?_assertMatch(false, is_string([0, 127, 256])),
     ?_assertMatch(false, is_string([-1, 127, 255])),
     ?_assertMatch(true, is_string([0, 127, 255]))
    ].

uniq_test_() ->
    [?_assertMatch([1, 2, 3], uniq([1, 2, 3])),
     ?_assertMatch([1, 2, 3], uniq([1, 2, 2, 3])),
     ?_assertMatch([1, 2, 3], uniq([1, 2, 3, 2])),
     ?_assertMatch([1, 2, 3], uniq([1, 1, 2, 2, 3, 2])),
     ?_assertMatch([3, 2, 1], uniq([3, 3, 2, 2, 1, 1, 2])),
     ?_assertMatch([], uniq([])),
     ?_assertMatch([1], uniq([1])),
     ?_assertMatch([1], uniq([1, 1]))
    ].

strip_test_() ->
    [?_assertMatch("abc", strip("abc", "")),
     ?_assertMatch("abc", strip("abc:", ":")),
     ?_assertMatch("abc", strip(":abc:", ":")),
     ?_assertMatch("abc", strip(":;'abc;:", ":;'"))
    ].

list_to_atom_test_() ->
    [?_assertMatch({ok, a}, list_to_atom("a", [a])),
     ?_assertMatch({ok, b}, list_to_atom("b", [a, b])),
     ?_assertMatch(error, list_to_atom("a", []))
    ].

split_lines_test() ->
    ?assertMatch(["", "", "a", "b", "c", ""],
                 split_lines("\n\na\nb\n\rc\r\n\n")).

split_to_key_value_test_() ->
    [?_assertMatch({"key", ""}, split_to_key_value("key")),
     ?_assertMatch({"key", ""}, split_to_key_value("KeY")),
     ?_assertMatch({"key", "value"}, split_to_key_value("KeY\tvalue")),
     ?_assertMatch({"key", "vAlue"}, split_to_key_value("KeY vAlue")),
     ?_assertMatch({"key", "vAlue 2"}, split_to_key_value("KeY vAlue 2")),
     ?_assertMatch({"key", "vAlue"}, split_to_key_value("KeY\t \tvAlue")),
     ?_assertMatch({"k", "V"}, split_to_key_value("K V "))
    ].

preparse_config_test_() ->
    [?_assertMatch([{1, "k", ""},
                    {3, "k", "v"},
                    {4, "k", "V"}],
                   preparse_config("k\n\nk v\nk\tV")),
     ?_assertMatch([{4, "k", ""},
                    {6, "k", "v"},
                    {7, "k", "V"}],
                   preparse_config("#z\n%z\n!z\nk\n\nk v\nk\tV")),
     ?_assertMatch([{4, "k", ""},
                    {6, "k", "v"},
                    {7, "k", "V"}],
                   preparse_config("$z\n$z\n$z\nk\n\nk v\nk\tV",
                                   [{comment_chars, "$"}])),
     ?_assertMatch([{1, "$z", ""},
                    {2, "$z", "s"},
                    {3, "$z", ""},
                    {4, "k", ""},
                    {6, "k", "v"},
                    {7, "k", "V"}],
                   preparse_config("$z\n$z s\n$z\nk\n\nk v\nk\tV",
                                   [{comment_chars, ""}]))].

parse_uri_test_() ->
    [?_assertMatch(
       {ok, [{proto, http},
             {hostname, "host.com"},
             {request, "/path/to/file?request=value"},
             {port, 123},
             {username, "u"}
            ]},
       parse_uri("http://u@host.com:123/path/to/file?request=value"))
    ].

is_ip_test_() ->
    [?_assertMatch(true, is_ip({0,0,0,0})),
     ?_assertMatch(true, is_ip({0,0,0,1})),
     ?_assertMatch(false, is_ip({0,0,0,-1})),
     ?_assertMatch(true, is_ip({0,0,0,16#ff})),
     ?_assertMatch(false, is_ip({0,0,0,16#ff + 1})),
     ?_assertMatch(true, is_ip({0,0,0,0,0,0,0,0})),
     ?_assertMatch(true, is_ip({0,0,0,0,0,0,0,1})),
     ?_assertMatch(false, is_ip({0,0,0,0,0,0,0,-1})),
     ?_assertMatch(true, is_ip({0,0,0,0,0,0,0,16#ffff})),
     ?_assertMatch(false, is_ip({0,0,0,0,0,0,0,16#ffff + 1}))
    ].

is_ip4_test_() ->
    [?_assertMatch(true, is_ip({0,0,0,0})),
     ?_assertMatch(true, is_ip({0,1,0,0})),
     ?_assertMatch(false, is_ip({0,-1,0,0})),
     ?_assertMatch(true, is_ip({0,16#ff,0,0})),
     ?_assertMatch(false, is_ip({0,16#ff + 1,0,0}))].

is_ip6_test_() ->
    [?_assertMatch(true, is_ip({0,0,0,0,0,0,0,0})),
     ?_assertMatch(true, is_ip({0,0,0,0,1,0,0,0})),
     ?_assertMatch(false, is_ip({0,0,0,0,-1,0,0,0})),
     ?_assertMatch(true, is_ip({0,0,0,0,16#ffff,0,0,0})),
     ?_assertMatch(false, is_ip({0,0,0,0,16#ffff + 1,0,0,0}))].

is_same_ip_type_test_() ->
    [?_assertMatch(true, is_same_ip_type({0,0,0,0}, {1,1,1,1})),
     ?_assertMatch(false, is_same_ip_type({0,0,0,0}, {1,1,1,1,1,1,1,1}))
    ].

ip_family_test_() ->
    [?_assertMatch(inet, ip_family({1,1,1,1})),
     ?_assertMatch(inet6, ip_family({1,1,1,1,1,1,1,1}))
    ].

is_ip_range_test_() ->
    [?_assertMatch(true, is_ip_range({{0,0,0,0}, {1,1,1,1}})),
     ?_assertMatch(false, is_ip_range({{1,1,1,1}, {0,0,0,0}})),
     ?_assertMatch(false, is_ip_range({1,1,1,1})),
     ?_assertMatch(false, is_ip_range(atom)),
     ?_assertMatch(false, is_ip_range({{0,0,0,0}, {1,1,1,1,1,1,1,1}}))
    ].

is_in_ip_range_test_() ->
    [?_assertMatch(true, is_in_ip_range({1,1,1,1}, {{0,0,0,0}, {2,2,2,2}})),
     ?_assertMatch(true, is_in_ip_range({1,1,1,1}, {{1,1,1,0}, {1,1,1,2}})),
     ?_assertMatch(true, is_in_ip_range({1,1,1,1}, {{1,1,1,1}, {1,1,1,2}})),
     ?_assertMatch(true, is_in_ip_range({1,1,1,1}, {{1,1,1,0}, {1,1,1,1}})),
     ?_assertMatch(false, is_in_ip_range({1,1,1,1}, {{1,1,1,2}, {1,1,1,3}})),
     ?_assertMatch(false, is_in_ip_range({1,1,1,2}, {{1,1,1,0}, {1,1,1,1}})),
     ?_assertMatch(true, is_in_ip_range({1,1,1,1,1,1,1,1},
                                        {{1,1,1,1,0,0,0,0},
                                         {1,1,1,1,2,2,2,2}})),
     ?_assertMatch(true, is_in_ip_range({1,1,1,1,1,1,1,1},
                                        {{1,1,1,1,1,1,1,0},
                                         {1,1,1,1,1,1,1,2}})),
     ?_assertMatch(true, is_in_ip_range({1,1,1,1,1,1,1,1},
                                        {{1,1,1,1,1,1,1,1},
                                         {1,1,1,1,1,1,1,2}})),
     ?_assertMatch(true, is_in_ip_range({1,1,1,1,1,1,1,1},
                                        {{1,1,1,1,1,1,1,0},
                                         {1,1,1,1,1,1,1,1}})),
     ?_assertMatch(false, is_in_ip_range({1,1,1,1,1,1,1,1},
                                         {{1,1,1,1,1,1,1,2},
                                          {1,1,1,1,1,1,1,3}})),
     ?_assertMatch(false, is_in_ip_range({1,1,1,1,1,1,1,2},
                                         {{1,1,1,1,1,1,1,0},
                                          {1,1,1,1,1,1,1,1}}))
    ].

cidr_to_ip_range_test_() ->
    [?_assertMatch({{1,1,1,0},{1,1,1,16#ff}},
                   cidr_to_ip_range({1,1,1,1}, 24)),
     ?_assertMatch({{1,1,0,0},{1,1,16#ff,16#ff}},
                   cidr_to_ip_range({1,1,1,1}, 16)),
     ?_assertMatch({{1,0,0,0},{1,16#ff,16#ff,16#ff}},
                   cidr_to_ip_range({1,1,1,1}, 8)),
     ?_assertMatch({{1,1,1,1},{1,1,1,1}},
                   cidr_to_ip_range({1,1,1,1}, 32)),
     ?_assertMatch({{0,0,0,0},{16#ff,16#ff,16#ff,16#ff}},
                   cidr_to_ip_range({1,1,1,1}, 0)),
     ?_assertMatch({{1,1,0,0,0,0,0,0},
                    {1,1,16#ffff,16#ffff,16#ffff,
                     16#ffff,16#ffff,16#ffff}},
                   cidr_to_ip_range({1,1,1,1,1,1,1,1}, 32)),
     ?_assertMatch({{1,0,0,0,0,0,0,0},
                    {1,16#ffff,16#ffff,16#ffff,16#ffff,
                     16#ffff,16#ffff,16#ffff}},
                   cidr_to_ip_range({1,1,1,1,1,1,1,1}, 16)),
     ?_assertMatch({{0,0,0,0,0,0,0,0},
                    {16#ff,16#ffff,16#ffff,16#ffff,16#ffff,
                     16#ffff,16#ffff,16#ffff}},
                   cidr_to_ip_range({1,1,1,1,1,1,1,1}, 8)),
     ?_assertMatch({{1,1,1,1,1,1,1,1},{1,1,1,1,1,1,1,1}},
                   cidr_to_ip_range({1,1,1,1,1,1,1,1}, 128)),
     ?_assertMatch({{0,0,0,0,0,0,0,0},
                    {16#ffff,16#ffff,16#ffff,16#ffff,
                     16#ffff,16#ffff,16#ffff,16#ffff}},
                   cidr_to_ip_range({1,1,1,1,1,1,1,1}, 0))
    ].

ip_add_test_() ->
    [?_assertMatch({0,0,0,1}, ip_add({0,0,0,0}, 1)),
     ?_assertMatch({0,0,0,2}, ip_add({0,0,0,0}, 2)),
     ?_assertMatch({0,0,0,1}, ip_add({0,0,0,2}, -1)),
     ?_assertMatch({0,0,0,10}, ip_add({0,0,0,19}, -9)),
     ?_assertMatch({0,1,0,0}, ip_add({0,0,0,0}, 16#10000)),
     ?_assertMatch({0,0,0,0,0,0,0,1}, ip_add({0,0,0,0,0,0,0,0}, 1)),
     ?_assertMatch({0,0,0,0,0,0,0,2}, ip_add({0,0,0,0,0,0,0,0}, 2)),
     ?_assertMatch({0,0,0,0,0,0,0,1}, ip_add({0,0,0,0,0,0,0,2}, -1)),
     ?_assertMatch({0,0,0,0,0,0,0,10}, ip_add({0,0,0,0,0,0,0,19}, -9)),
     ?_assertMatch({0,0,0,0,0,0,1,0}, ip_add({0,0,0,0,0,0,0,0}, 16#10000))
    ].

ip_to_binary_test_() ->
    [?_assertMatch(<<0,1,0,1>>, ip_to_binary({0,1,0,1})),
     ?_assertMatch(<<2,1,2,1>>, ip_to_binary({2,1,2,1})),
     ?_assertMatch(<<1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1>>,
                   ip_to_binary({16#101,16#101,16#101,16#101,
                                 16#101,16#101,16#101,16#101}))
    ].

binary_to_ip_test_() ->
    [?_assertMatch({0,1,0,1}, binary_to_ip(<<0,1,0,1>>)),
     ?_assertMatch({2,1,2,1}, binary_to_ip(<<2,1,2,1>>)),
     ?_assertMatch({16#101,16#101,16#101,16#101,
                    16#101,16#101,16#101,16#101},
                   binary_to_ip(<<1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1>>))
    ].

ip_to_integer_test_() ->
    [?_assertMatch(0, ip_to_integer({0,0,0,0})),
     ?_assertMatch(0, ip_to_integer({0,0,0,0,0,0,0,0})),
     ?_assertMatch(1, ip_to_integer({0,0,0,1})),
     ?_assertMatch(1, ip_to_integer({0,0,0,0,0,0,0,1})),
     ?_assertMatch(16#100, ip_to_integer({0,0,1,0})),
     ?_assertMatch(16#10000, ip_to_integer({0,0,0,0,0,0,1,0}))
    ].

integer_to_ip4_test_() ->
    [?_assertMatch({0,0,0,0}, integer_to_ip4(0)),
     ?_assertMatch({0,0,0,1}, integer_to_ip4(1)),
     ?_assertMatch({0,0,1,0}, integer_to_ip4(16#100))
    ].

integer_to_ip6_test_() ->
    [?_assertMatch({0,0,0,0,0,0,0,0}, integer_to_ip6(0)),
     ?_assertMatch({0,0,0,0,0,0,0,1}, integer_to_ip6(1)),
     ?_assertMatch({0,0,0,0,0,0,1,0}, integer_to_ip6(16#10000))
    ].

is_ip_pool_test_() ->
    [?_assertMatch(true, is_ip_pool([])),
     ?_assertMatch(true, is_ip_pool([{0,0,0,0}])),
     ?_assertMatch(false, is_ip_pool([{0,0,0,0}, garbage])),
     ?_assertMatch(false, is_ip_pool(garbage)),
     ?_assertMatch(true, is_ip_pool([{0,0,0,0}, {{1,1,1,1},{2,2,2,2}}])),
     ?_assertMatch(true, is_ip_pool([{0,0,0,0,0,0,0,0}])),
     ?_assertMatch(false, is_ip_pool([{0,0,0,0,0,0,0,0}, garbage])),
     ?_assertMatch(true, is_ip_pool([{0,0,0,0,0,0,0,0},
                                     {{0,0,0,0,1,1,1,1},
                                      {0,0,0,0,2,2,2,2}}]))
    ].

is_in_ip_pool_test_() ->
    [?_assertMatch(
        true, is_in_ip_pool({1,1,1,1}, [{{0,0,0,0}, {2,2,2,2}}])),
     ?_assertMatch(
        false, is_in_ip_pool({0,0,0,0}, [{{1,1,1,1}, {2,2,2,2}}])),
     ?_assertMatch(
        true, is_in_ip_pool({0,0,0,0}, [{{1,1,1,1}, {2,2,2,2}}, {0,0,0,0}])),
     ?_assertMatch(
        true, is_in_ip_pool({0,0,0,0,1,1,1,1},
                            [{{0,0,0,0,0,0,0,0}, {0,0,0,0,2,2,2,2}}])),
     ?_assertMatch(
        false, is_in_ip_pool({0,0,0,0,0,0,0,0},
                             [{{0,0,0,0,1,1,1,1}, {0,0,0,0,2,2,2,2}}])),
     ?_assertMatch(
        true, is_in_ip_pool({0,0,0,0,0,0,0,0},
                            [{{0,0,0,0,1,1,1,1}, {0,0,0,0,2,2,2,2}},
                             {0,0,0,0,0,0,0,0}]))
    ].

flatten_ip_pool_test_() ->
    [?_assertMatch([{1,1,1,1}], flatten_ip_pool([{1,1,1,1}, {1,1,1,1},
                                                 {{1,1,1,1}, {1,1,1,1}}])),
     ?_assertMatch([{{1,1,1,1},{1,1,1,2}}],
                   flatten_ip_pool([{1,1,1,1}, {1,1,1,2}])),
     ?_assertMatch([{{1,1,1,1},{1,1,1,3}}],
                   flatten_ip_pool([{1,1,1,1}, {1,1,1,3}, {1,1,1,2}])),
     ?_assertMatch([{0,0,0,0,1,1,1,1}],
                   flatten_ip_pool([{0,0,0,0,1,1,1,1}, {0,0,0,0,1,1,1,1},
                                    {{0,0,0,0,1,1,1,1},
                                     {0,0,0,0,1,1,1,1}}])),
     ?_assertMatch([{{0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,2}}],
                   flatten_ip_pool([{0,0,0,0,1,1,1,1}, {0,0,0,0,1,1,1,2}])),
     ?_assertMatch([{{0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,3}}],
                   flatten_ip_pool([{0,0,0,0,1,1,1,1}, {0,0,0,0,1,1,1,3},
                                    {0,0,0,0,1,1,1,2}]))
    ].

list_to_ip_test_() ->
    [?_assertMatch({ok,{0,0,0,0}}, list_to_ip("0.0.0.0")),
     ?_assertMatch(error, list_to_ip("0.0.0.0z")),
     ?_assertMatch({ok,{1,0,0,1}}, list_to_ip("1.0.0.1")),
     ?_assertMatch({ok,{1,1,1,1,0,0,0,0}}, list_to_ip("1:1:1:1::")),
     ?_assertMatch(error, list_to_ip("::g")),
     ?_assertMatch({ok,{1,0,0,0,1,2,2,2}}, list_to_ip("1::1:2:2:2")),
     ?_assertMatch({ok,{16#ff,0,0,0,0,0,0,16#eeee}}, list_to_ip("ff::eeee")),
     ?_assertMatch({ok,{0,0,0,0,0,0,0,0}}, list_to_ip("::"))
    ].

list_to_ip4_test_() ->
    [?_assertMatch({ok,{0,0,0,0}}, list_to_ip("0.0.0.0")),
     ?_assertMatch(error, list_to_ip("0.0.0.0z")),
     ?_assertMatch({ok,{1,0,0,1}}, list_to_ip("1.0.0.1"))].

list_to_ip6_test_() ->
    [?_assertMatch({ok,{1,1,1,1,0,0,0,0}}, list_to_ip("1:1:1:1::")),
     ?_assertMatch(error, list_to_ip("::g")),
     ?_assertMatch({ok,{1,0,0,0,1,2,2,2}}, list_to_ip("1::1:2:2:2")),
     ?_assertMatch({ok,{16#ff,0,0,0,0,0,0,16#eeee}}, list_to_ip("ff::eeee")),
     ?_assertMatch({ok,{0,0,0,0,0,0,0,0}}, list_to_ip("::"))].

list_to_cidr_test_() ->
    [?_assertMatch({ok, {1,1,1,1}, 32}, list_to_cidr("1.1.1.1/32")),
     ?_assertMatch({ok, {1,1,1,1}, 24}, list_to_cidr("1.1.1.1/24")),
     ?_assertMatch({ok, {1,1,0,1}, 8}, list_to_cidr("1.1.0.1/8")),
     ?_assertMatch({ok, {1,1,0,1}, 0}, list_to_cidr("1.1.0.1/0")),
     ?_assertMatch(error, list_to_cidr("1.1.0.1/-0")),
     ?_assertMatch(error, list_to_cidr("1.1.0.1/33")),
     ?_assertMatch({ok, {0,0,0,0,1,1,1,1}, 128},
                   list_to_cidr("::1:1:1:1/128")),
     ?_assertMatch({ok, {0,0,0,0,1,1,1,1}, 24},
                   list_to_cidr("::1:1:1:1/24")),
     ?_assertMatch({ok, {0,0,0,0,1,1,0,1}, 8},
                   list_to_cidr("::1:1:0:1/8")),
     ?_assertMatch({ok, {0,0,0,0,1,1,0,1}, 0},
                   list_to_cidr("::1:1:0:1/0")),
     ?_assertMatch(error, list_to_cidr("::1:1:0:1/-0")),
     ?_assertMatch(error, list_to_cidr("::1:1:0:1/129"))
    ].

list_to_ip_range_test_() ->
    [?_assertMatch({ok, {{1,1,1,1},{1,1,1,1}}},
                   list_to_ip_range("1.1.1.1")),
     ?_assertMatch({ok, {{1,1,1,0},{1,1,1,255}}},
                   list_to_ip_range("1.1.1.1/24")),
     ?_assertMatch({ok, {{1,1,0,0},{1,1,255,255}}},
                   list_to_ip_range("1.1.1.1/16")),
     ?_assertMatch({ok, {{1,1,1,1},{1,1,1,4}}},
                   list_to_ip_range("1.1.1.1-1.1.1.4")),
     ?_assertMatch({ok, {{1,1,1,1},{1,1,1,4}}},
                   list_to_ip_range("1.1.1.1,1.1.1.4")),
     ?_assertMatch({ok, {{1,1,1,1},{1,1,1,4}}},
                   list_to_ip_range("1.1.1.1 1.1.1.4")),
     ?_assertMatch({ok, {{0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,1}}},
                   list_to_ip_range("::1:1:1:1")),
     ?_assertMatch({ok, {{0,0,0,0,1,1,1,0},{0,0,0,0,1,1,1,16#ffff}}},
                   list_to_ip_range("::1:1:1:1/112")),
     ?_assertMatch({ok, {{0,0,0,0,1,1,0,0},{0,0,0,0,1,1,16#ffff,16#ffff}}},
                   list_to_ip_range("::1:1:1:1/96")),
     ?_assertMatch({ok, {{0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,4}}},
                   list_to_ip_range("::1:1:1:1-::1:1:1:4")),
     ?_assertMatch({ok, {{0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,4}}},
                   list_to_ip_range("::1:1:1:1,::1:1:1:4")),
     ?_assertMatch({ok, {{0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,4}}},
                   list_to_ip_range("::1:1:1:1 ::1:1:1:4"))
    ].

list_to_port_number_test_() ->
    [?_assertMatch(1, list_to_port_number("1")),
     ?_assertMatch(10, list_to_port_number("10")),
     ?_assertMatch(255, list_to_port_number("255")),
     ?_assertMatch(16#ffff, list_to_port_number("65535")),
     ?_assertError(_, list_to_port_number("0")),
     ?_assertError(_, list_to_port_number("1.1")),
     ?_assertError(_, list_to_port_number("1a")),
     ?_assertError(_, list_to_port_number("z")),
     ?_assertError(_, list_to_port_number("-1")),
     ?_assertError(_, list_to_port_number("65536"))
    ].

ip_to_list_test_() ->
    [?_assertMatch("0.0.0.0", ip_to_list({0,0,0,0})),
     ?_assertMatch("1.0.0.1", ip_to_list({1,0,0,1})),
     ?_assertMatch("1:1:1:1::", ip_to_list({1,1,1,1,0,0,0,0})),
     ?_assertMatch("1::1:2:2:2", ip_to_list({1,0,0,0,1,2,2,2})),
     ?_assertMatch("ff::eeee", ip_to_list({16#ff,0,0,0,0,0,0,16#eeee})),
     ?_assertMatch("::", ip_to_list({0,0,0,0,0,0,0,0}))
    ].

socket_to_list_test_() ->
    [?_assertMatch("1.1.1.1:1", socket_to_list({1,1,1,1}, 1)),
     ?_assertMatch("[::]:1", socket_to_list({0,0,0,0,0,0,0,0}, 1))
    ].

ip_range_to_list_test_() ->
    [?_assertMatch("1.1.1.1-2.2.2.2",
                   ip_range_to_list({{1,1,1,1}, {2,2,2,2}})),
     ?_assertMatch("::1:1:1:1-::2:2:2:2",
                   ip_range_to_list({{0,0,0,0,1,1,1,1}, {0,0,0,0,2,2,2,2}}))
    ].

ip_pool_to_list_test_() ->
    [?_assertMatch(
        "1.1.1.1;2.2.2.2-3.3.3.3;4.4.4.4;5.5.5.5-6.6.6.6",
        ip_pool_to_list(
          [{1,1,1,1},{{2,2,2,2},{3,3,3,3}},{4,4,4,4},{{5,5,5,5},{6,6,6,6}}]
         )),
     ?_assertMatch(
        "::1:1:1:1;::2:2:2:2-::3:3:3:3;::4:4:4:4;::5:5:5:5-::6:6:6:6",
        ip_pool_to_list(
          [{0,0,0,0,1,1,1,1},{{0,0,0,0,2,2,2,2},{0,0,0,0,3,3,3,3}},
           {0,0,0,0,4,4,4,4},{{0,0,0,0,5,5,5,5},{0,0,0,0,6,6,6,6}}])),
     ?_assertMatch("any", ip_pool_to_list([any]))
    ].

-endif.

