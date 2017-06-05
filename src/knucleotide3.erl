% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%% contributed by Pierre Fenoll based on an earlier submission
%%             by Fredrik Svahn based on an earlier submission
%%             by Kenneth Johansson, Vlad Dumitrescu and Ulf Wiger

-module(knucleotide3).
-export([main/1]).

%% Read and discard until start of third segment
seek_three() ->
    case io:get_line('') of
	<<">TH", _/binary>> -> done;
	eof -> erlang:error(eof);
	_ -> seek_three()
    end.

visit(<<N:1/binary, Ucleotides:17/binary, Rest/binary>>) ->
    store(<<N:1/binary, Ucleotides:17/binary>>),
    case byte_size(Rest) =:= 0 andalso get_line() of
        false -> visit(<<Ucleotides/binary, Rest/binary>>);
        eof -> done;
        More -> visit(<<Ucleotides/binary, More/binary>>)
    end.

-define(TWELVE(_01, _02, _03, _04, _05, _06, _07, _08, _09, _10, _11, _12)
       ,<<_01:1/binary, _02:1/binary, _03:1/binary, _04:1/binary, _05:1/binary, _06:1/binary, _07:1/binary, _08:1/binary, _09:1/binary, _10:1/binary, _11:1/binary, _12:1/binary>>).
-define(SIX(_01, _02, _03, _04, _05, _06)
       ,<<_01:1/binary, _02:1/binary, _03:1/binary, _04:1/binary, _05:1/binary, _06:1/binary>>).
-define(FOUR(_01, _02, _03, _04)
       ,<<_01:1/binary, _02:1/binary, _03:1/binary, _04:1/binary>>).
-define(THREE(_01, _02, _03), <<_01:1/binary, _02:1/binary, _03:1/binary>>).
-define(TWO(_01, _02), <<_01:1/binary, _02:1/binary>>).

store(<<_01:1/binary, _02:1/binary, _03:1/binary, _04:1/binary, _05:1/binary, _06:1/binary, _07:1/binary, _08:1/binary, _09:1/binary,
        _10:1/binary, _11:1/binary, _12:1/binary, _13:1/binary, _14:1/binary, _15:1/binary, _16:1/binary, _17:1/binary, _18:1/binary>>=Nucleotides) ->
    incr(Nucleotides),

    incr(?TWELVE(_01, _02, _03, _04, _05, _06, _07, _08, _09, _10, _11, _12)),
    incr(?TWELVE(_02, _03, _04, _05, _06, _07, _08, _09, _10, _11, _12, _13)),
    incr(?TWELVE(_03, _04, _05, _06, _07, _08, _09, _10, _11, _12, _13, _14)),
    incr(?TWELVE(_04, _05, _06, _07, _08, _09, _10, _11, _12, _13, _14, _15)),
    incr(?TWELVE(_05, _06, _07, _08, _09, _10, _11, _12, _13, _14, _15, _16)),
    incr(?TWELVE(_06, _07, _08, _09, _10, _11, _12, _13, _14, _15, _16, _17)),
    incr(?TWELVE(_07, _08, _09, _10, _11, _12, _13, _14, _15, _16, _17, _18)),

    incr(?SIX(_01, _02, _03, _04, _05, _06)),
    incr(?SIX(_02, _03, _04, _05, _06, _07)),
    incr(?SIX(_03, _04, _05, _06, _07, _08)),
    incr(?SIX(_04, _05, _06, _07, _08, _09)),
    incr(?SIX(_05, _06, _07, _08, _09, _10)),
    incr(?SIX(_06, _07, _08, _09, _10, _11)),
    incr(?SIX(_07, _08, _09, _10, _11, _12)),
    incr(?SIX(_08, _09, _10, _11, _12, _13)),
    incr(?SIX(_09, _10, _11, _12, _13, _14)),
    incr(?SIX(_10, _11, _12, _13, _14, _15)),
    incr(?SIX(_11, _12, _13, _14, _15, _16)),
    incr(?SIX(_12, _13, _14, _15, _16, _17)),
    incr(?SIX(_13, _14, _15, _16, _17, _18)),

    incr(?FOUR(_01, _02, _03, _04)),
    incr(?FOUR(_02, _03, _04, _05)),
    incr(?FOUR(_03, _04, _05, _06)),
    incr(?FOUR(_04, _05, _06, _07)),
    incr(?FOUR(_05, _06, _07, _08)),
    incr(?FOUR(_06, _07, _08, _09)),
    incr(?FOUR(_07, _08, _09, _10)),
    incr(?FOUR(_08, _09, _10, _11)),
    incr(?FOUR(_09, _10, _11, _12)),
    incr(?FOUR(_10, _11, _12, _13)),
    incr(?FOUR(_11, _12, _13, _14)),
    incr(?FOUR(_12, _13, _14, _15)),
    incr(?FOUR(_13, _14, _15, _16)),
    incr(?FOUR(_14, _15, _16, _17)),
    incr(?FOUR(_15, _16, _17, _18)),

    incr(?THREE(_01, _02, _03)),
    incr(?THREE(_02, _03, _04)),
    incr(?THREE(_03, _04, _05)),
    incr(?THREE(_04, _05, _06)),
    incr(?THREE(_05, _06, _07)),
    incr(?THREE(_06, _07, _08)),
    incr(?THREE(_07, _08, _09)),
    incr(?THREE(_08, _09, _10)),
    incr(?THREE(_09, _10, _11)),
    incr(?THREE(_10, _11, _12)),
    incr(?THREE(_11, _12, _13)),
    incr(?THREE(_12, _13, _14)),
    incr(?THREE(_13, _14, _15)),
    incr(?THREE(_14, _15, _16)),
    incr(?THREE(_15, _16, _17)),
    incr(?THREE(_16, _17, _18)),

    incr(?TWO(_01, _02)),
    incr(?TWO(_02, _03)),
    incr(?TWO(_03, _04)),
    incr(?TWO(_04, _05)),
    incr(?TWO(_05, _06)),
    incr(?TWO(_06, _07)),
    incr(?TWO(_07, _08)),
    incr(?TWO(_08, _09)),
    incr(?TWO(_09, _10)),
    incr(?TWO(_10, _11)),
    incr(?TWO(_11, _12)),
    incr(?TWO(_12, _13)),
    incr(?TWO(_13, _14)),
    incr(?TWO(_14, _15)),
    incr(?TWO(_15, _16)),
    incr(?TWO(_16, _17)),
    incr(?TWO(_17, _18)),

    [incr(<<One>>) || <<One>> <= Nucleotides],
    ok.

get_line() ->
    case io:get_line('') of
        eof -> eof;
        Line ->
            <<<<(C - ($a - $A))>> || <<C>> <= Line,
                                     C =/= $\n
            >>
    end.

%% Update hash table counter for already existing pattern or insert new
incr(Key) ->
    case get(Key) of
        undefined -> put(Key, 1);
        Value -> put(Key, Value + 1)
    end.

%% Print the frequency table in the right order
print_freq_table(Pattern) ->
    KVs = [{Nucleotid, get(Nucleotid)} || Nucleotid <- nucleotids(Pattern)],
    FreqList = lists:reverse(lists:keysort(2, KVs)),
    Total = lists:foldr(fun({_, Count}, Acc) -> Acc + Count end, 0, FreqList),
    [io:fwrite("~s ~.3f\n", [Nucleotid, Count * 100 / Total])
     || {Nucleotid, Count} <- FreqList
    ],
    io:fwrite("\n").

nucleotids('?') -> [<<"A">>, <<"T">>, <<"G">>, <<"C">>];
nucleotids('??') ->
    ATGC = nucleotids('?'),
    [<<(I)/binary, (J)/binary>> || I <- ATGC, J <- ATGC].

%% Print number of occurrences for a specific pattern
print_count(Pattern) ->
    V = case get(Pattern) of
            undefined -> 0;
            Value -> Value
        end,
    io:fwrite("~w\t~s\n", [V, Pattern]).

main(_Arg) ->
    io:setopts(standard_io, [binary]),
    seek_three(),
    visit(get_line()),
    print_freq_table('?'),
    print_freq_table('??'),
    print_count(<<"GGT">>),
    print_count(<<"GGTA">>),
    print_count(<<"GGTATT">>),
    print_count(<<"GGTATTTTAATT">>),
    print_count(<<"GGTATTTTAATTTATAGT">>).
