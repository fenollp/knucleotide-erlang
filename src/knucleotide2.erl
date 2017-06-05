% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%% contributed by Pierre Fenoll based on an earlier submission
%%             by Fredrik Svahn based on an earlier submission
%%             by Kenneth Johansson, Vlad Dumitrescu and Ulf Wiger

-module(knucleotide2).
-export([main/1]).

%% Read and discard until start of third segment
seek_three() ->
    case io:get_line('') of
	<<">TH", _/binary>> -> done;
	eof -> erlang:error(eof);
	_ -> seek_three()
    end.

%% Read third segment
get_seq_three(Seq) ->
    case io:get_line('') of
	eof -> iolist_to_binary(lists:reverse(Seq));
	Str ->
            Upper = <<<<(C - ($a - $A))>> || <<C>> <= Str,
                                             C =/= $\n
                    >>,
            get_seq_three([Upper|Seq])
    end.

%% Generate frequency hash table
gen_freq_table(Seq, Len) ->
    gen_freq_table(Seq, Len, byte_size(Seq) - Len).

gen_freq_table(_, _, -1) -> done;
gen_freq_table(Seq, Len, Dec) ->
    <<_:Dec/binary, Key:Len/binary, _/binary>> = Seq,
    update_counter(Key),
    gen_freq_table(Seq, Len, Dec - 1).

%% Update hash table counter for already existing pattern or insert new
update_counter(Key) ->
    case get(Key) of
        undefined -> put(Key, 1);
        Value -> put(Key, Value + 1)
    end.

%% Print the frequency table in the right order
print_freq_table(_Pattern) ->
    FreqList = lists:reverse(lists:keysort(2, get())),
    Total = lists:foldr(fun({_, Count}, Acc) -> Acc + Count end, 0, FreqList),
    [io:fwrite("~s ~.3f\n", [Nucleoid, Count * 100 / Total])
     || {Nucleoid, Count} <- FreqList
    ],
    io:fwrite("\n").

%% Print number of occurrences for a specific pattern
print_count(Pattern) ->
    case get(Pattern) of
        undefined -> io:fwrite("~w\t~s\n", [0, Pattern]);
        Value -> io:fwrite("~w\t~s\n", [Value, Pattern])
    end.

%% Spawn a worker process with its own hash table
do({PrintFun, Pattern}, Seq) ->
    spawn(fun() ->
                  gen_freq_table(Seq, byte_size(Pattern)),
                  %% Work is done, wait for token and print
                  receive Pids ->
                          PrintFun(Pattern),
                          hd(Pids) ! tl(Pids)
                  end
          end
         ).

main(_Arg) ->
    io:setopts(standard_io, [binary]),
    seek_three(),
    Seq = get_seq_three([]),
    Pids = [do(Action, Seq)
            || Action <- [{fun print_freq_table/1, <<"?">>}
                         ,{fun print_freq_table/1, <<"??">>}
                         ,{fun print_count/1, <<"GGT">>}
                         ,{fun print_count/1, <<"GGTA">>}
                         ,{fun print_count/1, <<"GGTATT">>}
                         ,{fun print_count/1, <<"GGTATTTTAATT">>}
                         ,{fun print_count/1, <<"GGTATTTTAATTTATAGT">>}
                         ]
           ],
    %% Pass token to print in order
    hd(Pids) ! tl(Pids) ++ [self()],
    receive _Pid -> halt(0) end.
