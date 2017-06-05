% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%% contributed by Pierre Fenoll based on an earlier submission
%%             by Fredrik Svahn based on an earlier submission
%%             by Kenneth Johansson, Vlad Dumitrescu and Ulf Wiger

-module(knucleotide2).
-export([main/1]).

to_upper(Str) ->
    <<<<(C - ($a - $A))>> || <<C>> <= Str,
                             C =/= $\n
    >>.

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
	%% Str -> get_seq_three([to_upper(Str, []) | Seq])
	Str -> get_seq_three([to_upper(Str) | Seq])
    end.

%% Generate frequency hash table
gen_freq_table(FreqT, Seq, Len) ->
    gen_freq_table(Seq, Len, FreqT, byte_size(Seq) - Len).

gen_freq_table(_, _, _, -1) -> done;
gen_freq_table(Seq, Len, FreqT, Dec) ->
    <<_:Dec/binary, Key:Len/binary, _/binary>> = Seq,
    update_counter(Key, FreqT),
    gen_freq_table(Seq, Len, FreqT, Dec - 1).

%% Update hash table counter for already existing pattern or insert new
update_counter(Key, FreqT) ->
    try ets:update_counter(FreqT, Key, 1) of _ -> ok
    catch error:badarg -> ets:insert(FreqT, {Key, 1})
    end.

%% Print the frequency table in the right order
print_freq_table(FreqT, _Pattern) ->
    FreqList = lists:reverse(lists:keysort(2, ets:tab2list(FreqT))),
    Total = lists:foldr(fun({_, Count}, Acc) -> Acc + Count end, 0, FreqList),
    [io:fwrite("~s ~.3f\n", [Nucleoid, Count * 100 / Total])
     || {Nucleoid, Count} <- FreqList
    ],
    io:fwrite("\n").

%% Print number of occurrences for a specific pattern
print_count(FreqT, Pattern) ->
    case ets:lookup(FreqT, Pattern) of
	[{_, Value}] -> io:fwrite("~w\t~s\n", [Value, Pattern]);
	[] -> io:fwrite("~w\t~s\n", [0, Pattern])
    end.

%% Spawn a worker process with its own hash table
do({PrintFun, Pattern}, Seq) ->
    spawn(fun() ->
                  FreqT = ets:new(hash, [set]),
                  gen_freq_table(FreqT, Seq, byte_size(Pattern)),
                  %% Work is done, wait for token and print
                  receive Pids ->
                          PrintFun(FreqT, Pattern),
                          hd(Pids) ! tl(Pids)
                  end,
                  ets:delete(FreqT)
          end
         ).

main(_Arg) ->
    io:setopts(standard_io, [binary]),
    seek_three(),
    Seq = get_seq_three([]),
    Pids = [do(Action, Seq)
            || Action <- [{fun print_freq_table/2, <<"?">>}
                         ,{fun print_freq_table/2, <<"??">>}
                         ,{fun print_count/2, <<"GGT">>}
                         ,{fun print_count/2, <<"GGTA">>}
                         ,{fun print_count/2, <<"GGTATT">>}
                         ,{fun print_count/2, <<"GGTATTTTAATT">>}
                         ,{fun print_count/2, <<"GGTATTTTAATTTATAGT">>}
                         ]
           ],
    %% Pass token to print in right order
    hd(Pids) ! tl(Pids) ++ [self()],
    receive _Pid -> halt(0) end.
