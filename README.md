# knucleotide-erlang

Attempt at a "better" Erlang knucleotide implementation for the Benchmarking Game.

Original winning Erlang (HiPE) submission code in [src/knucleotide.erl](src/knucleotide.erl).



## [Original results](http://benchmarksgame.alioth.debian.org/u64q/knucleotide.html)

```
× source secs mem gz cpu cpu load
1 Rust #4 5.27 158,224 1728 16.66 99% 58% 91% 71%
31 Erlang HiPE #3 163.56 1,077,148 932 7 min 51% 57% 99% 64%
33 Erlang #3 175.10 892,448 932 8 min 69% 71% 94% 56%
39 Erlang HiPE 206.47 3,561,784 930 7 min 58% 83% 76% 46%
46 Erlang 242.57 3,590,628 930 8 min 57% 71% 71% 71%
```
