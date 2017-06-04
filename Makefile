ERLC = erlc
ERL = erl -smp enable -noshell

BENCH_IN = fasta/knucleotide-input25000000.txt
BENCH_OUT = priv/knucleotide-output25000000.txt

.PHONY: $(BENCH_IN)

all: $(BENCH_IN) original
	time $(ERL) -run knucleotide main 0 < $(BENCH_IN) > $(BENCH_OUT)
	git --no-pager diff --cached -- $(BENCH_OUT)
	bash -c '[[ 0 -eq $$(git status --porcelain $(BENCH_OUT) | wc -l) ]]'

$(BENCH_IN):
	$(MAKE) -C fasta/

original: src/knucleotide.erl
	$(ERLC) +native +'{hipe, [o3]}' $^

test: IN = priv/knucleotide-input.txt
test: OUT = priv/knucleotide-output.txt
test: original
	time $(ERL) -run knucleotide main 0 < $(IN) > $(OUT)
	git --no-pager diff --cached -- $(OUT)
	bash -c '[[ 0 -eq $$(git status --porcelain $(OUT) | wc -l) ]]'
