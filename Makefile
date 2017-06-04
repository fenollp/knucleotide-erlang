ERLC = erlc
ERL = erl -smp enable -noshell

IN = priv/knucleotide-input.txt
OUT = priv/knucleotide-output.txt

all: original

original: src/knucleotide.erl
	$(ERLC) +native +'{hipe, [o3]}' $^

test: original
	$(ERL) -run knucleotide main 0 < $(IN) > $(OUT)
	git --no-pager diff --cached -- $(OUT)
	bash -c '[[ 0 -eq $$(git status --porcelain $(OUT) | wc -l) ]]'
