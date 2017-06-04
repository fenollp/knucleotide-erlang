SLOW_OPTS = 25000000  # official shootout setting

knucleotide-input25000000.txt: fasta
	./$^ $(SLOW_OPTS) | tr -d '\r' > $@

fasta: fasta.c
	$(CC) -std=gnu99 -O3 -fomit-frame-pointer $< -o $@
