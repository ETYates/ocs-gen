CC=dune
OUTBIN=./src/main.exe
TEST_TXT=./tests/lunt.txt

.PHONY: clean

all:
	$(CC) build $(OUTBIN)

run:
	$(CC) exec $(OUTBIN)

test:
	$(CC) exec -- $(OUTBIN) -t $(TEST_TXT)

clean:
	rm -rf _build/
