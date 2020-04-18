CC=dune
OUTBIN=./src/main.exe
TEST_TXT=./tests/lunt.txt

.PHONY: clean

all:
	$(CC) build $(OUTBIN)
	cp ./_build/default/src/main.exe .

run:
	$(CC) exec $(OUTBIN)

test:
	$(CC) exec -- $(OUTBIN) -t $(TEST_TXT)

clean:
	rm -rf _build/
