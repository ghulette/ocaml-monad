CC=ocamlc
CFLAGS=-c

all: test

test: monad.cmo option_monad.cmo test.cmo
	$(CC) monad.cmo option_monad.cmo test.cmo -o test

monad.cmo: monad.ml
	$(CC) $(CFLAGS) monad.ml

option_monad.cmo: option_monad.ml
	$(CC) $(CFLAGS) option_monad.ml

test.cmo: test.ml
	$(CC) $(CFLAGS) test.ml

clean:
	rm -rf *.cmo *.cmi test
