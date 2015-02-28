SOURCES = functor.mli functor.ml \
          monad.mli monad.ml
RESULT  = monadlib

all: byte-code-library native-code-library

include OCamlMakefile
