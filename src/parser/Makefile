OCAMLCC=ocamlc
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

.PHONY : clean

all: main test

%.cmo: %.ml
	$(OCAMLCC) -c $<
%.cmi: %.mli
	$(OCAMLCC) -c $<

parser.cmo:parser.cmi parser.mly
lexer.cmo: parser.cmo lexer.ml

parser.ml: parser.mli parser.mly
parser.mli: parser.mly
	$(OCAMLYACC) $<

%.ml: %.mll
	$(OCAMLLEX) $<

# main: parser.cmo lexer.cmo test.cmo
# 	$(OCAMLCC) $^ -o main

main:
	ocp-build parser-test
	cp ../../_obuild/parser-test/parser-test.byte main

test: main
	@for f in `ls test*.grm`; do \
		echo Test \"$$f\"\:; \
		./main $$f; \
		echo ; \
	done

clean:
	rm -rf *.cmo *.cmi main parser.ml lexer.ml
