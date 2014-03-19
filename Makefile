TEMPORARY_DIR=_obuild
AUTO_GEN_DIR=src/auto_gen
ERROR_GEN_EXE=./$(TEMPORARY_DIR)/error_gen/error_gen.byte
NOWORK_BIN_TMP=$(TEMPORARY_DIR)/nowork/nowork.byte
NOWORK_BIN=nowork

DOC_FILES=coding-style.tex dev-manual.tex user-manual.tex methodology.tex
DOC_PDF=$(DOC_FILES:.tex=.pdf)
PDF=$(addprefix doc/pdf/, $(DOC_PDF))

all: main

generator:
	ocp-build build error_gen

launch_gen: generator
	$(ERROR_GEN_EXE) -d data/error/ -o $(AUTO_GEN_DIR)/

main: launch_gen
	ocp-build build nowork
	cp $(NOWORK_BIN_TMP) $(NOWORK_BIN)

test: main
	./$(NOWORK_BIN) --no-repl --no-warning --debug data/test/test.nw

install:
	ocp-build install nowork

uninstall:
	ocp-build -uninstall nowork 

doc/pdf/%.pdf: doc/%.tex
	pdflatex -interaction=batchmode -output-directory doc/pdf $<

doc/client/%.pdf: doc/client/%.tex
	pdflatex -interaction=batchmode -output-directory doc/pdf $<

doc_dirs:
	rm -fr doc/pdf doc/reference
	mkdir doc/pdf doc/reference

ocamldoc:
	ocamldoc -html -d doc/reference/ -I _obuild/ -t NoWork src/*.mli src/algorithms/*.mli src/generator/*.mli src/interactive/*.mli src/system/*.mli src/parser/*.mli

doc: doc_dirs $(PDF) ocamldoc
	make clean_doc

clean: clean_doc
	ocp-build clean
	rm -f $(AUTO_GEN_DIR)/*.ml*
	rm -fr doc/pdf doc/reference

clean_doc:
	rm -f doc/pdf/*.aux doc/pdf/*.fdb_latexmk doc/pdf/*.log doc/pdf/*.gz doc/pdf/*.out
