TEMPORARY_DIR=_obuild
AUTO_GEN_DIR=src/auto_gen
ERROR_GEN_EXE=./$(TEMPORARY_DIR)/error_gen/error_gen.byte

all: main

generator:
	ocp-build build error_gen

launch_gen: generator
	$(ERROR_GEN_EXE) -c data/rewriting_system_error.conf -o src/auto_gen/rewriting_system_error.ml
	$(ERROR_GEN_EXE) -c data/term_system_error.conf -o src/auto_gen/term_system_error.ml

main: launch_gen
	ocp-build build main

test: launch_gen
	ocp-build build test
	./$(TEMPORARY_DIR)/test/test.byte

clean:
	rm -r $(TEMPORARY_DIR)
	rm $(AUTO_GEN_DIR)/*.ml