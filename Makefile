TEMPORARY_DIR=_obuild
AUTO_GEN_DIR=src/auto_gen
ERROR_GEN_EXE=./$(TEMPORARY_DIR)/error_gen/error_gen.byte

all: main

generator:
	ocp-build build error_gen

launch_gen: generator
	$(ERROR_GEN_EXE) -d data/error/ -o $(AUTO_GEN_DIR)/

main: launch_gen
	ocp-build build main

test: launch_gen
	ocp-build build test
	./$(TEMPORARY_DIR)/test/test.byte

clean:
	ocp-build clean
	rm $(AUTO_GEN_DIR)/*.ml*
