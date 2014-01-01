TEMPORARY_DIR=_obuild
AUTO_GEN_DIR=src/auto_gen

all: main

generator:
	ocp-build build error_gen

launch_gen: generator
	./$(TEMPORARY_DIR)/error_gen/error_gen.byte

main: launch_gen
	ocp-build build main

test: launch_gen
	ocp-build build test
	./$(TEMPORARY_DIR)/test/test.byte

clean:
	rm -r $(TEMPORARY_DIR)
	rm $(AUTO_GEN_DIR)/*.ml