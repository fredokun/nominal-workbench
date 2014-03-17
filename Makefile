TEMPORARY_DIR=_obuild
AUTO_GEN_DIR=src/auto_gen
ERROR_GEN_EXE=./$(TEMPORARY_DIR)/error_gen/error_gen.byte
NOWORK_BIN_TMP=$(TEMPORARY_DIR)/nowork/nowork.byte
NOWORK_BIN=nowork

all: main

generator:
	ocp-build build error_gen

launch_gen: generator
	$(ERROR_GEN_EXE) -d data/error/ -o $(AUTO_GEN_DIR)/

main: launch_gen
	ocp-build build nowork
	cp $(NOWORK_BIN_TMP) $(NOWORK_BIN)

test: main
	./$(NOWORK_BIN) --no-repl --debug data/test/test.nw

install:
	ocp-build install nowork

uninstall:
	ocp-build -uninstall nowork 

clean:
	ocp-build clean
	rm -f $(AUTO_GEN_DIR)/*.ml*
