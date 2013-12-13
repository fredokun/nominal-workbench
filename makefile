TEMPORARY_DIR=_obuild
AUTO_GEN_DIR=src/auto_gen

all: main

generator:
	ocp-build error_gen

launch_gen: generator
	./_obuild/error_gen/error_gen.byte

main: launch_gen
	ocp-build main

clean:
	rm -r $(TEMPORARY_DIR)
	rm $(AUTO_GEN_DIR)/*.ml