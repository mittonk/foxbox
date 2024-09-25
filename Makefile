all: sokoban.gb

sokoban.gb: sokoban.o sprobjs_lib.o
	rgblink -n sokoban.sym -o $@ $^
	rgbfix -v -p 0xFF $@

%.o: %.asm
	rgbasm -o $@ $<
