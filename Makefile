all: sokoban.gb

sokoban.gb: sokoban.o sprobjs_lib.o
	rgblink -n sokoban.sym -o $@ $^
	rgbfix -v -p 0xFF $@

%.o: %.asm
	rgbasm -o $@ $<

sokoban.o: assets/player_east.2bpp assets/player_west.2bpp

%.2bpp: %.png
	rgbgfx -Z -o $@ $<
