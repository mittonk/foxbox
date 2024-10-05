ASSETS = assets/player_east.2bpp \
         assets/player_east2.2bpp \
         assets/player_north.2bpp \
         assets/player_south.2bpp

all: sokoban.gb

sokoban.gb: sokoban.o sprobjs_lib.o
	rgblink -n sokoban.sym -o $@ $^
	rgbfix -v -p 0xFF $@

%.o: %.asm
	rgbasm -o $@ $<

sokoban.o: $(ASSETS)

%.2bpp: %.png
	rgbgfx -Z -o $@ $<
