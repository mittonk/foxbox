ASSETS = assets/player_east.2bpp \
         assets/player_east2.2bpp \
         assets/player_north.2bpp \
         assets/player_south.2bpp \
			assets/title.2bpp

all: foxbox.gb

foxbox.gb: foxbox.o sprobjs_lib.o
	rgblink -n foxbox.sym -o $@ $^
	rgbfix -v -p 0xFF $@

%.o: %.asm
	rgbasm -o $@ $<

foxbox.o: $(ASSETS)

%.2bpp: %.png
	rgbgfx -Z -o $@ $<

clean:
	rm -f *.gb *.o *.sym
	rm -f assets/*.2bpp
