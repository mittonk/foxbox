all: sokoban.gb

sokoban.gb: sokoban.o
	rgblink -o sokoban.gb sokoban.o
	rgbfix -v -p 0xFF sokoban.gb

sokoban.o: sokoban.asm
	rgbasm -o sokoban.o sokoban.asm
