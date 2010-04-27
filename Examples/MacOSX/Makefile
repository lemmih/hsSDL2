test: wrapper.o SDLWrapper.hs
	ghc -no-hs-main --make wrapper.o SDLWrapper.hs -o test
SDLWrapper_stub.h: SDLWrapper.hs
	ghc -no-hs-main --make SDLWrapper.hs -c
wrapper.o: SDLWrapper_stub.h
	ghc -no-hs-main `sdl-config --cflags` -Wall wrapper.c -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h test
.PHONY: clean

