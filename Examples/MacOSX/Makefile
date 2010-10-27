# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=main
$(PROGNAME): mainc.o MainWrapper.hs Main.hs
	ghc -no-hs-main --make mainc.o MainWrapper.hs -o $@
mainc.o: mainc.c MainWrapper_stub.h
	ghc -no-hs-main `sdl-config --cflags` -Wall $*.c -c
MainWrapper_stub.h: MainWrapper.hs
	ghc -no-hs-main --make $< -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
#
