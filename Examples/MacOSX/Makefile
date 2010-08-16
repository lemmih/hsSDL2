# build with SDL C wrapper for mac osx (see mainc.c)
main: mainc.o MainRenamed.hs Main.hs
	ghc -no-hs-main --make mainc.o MainRenamed.hs -o $@

mainc.o: mainc.c MainRenamed_stub.h
	ghc -no-hs-main `sdl-config --cflags` -Wall $*.c -c

MainRenamed_stub.h: MainRenamed.hs
	ghc -no-hs-main --make $< -c

clean:
	rm -f *.hi *.o *_stub.c *_stub.h main

.PHONY: clean
