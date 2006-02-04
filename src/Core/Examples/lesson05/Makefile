
HC = ghc-6.3

# for all, set the targets to be every lesson1.c-lesson13.c 
# file, removing the .c extension.  That is, at this point,
# it would produce lesson1, lesson2, lesson3,...,lesson13 targets.
#
all: $(basename $(wildcard lesson[1-9].hs))

# same as for all, except set the targets to be 
# lessonX.tar.gz from lessonX.c.  This is really
# only used to build smaller tutorial .tar.gz files
# to send to nehe.
#
dist: $(foreach file,$(basename $(wildcard lesson[1-9].hs lesson1[0-3].hs)),$(file).tar.gz)

# to produce, say, lesson1:
#
# 1. compile the thing.  uses the variables defined above.
# 
lesson% : lesson%.hs
	$(HC) --make $@.hs -o $@

# to clean up:
# delete all of the lessonX files.
clean:
	rm $(wildcard lesson[1-9])
	rm $(wildcard lesson[1-9].o)
	rm $(wildcard lesson[1-9].hi)

