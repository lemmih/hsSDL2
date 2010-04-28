AC_INIT([Haskell SDL package], [0.5.4], [lemmih@gmail.com], [SDL])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([includes/HsSDLConfig.h.in])

# Header file to place defines in
AC_CONFIG_HEADERS([includes/HsSDLConfig.h])

AC_ARG_WITH([pthread],
            AS_HELP_STRING([--with-pthread], [doesn't strip -lpthread from the libraries]),
                  [pthread=$withval],
                        [pthread=no])

AC_PATH_PROGS([SDL_CONFIG], [sdl-config sdl11-config], [none])

if test "x$SDL_CONFIG" = "xnone"; then
   
   AC_MSG_ERROR([*** SDL not found! Get SDL from www.libsdl.org.
If you already installed it, check it's in the path. If problem remains,
please send a mail to the address that appears in ./configure --version
indicating your platform, the version of configure script and the problem.])

fi

if test "x$pthread" = "xno"; then
  SDL_LIBS=`$SDL_CONFIG --libs | sed "s/-lpthread//"`
else
  SDL_LIBS=`$SDL_CONFIG --libs`
fi

SDL_CFLAGS=`$SDL_CONFIG --cflags`

if test x`uname` = "xDarwin"; then
	SDL_LIBS="`$SDL_CONFIG --prefix`/lib/libSDLmain.a `$SDL_CONFIG --prefix`/lib/libSDL.dylib -Wl,-framework,Cocoa"
fi

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

AC_CONFIG_FILES([config.mk SDL.buildinfo])
AC_OUTPUT
