AC_INIT([Haskell SDL package], [0.4.0], [lemmih@gmail.com], [SDL-gfx])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([includes/HsSDLConfig.h.in])

# Header file to place defines in
AC_CONFIG_HEADERS([includes/HsSDLConfig.h])

AC_PATH_PROGS([SDL_CONFIG], [sdl-config sdl11-config], [none])

if test "x$SDL_CONFIG" = "xnone"; then
   
   AC_MSG_ERROR([*** SDL not found! Get SDL from www.libsdl.org.
If you already installed it, check it's in the path. If problem remains,
please send a mail to the address that appears in ./configure --version
indicating your platform, the version of configure script and the problem.])

fi

AC_CHECK_LIB([SDL_gfx],
             [SDL_initFramerate],
                  [if test -n "$LDPREFIX" -a -r `$SDL_CONFIG --prefix`/lib/libSDL_gfx.la
then SDL_LIBS=`$SDL_CONFIG --prefix`/lib/libSDL_gfx.la
else SDL_LIBS=-lSDL_gfx
fi],
             [AC_MSG_ERROR([*** SDL_gfx lib not found! Get SDL_gfx from
http://www.ferzkopp.net/~aschiffler/Software/SDL_gfx-2.0/])])
SDL_CFLAGS=`$SDL_CONFIG --cflags`

AC_CHECK_HEADER([SDL_framerate.h SDL_gfxPrimitives.h],
                [],
                        [AC_MSG_ERROR([*** SDL_gfx include files not found!
You should install the development package.])])

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

AC_CONFIG_FILES([config.mk SDL-gfx.buildinfo])
AC_OUTPUT
