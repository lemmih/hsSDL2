AC_INIT([Haskell SDL package], [0.1.0], [lemmih@gmail.com], [SDL])

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

AC_CHECK_LIB([SDL_image],
             [IMG_Load],
                  [if test -n "$LDPREFIX" -a -r `$SDL_CONFIG --prefix`/lib/libSDL_image.la
then SDL_LIBS=`$SDL_CONFIG --prefix`/lib/libSDL_image.la
else SDL_LIBS=-lSDL_image
fi],
             [AC_MSG_ERROR([*** SDL_image lib not found! Get SDL_image from
http://www.libsdl.org/projects/SDL_image/index.html])])

SDL_CFLAGS=`$SDL_CONFIG --cflags`

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

AC_CONFIG_FILES([config.mk SDL-image.buildinfo])
AC_OUTPUT
