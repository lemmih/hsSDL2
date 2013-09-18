AC_INIT([Haskell SDL package], [0.5.2], [lemmih@gmail.com], [SDL2-image])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([includes/HsSDLConfig.h.in])

# Header file to place defines in
AC_CONFIG_HEADERS([includes/HsSDLConfig.h])

AC_PATH_PROGS([SDL_CONFIG], [sdl2-config sdl20-config], [none])

if test "x$SDL_CONFIG" = "xnone"; then
   
   AC_MSG_ERROR([*** SDL not found! Get SDL from www.libsdl.org.
If you already installed it, check it's in the path. If problem remains,
please send a mail to the address that appears in ./configure --version
indicating your platform, the version of configure script and the problem.])

fi

SDL_LIBS="-L`$SDL_CONFIG --prefix`/lib -lSDL2_image"

LDFLAGS=$SDL_LIBS
AC_CHECK_LIB([SDL2_image],
             [IMG_Load],
             ,
             [AC_MSG_ERROR([*** SDL2_image lib not found! Get SDL2_image from
http://www.libsdl.org/projects/SDL_image/index.html])])
LDFLAGS=

if test x`uname` = "xDarwin"; then
   #SDL_LIBS="`$SDL_CONFIG --prefix`/lib/libSDLmain.a `$SDL_CONFIG --prefix`/lib/libSDL.dylib -Wl,-framework,Cocoa"
  SDL_LIBS="$SDL_LIBS `$SDL_CONFIG --prefix`/lib/libSDL2_image.dylib -Wl,-framework,Cocoa"
fi

SDL_CFLAGS=`$SDL_CONFIG --cflags`

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

AC_CONFIG_FILES([config.mk SDL-image.buildinfo])
AC_OUTPUT
