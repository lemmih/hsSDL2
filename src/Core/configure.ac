AC_INIT([Haskell SDL package], [0.1.0], [lemmih@gmail.com], [SDL])

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
SDL_BUILD_PACKAGE=yes
BUILD_PACKAGE_BOOL=True
AC_SUBST([SDL_BUILD_PACKAGE])
AC_SUBST([BUILD_PACKAGE_BOOL])

min_sdl_version=1.2.8
AC_MSG_CHECKING(for SDL - version >= $min_sdl_version)

if test "x$pthread" = "xno"; then
  SDL_LIBS=`$SDL_CONFIG --libs | sed "s/-lpthread//"`
else
  SDL_LIBS=`$SDL_CONFIG --libs`
fi

SDL_CFLAGS=`$SDL_CONFIG --cflags`

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

sdl_major_version=`$SDL_CONFIG --version | \
    sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
sdl_minor_version=`$SDL_CONFIG --version | \
    sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
sdl_micro_version=`$SDL_CONFIG --version | \
    sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`


AC_CONFIG_FILES([config.mk SDL.buildinfo])
AC_OUTPUT
