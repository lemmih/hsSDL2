AC_INIT([Haskell SDL package], [0.2.0], [lemmih@gmail.com], [SDL-mixer])

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

AC_CHECK_LIB([SDL_mixer],
             [Mix_OpenAudio],
                  [if test -n "$LDPREFIX" -a -r `$SDL_CONFIG --prefix`/lib/libSDL_mixer.la
then SDL_LIBS=`$SDL_CONFIG --prefix`/lib/libSDL_mixer.la
else SDL_LIBS=-lSDL_mixer
fi],
             [AC_MSG_ERROR([*** SDL_mixer lib not found! Get SDL_mixer from
http://www.libsdl.org/projects/SDL_mixer/index.html])])
SDL_CFLAGS=`$SDL_CONFIG --cflags`

AC_SUBST([SDL_CFLAGS])
AC_SUBST([SDL_LIBS])

AC_CONFIG_FILES([config.mk SDL-mixer.buildinfo])
AC_OUTPUT
