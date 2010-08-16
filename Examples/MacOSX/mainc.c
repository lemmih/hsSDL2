/*
A C wrapper to get proper SDL initialisation on mac. From MACOSX in the
SDL haskell package:

  SDL uses Objective-C and Cocoa to open a UI window; this means that
  Cocoa must be initialized, and in particular, an NSAutoReleasePool be in
  place.  This initialization is done in libSDLmain.  For
  C/C++/Objective-C programs, libSDLmain #defines the developer's main to
  be SDL_main and piggy backs SDL_main onto it's own Cocoa main, using the
  C preprocessor and abusing the linker.  Of course, this technique will
  never work for Haskell: there is no clean entry point to a Haskell
  runtime in this fashion.
  ...
  MainRenamed.hs imports Main and foreign-exports Main.main as
  haskell_main to be able to call it from C. mainc.c includes SDL.h and
  contains a main function to make the preprocessor magic of SDL
  happen. The C main function initializes the GHC RTS and calls
  haskell_main from MainRenamed.hs which is Main.main in disguise.  The
  Makefile then uses ghc --make to link our objects with the GHC RTS and SDL.

See also
http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi-ghc.html#using-own-main

*/

#include <SDL.h>
#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "MainRenamed_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_MainRenamed (void);
#endif

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_MainRenamed);
#endif
  haskell_main();
  hs_exit();
  return 0;
}
