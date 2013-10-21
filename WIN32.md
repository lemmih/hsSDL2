Building hsSDL on Win32
-----------------------
 * Bit Connor <bit@mutantlemon.com>
 * Trevor Elliott <https://github.com/elliottt>


This is how I managed to get hsSDL working on Windows XP.
I used GHC version 7.6.1


  1. Download the SDL mingw development package from the SDL website
     [http://www.libsdl.org](http://www.libsdl.org).
     The file I used was `SDL2-devel-2.0.0-mingw.tar.gz`.

  2. Extract it somewhere. You will get a directory called `SDL2-2.0.0`.
     I used `C:\SDL2-2.0.0`.

  3. Modify SDL.cabal file from hsSDL distribution.
     * There is a line:

       ```
       Build-type: Custom
       ```

       Change it to:

       ```
       Build-type: Simple
       ```
     * If you're building with an x86_64 version of ghc, add these lines to the end of the library stanza:

       ```
       Include-Dirs:   C:\SDL2-2.0.0\x86_64-w64-mingw32\include\SDL2
       Extra-Lib-Dirs: C:\SDL2-2.0.0\x86_64-w64-mingw32\lib
       Extra-Lib-Dirs: C:\SDL2-2.0.0\x86_64-w64-mingw32\bin
       ```
       
       Otherwise, add these three lines:
       
       ```
       Include-Dirs:   C:\SDL2-2.0.0\i686-w64-mingw32\include\SDL2
       Extra-Lib-Dirs: C:\SDL2-2.0.0\i686-w64-mingw32\lib
       Extra-Lib-Dirs: C:\SDL2-2.0.0\i686-w64-mingw32\bin
       ```

  4. Open a Windows Command Prompt (Start -> Run -> "cmd.exe")
     cd into the hsSDL distribution directory and run:

     ```sh
     cabal install
     ```

  5. Compile the example program. Run:

     ```sh
     cd Examples
     ghc --make Test.hs
     ```

     You should get a Test.exe file.
     Before running it, copy the SDL.dll file into the directory. You can find
     it here:

     ```
     C:\SDL-1.2.12\bin\SDL.dll
     ```

     Now run Test.exe, press spacebar a few times to watch the smiley face jump
     around, and finally press Q to quit.

  6. Using SDL from GHCi requires a trick. If you try running Test.hs you will
     get this error:

     ```
     > ghci Test.hs
     Prelude Main> main
     Loading package SDL-0.4.0 ... can't load .so/.DLL for: SDLmain (addDLL: unknown
     error)
     ```

     To get ghci working, you must make 2 copies of SDL.dll called SDLmain.dll,
     and SDL.dll.dll:

     ```sh
     copy SDL.dll SDLmain.dll
     copy SDL.dll SDL.dll.dll
     ```

     Now everything should work!

Peace,
Bit Connor <bit@mutantlemon.com>
