{ }:
with import <nixpkgs> { }; 
let
  inherit (haskellPackages_ghc763_profiling) cabal cabalInstall ghcMod vector text;

in cabal.mkDerivation (self: {
  pname = "SDL2";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ SDL2 vector text ];
  buildTools = [ cabalInstall ];
})
