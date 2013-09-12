{ }:
with import <nixpkgs> { }; 
let
  inherit (haskellPackages) cabal cabalInstall ghcMod;
  SDL2 = callPackage <nixpkgs/pkgs/development/libraries/SDL/SDL2.nix> {};

in cabal.mkDerivation (self: {
  pname = "SDL2";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ SDL2 ];
  buildTools = [ cabalInstall vim_configurable git fish ghcMod less ];
})
