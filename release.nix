{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs.haskellPackages) callPackage;
in {
  haskell-src-meta = callPackage ./haskell-src-meta { } ;
}
