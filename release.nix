{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs.haskellPackages) callPackage;
in {
  package = callPackage ./haskell-src-meta.nix { } ;
}
