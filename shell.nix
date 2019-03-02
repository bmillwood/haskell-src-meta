{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs.haskell.lib) addBuildTools;
  inherit (import ./release.nix { inherit pkgs; }) package;
in
  (addBuildTools package
  ( with pkgs;
    with pkgs.haskellPackages;
    [cabal-install ghcid stylish-haskell shellcheck colordiff]
  )).env
