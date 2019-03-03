{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs.haskell.lib) addBuildTools;
  inherit (import ./release.nix { inherit pkgs; }) haskell-src-meta;
in
  (addBuildTools haskell-src-meta
  ( with pkgs;
    with pkgs.haskellPackages;
    [cabal-install ghcid stylish-haskell shellcheck colordiff]
  )).env
