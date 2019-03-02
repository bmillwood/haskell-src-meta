let
  # TODO: arg w/ default or whatever is idiomatic nix
  pkgs = import <nixpkgs> { };
  inherit (pkgs.haskell.lib) addBuildTools;
  inherit (import ./release.nix) package;
in
  (addBuildTools package
  # TODO: not pull lib versions of build tools if possible
  ( with pkgs;
    with pkgs.haskellPackages;
    [cabal-install ghcid stylish-haskell shellcheck colordiff]
  )).env
