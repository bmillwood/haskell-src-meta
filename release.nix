let
  pkgs = import <nixpkgs> { };
  inherit (pkgs.haskellPackages) cabal-install ghcid;
  inherit (builtins) concatLists;
  inherit (pkgs.haskell.lib) addBuildTool overrideCabal failOnAllWarnings;
  withCabalInstall = drv: addBuildTool drv cabal-install;
  withGhcid = drv: addBuildTool drv ghcid;
in
  { package =
      withGhcid (withCabalInstall
        ( pkgs.haskellPackages.callPackage ./haskell-src-meta.nix { } ));
  }
