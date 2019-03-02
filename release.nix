let
  pkgs = import <nixpkgs> { };
  inherit (pkgs.haskellPackages) callPackage;
in {
  package = callPackage ./haskell-src-meta.nix { } ;
}
