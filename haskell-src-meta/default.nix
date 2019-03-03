{ mkDerivation, base, containers, haskell-src-exts, HUnit, pretty
, stdenv, syb, template-haskell, test-framework
, test-framework-hunit, th-orphans
}:
mkDerivation {
  pname = "haskell-src-meta";
  version = "0.8.2";
  src = ./.;
  libraryHaskellDepends = [
    base haskell-src-exts pretty syb template-haskell th-orphans
  ];
  testHaskellDepends = [
    base containers haskell-src-exts HUnit pretty syb template-haskell
    test-framework test-framework-hunit
  ];
  description = "Parse source to template-haskell abstract syntax";
  license = stdenv.lib.licenses.bsd3;
}
