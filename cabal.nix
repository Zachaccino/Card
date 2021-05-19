{ mkDerivation, base, containers, hello, hpack, stdenv }:
mkDerivation {
  pname = "card";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  libraryToolDepends = [ hello hpack ];
  executableHaskellDepends = [ base containers ];
  executableToolDepends = [ hello ];
  testHaskellDepends = [ base containers ];
  testToolDepends = [ hello ];
  prePatch = "hpack";
  homepage = "https://github.com/MatrixAI/Haskell-Demo#readme";
  license = stdenv.lib.licenses.asl20;
}
