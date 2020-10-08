{ mkDerivation, base, colour, criterion, dhall, either, hpack
, megaparsec, optparse-generic_1_4_3, stdenv, tasty, tasty-hspec, text
}:
mkDerivation {
  pname = "inky";
  version = "0.0.0";
  src = /home/l/prog/inky;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base colour dhall either megaparsec optparse-generic_1_4_3 text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hspec ];
  benchmarkHaskellDepends = [ base criterion ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/inky#readme";
  license = stdenv.lib.licenses.mit;
}
