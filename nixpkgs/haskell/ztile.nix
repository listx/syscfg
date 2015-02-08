{ mkDerivation, base, containers, fgl, mwc-random, primitive
, QuickCheck, stdenv, tasty, tasty-quickcheck, utility-ht, vector
}:
mkDerivation {
  pname = "ztile";
  version = "0.1.0.1";
  src = /home/l/prog/ztile;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base containers fgl mwc-random primitive QuickCheck tasty
    tasty-quickcheck utility-ht vector
  ];
  description = "Square and Hex tiles for games";
  license = stdenv.lib.licenses.bsd2;
}
