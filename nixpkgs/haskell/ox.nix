{ mkDerivation, array, base, cmdargs, old-locale, parsec
, QuickCheck, regex-pcre, stdenv, time
}:
mkDerivation {
  pname = "ox";
  version = "1.0.1";
  src = /home/l/prog/ox;
  buildDepends = [
    array base cmdargs old-locale parsec QuickCheck regex-pcre time
  ];
  license = stdenv.lib.licenses.mit;
}
