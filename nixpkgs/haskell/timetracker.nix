{ mkDerivation, array, base, cmdargs, old-locale, parsec
, QuickCheck, regex-pcre, stdenv, time
}:
mkDerivation {
  pname = "timetracker";
  version = "1.0.1";
  src = /home/l/prog/timetracker;
  buildDepends = [
    array base cmdargs old-locale parsec QuickCheck regex-pcre time
  ];
  license = stdenv.lib.licenses.mit;
}
