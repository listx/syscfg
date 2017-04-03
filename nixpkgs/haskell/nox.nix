{ mkDerivation, base, cmdargs, parsec, stdenv, text }:
mkDerivation {
  pname = "nox";
  version = "0.0.1";
  src = /home/l/prog/nox;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cmdargs parsec text ];
  license = "unknown";
}
