{ mkDerivation, base, cmdargs, directory, hinotify, monads-tf
, process, stdenv, lib, stm, time
}:
mkDerivation {
  pname = "auca";
  version = "0.0.1.5";
  src = /home/l/prog/auca;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base cmdargs directory hinotify monads-tf process stm time
  ];
  description = "Execute arbitrary command(s) based on file changes";
  license = lib.licenses.bsd2;
}
