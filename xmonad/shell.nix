{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, containers, random, safe, stdenv
      , unix, X11, xmonad, xmonad-contrib
      }:
      mkDerivation {
        pname = "xmonad-linus";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base containers random safe unix X11 xmonad xmonad-contrib
        ];
        license = stdenv.lib.licenses.bsd2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
