# https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-create-ad-hoc-environments-for-nix-shell

{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc921" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps:
    with ps;
    [
      # If you want to use cached Haskell packages (instead of making cabal
      # download them dynamically), you can list them here (e.g., "mtl").
    ]);
in pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [
    ghc
    # pkgs.gmp
    # pkgs.libffi
    # pkgs.pkgconfig
    # pkgs.xlibsWrapper
    # pkgs.xorg.libXinerama
    # pkgs.xorg.libXScrnSaver
    # pkgs.xorg.libXrandr
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
