# https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-create-ad-hoc-environments-for-nix-shell

{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc921" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps:
    with ps;
    [
      # If you want to use cached Haskell packages (instead of making cabal
      # download them dynamically), you can list them here (e.g., "mtl"). But
      # this only works if the nixpkgs cache has them built; it may be that
      # nixpkgs does not have them yet (at least at the correct versions) which
      # would mean that we would have to download and build these packages
      # ourselves.

      #http-client
      #servant
      #servant-client
      #text
    ]);
in pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [
    ghc
    pkgs.zlib
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
