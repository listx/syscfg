# https://stackoverflow.com/a/56180220/437583

with import (builtins.fetchGit {
    # Unfortunately, we can't check out arbitrary GitHub commit SHAs, so we
    # have to refer to a local clone of
    # https://github.com/NixOS/nixpkgs-channels (which itself just follows
    # https://github.com/NixOS/nixpkgs.git with a verification delay, as per
    # https://nixos.wiki/wiki/Nix_channels).
    url = "~/prog/foreign/nixpkgs";
    # Known good commit for moving branch "nixpkgs-unstable".
    ref = "104f8a0e1a17a894b320f86add409d9aebb36fe4";
}) {};

haskell.lib.buildStackProject {
    name = "my-project";
    buildInputs = [
      gmp
      haskell.compiler.ghc865
      libffi
      pkgconfig
      x11
      xorg.libXinerama
      xorg.libXScrnSaver
      xorg.libXrandr ]; }
