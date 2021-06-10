# https://stackoverflow.com/a/56180220/437583

with import (builtins.fetchGit {
    url = "~/prog/foreign/nixpkgs";
    # Known good commit for moving branch "nixos-21.05".
    ref = "60cce7e5e1fdf62421ef6d4184ee399b46209366";
}) {};

haskell.lib.buildStackProject {
    name = "my-project";
    buildInputs = [
      gmp
      haskell.compiler.ghc8104
      libffi
      pkgconfig
      x11
      xorg.libXinerama
      xorg.libXScrnSaver
      xorg.libXrandr ]; }
