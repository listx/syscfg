# See https://nixos.org/manual/nixpkgs/stable/#chap-overlays for an overview of overlays.

self: super:

{
  # Taken from
  # http://lists.science.uu.nl/pipermail/nix-dev/2015-January/015601.html. We
  # add in some custom Haskell packages.
  haskellPackages = super.haskellPackages.override {
    overrides = self: super: {
      # Local packages not found on Hackage, but which still exist on the
      # local machine. As long as they have a .cabal file, we can use
      # `cabal2nix` to generate a Nix expression to build them.
      #
      # To generate the local package's nix expression, simply invoke
      #
      #   cabal2nix path/to/folder/containing/cabal/file > project-name.nix
      #
      # . E.g., like this:
      #
      #   cabal2nix ~/prog/auca > auca.nix
      #
      # . The `~/prog/auca` folder contains the `auca.cabal` file, which
      # will be looked up automatically by cabal2nix. Then, it's simply a
      # matter of moving the generated file to a folder inside ~/.nixpkgs,
      # and calling that file with `callPackage`.
      #
      # As for the local package itself, you can do
      #
      #   cabal2nix --shell path/to/folder > shell.nix
      #
      # to create a nix-shell environment. You can just do `nix-shell` after
      # that to get ghci. For cabal2nix to work properly, you need to have
      # the `cabal` binary available. To do this, you should have
      # `haskellPackages.cabal-install` installed either on your system, or
      # through nix-env.
      auca = self.callPackage ./haskell/auca.nix { };
    };
  };
}
