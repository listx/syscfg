let
  pkgs = import ./pkgs.nix;
  hsPkgs = pkgs.hsPkgs;
  ## To configure ormolu, I often end up creating a wrapped version in which I pass it the desired flags.
  ## To use it, remove the ormolu line from the `tools` section, uncomment the lines below, and add `ormolu-wrapped` to  `shellFor`'s `buildInputs`.
  # ormolu-wrapped =
  #   let ormolu = pkgs.haskell-nix.tool hsPkgs.projectModule.compiler-nix-name "ormolu" "latest";
  #   in
  #   pkgs.writeShellScriptBin "ormolu" ''
  #     ${ormolu}/bin/ormolu --ghc-opt=-XImportQualifiedPost $@
  #   '';
in
hsPkgs.shellFor {

  # Disable hoogle for now because it doesn't built with ghc921 yet. See
  # https://github.com/input-output-hk/haskell.nix/issues/1292#issuecomment-969440788.
  withHoogle = false;
  tools = {
    cabal = "latest";
    # Disable some tools that lack ghc921 support.
    # ghcid = "latest";
    # haskell-language-server = "latest";
    # ormolu = "latest";
    # hlint = "latest";
  };
  # buildInputs = [ ormolu-wrapped ]; # See note about ormolu-wrapped above
  exactDeps = true;
}
