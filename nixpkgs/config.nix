super:
{
  # Nix evaluates this file and will call the `packageOverrides` attribute as a
  # *function* and given the original Nixpkgs list of packages as a parameter
  # (actually, the attribute within the passed-in-parameter, called `pkgs`,
  # contains the actual packages).

  # For an excellent reference on all keywords/syntax on the Nix language,
  # consult the paper "NixOS: A Purely Functional Linux Distribution" (2008);
  # despite its name, it actually defines in great detail the Nix language
  # itself used to build NixOS.

  # Another great resource is
  # https://nixos.org/nixos/manual/sec-configuration-syntax.html, which has a
  # "Syntax Summary" table.

  # A more complete reference is
  # http://nixos.org/nix/manual/#chap-writing-nix-expressions.

  packageOverrides = super: let self = super.pkgs; in
  {
    # We import the packages from `pkgsets`. But `pkgsets` doesn't define things
    # on its own; rather, it picks out the official packages already defined in
    # Nixpkgs' `default.nix` which in turn pulls in
    # `pkgs/top-level/all-packages.nix`. So we have to pass into it the master
    # list of packages, which is `self`. Then from within that file, we can say
    # things like "vim" and Nix will know what "vim" means.
    pkgsets = import ./pkgsets.nix { super = super; };
    # `inherit` is a shorter syntax that basically does mass assignment of
    # attributes for us. Otherwise, we'd have to write, e.g.,
    #
    #   self.l_base_env = l_base_env;
    #   self.l_dev_env = l_dev_env;
    #
    # . Read more at http://nixos.org/nix/manual/#chap-writing-nix-expressions.

    inherit (self.pkgsets)
      l_set_home
      l_set_work
      l_set_base
      l_set_dev
      l_set_media
      l_set_web
      l_set_av
      l_set_office
      l_set_games
      l_set_misc
      l_set_haskell
      # Haskell packages are hidden by default. To search for, e.g., "auca"
      # defined from pkgsets.nix, do `nix-env -qaP -A
      # nixpkgs.haskellngPackages.auca`
      haskellngPackages;
  };
}
