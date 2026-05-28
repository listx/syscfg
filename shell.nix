let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  git-hooks = import sources."git-hooks.nix";

  # 3. Configure the pre-commit checks
  pre-commit-check = git-hooks.run {
    src = ./.;
    hooks = {
      shellcheck = {
        enable = true;
        excludes = ["\\.(zsh|zshrc|zprofile|zshenv)$" "zsh/*"];
      };
      editorconfig-checker.enable = true;
    };
    package = pkgs.prek;
  };
in
pkgs.mkShell {
  name = "syscfg-dev-shell";

  buildInputs = with pkgs; [
    editorconfig-checker
    pikchr
    shellcheck
    nixfmt
    niv
    ormolu
    parinfer-rust
    shfmt
    vale
    pkgs.prek
  ] ++ pre-commit-check.enabledPackages;

  shellHook = ''
    echo -e "\n$(tput setaf 4)"
    echo Syscfg | ${pkgs.figlet}/bin/figlet
    echo -e "$(tput sgr0)\n"
    echo "Using nixpkgs commit: ${sources.nixpkgs.rev} ${pkgs.lib.version}"
    echo
    ${pre-commit-check.shellHook}
  '';
}
