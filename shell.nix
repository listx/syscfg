let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
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
  ];

  shellHook = ''
    echo -e "\n$(tput setaf 4)"
    echo Syscfg | ${pkgs.figlet}/bin/figlet
    echo -e "$(tput sgr0)\n"
    echo "Using nixpkgs commit: ${sources.nixpkgs.rev} ${pkgs.lib.version}"
    echo
  '';
}
