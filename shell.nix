let
  sources = import ./nix/sources.nix;
  rust-overlay = import sources.rust-overlay;
  pkgs = import sources.nixpkgs {
    overlays = [ rust-overlay ];
  };
  rustToolchain = pkgs.rust-bin.stable.latest.default.override {
    extensions = [ "rust-src" "rust-analyzer" "clippy" ];
  };
  git-hooks = import sources."git-hooks.nix";

  # 3. Configure the pre-commit checks
  pre-commit-check = git-hooks.run {
    src = ./.;
    hooks = {
      shellcheck = {
        enable = true;
        excludes = [
          "script/aex.sh"
          "nixos/w0/syschdemd.sh"
          "\\.(zsh|zshrc|zprofile|zshenv)$"
          "zsh/*"
        ];
      };
      editorconfig-checker.enable = true;
      clippy = {
        enable = true;
        package = rustToolchain;
      };
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

    rustToolchain
  ] ++ pre-commit-check.enabledPackages;

  nativeBuildInputs = with pkgs; [
    pkg-config
    gcc
  ];
  propagatedBuildInputs = with pkgs; [
    openssl
    zlib
  ];

  shellHook = ''
    echo -e "\n$(tput setaf 4)"
    echo Syscfg | ${pkgs.figlet}/bin/figlet
    echo -e "$(tput sgr0)\n"
    echo "Using nixpkgs commit: ${sources.nixpkgs.rev} ${pkgs.lib.version}"
    echo "Rust compiler profile: $(rustc --version)"
    echo
    ${pre-commit-check.shellHook}
  '';
}
