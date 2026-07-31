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
        settings.offline = false; #
      };
    };
    package = pkgs.prek;
  };
  lilacSrc = builtins.fetchGit {
    url = "https://git.sr.ht/~listx/lilac";
    ref = "refs/tags/v0.1.0.8";
    name = "lilac";
    submodules = true;
  };

  lilacPkg = import "${lilacSrc}/nix/release.nix" {
    lilacGitVersion = "v0.1.0.8";
  };
in
pkgs.mkShell {
  name = "syscfg-dev-shell";

  buildInputs = with pkgs; [
    lilacPkg
    editorconfig-checker
    pikchr
    shellcheck
    nixfmt
    niv
    ormolu
    parinfer-rust
    shfmt
    vale
    prek

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
    echo "Lilac version: $(lilac --version)"
    echo "Rust compiler profile: $(rustc --version)"
    echo
    ${pre-commit-check.shellHook}
  '';
}
