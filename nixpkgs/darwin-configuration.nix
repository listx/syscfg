{ config, lib, pkgs, ... }:

let
  melby-release = import "${HOME}/prog/melby/package/build.nix";
  HOME = config.system.primaryUserHome;
  baseconfig = { allowUnfree = true; };
  unstable = import <nixpkgs-unstable> { config = baseconfig; };
in {
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "aspell-dict-en-science" ];

  # List packages installed in system profile.
  #
  # FIXME: Remove tooling that can be project-specific and placed into
  # "<PROJECT_ROOT>/.envrc".
  environment.systemPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    autoconf
    babashka
    bashInteractive
    bear
    ccls
    clang-tools
    cljfmt
    clj-kondo
    clojure
    clojure-lsp
    coreutils
    dhall
    difftastic
    dos2unix
    editorconfig-core-c
    # Emacs is broken on Sequoia 15.4. See https://github.com/NixOS/nixpkgs/issues/395169#issuecomment-2769619888.
    (emacs.override { withNativeCompilation = false; })
    fd
    ffmpeg
    findutils
    fzf
    gawk
    gcal
    git
    gnugrep
    gnumake
    gnupg
    gnused
    gnutar
    graphviz
    htop
    inkscape
    jq
    jujutsu
    less
    lieer
    melby-release.melby-daemon
    melby-release.melby-client-rust
    neil
    neovim
    nodejs
    notmuch
    openjdk
    pass
    pdf2svg
    nodePackages.prettier
    procps
    ripgrep
    rustfmt
    scc
    shellcheck
    shfmt
    source-code-pro
    source-sans
    source-serif
    source-han-sans
    source-han-serif
    source-han-mono
    stack
    texliveFull
    texlivePackages.raleway
    tig
    tmux
    tree
    util-linux
    wezterm
    xz
    zathura
    zsh
    zstd
  ];

  # Enable melbyd with launchd.
  launchd.user.agents.melbyd = {
    script = "${melby-release.melby-daemon}/bin/melbyd start";
    environment = {
      RELEASE_COOKIE = "${HOME}/.melby/cookie";
      LUA_PATH = "${HOME}/.melby/?.lua";
    };
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
    };
  };

  # Auto upgrade nix package and the daemon service.
  nix.package = pkgs.nix;

  # nix-direnv
  # See https://github.com/nix-community/nix-direnv.
  programs.direnv = {
    enable = true;
    package = pkgs.direnv;
    silent = false;
    loadInNixShell = true;
    direnvrcExtra = "";
    nix-direnv = {
      enable = true;
      package = pkgs.nix-direnv;
    };
  };

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # Set primary user, because nix-darwin requires it (for now) as part of the
  # migration to use "sudo darwin-rebuild".
  #
  # https://github.com/nix-darwin/nix-darwin/blob/fa6120c32f10bd2aac9e8c9a6e71528a9d9d823b/modules/system/primary-user.nix#L53-L58
  system.primaryUser = "l";

  # Disable default "walters" prompt, which adds an annoying green PWD string at the far right hand side of the terminal.
  programs.zsh.promptInit = "";

  environment.variables.SHELL = "${pkgs.zsh}/bin/zsh";
  environment.variables.LANG = "en_US.UTF-8";
}
