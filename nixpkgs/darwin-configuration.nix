{ config, lib, pkgs, ... }:

let
  melby-release = import "${HOME}/prog/melby/package/build.nix";
  HOME = builtins.getEnv "HOME";
in
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "aspell-dict-en-science"
  ];
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    [ (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
      autoconf
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
      direnv
      dos2unix
      emacs
      fd
      ffmpeg
      findutils
      fzf
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
      procps
      ripgrep
      rustfmt
      scc
      source-serif
      stack
      tig
      tmux
      tree
      util-linux
      wezterm
      xz
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
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Enable Lorri daemon.
  services.lorri.enable = true;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # Disable default "walters" prompt, which adds an annoying green PWD string at the far right hand side of the terminal.
  programs.zsh.promptInit = "";

  environment.loginShell = "${pkgs.zsh}/bin/zsh -l";
  environment.variables.SHELL = "${pkgs.zsh}/bin/zsh";
  environment.variables.LANG = "en_US.UTF-8";
}
