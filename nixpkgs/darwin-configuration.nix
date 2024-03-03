{ config, pkgs, ... }:

let
  mool-release = import ../../prog/mool/package/build.nix;
  HOME = builtins.getEnv "HOME";
in
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    [ alacritty
      bashInteractive
      coreutils
      dhall
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
      jujutsu
      less
      lorri
      mool-release.mool-server
      mool-release.mool-client
      mpv
      neovim
      nodejs
      pass
      pdf2svg
      procps
      ripgrep
      rustfmt
      source-serif
      stack
      tig
      tmux
      tree
      util-linux
      wezterm
      xz
      youtube-dl
      zsh
      zstd
    ];

  # Enable mools with launchd.
  launchd.user.agents.mools = {
    script = "${mool-release.mool-server}/bin/mools start";
    environment = {
      RELEASE_COOKIE = "${HOME}/.mool/cookie";
      LUA_PATH = "${HOME}/.mool/?.lua";
    };
    #path = [ mool-release.mool-server ];
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
    };
  };

  # Enable Karabiner-Elements.
  services.karabiner-elements.enable = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

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
