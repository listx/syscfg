{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    [ alacritty
      bashInteractive
      clang
      coreutils
      dhall
      direnv
      dos2unix
      elixir_1_14
      emacs
      fd
      ffmpeg
      findutils
      fzf
      git
      gnugrep
      gnumake
      gnupg
      gnused
      gnutar
      graphviz
      html-tidy
      htop
      inkscape
      less
      lorri
      mpv
      neovim
      nodejs
      pass
      pdf2svg
      rtorrent
      ripgrep
      rustfmt
      source-serif
      stack
      texlive.combined.scheme-full
      tig
      tmux
      tree
      util-linux
      wezterm
      xz
      youtube-dl
      zsh
    ];

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
}
