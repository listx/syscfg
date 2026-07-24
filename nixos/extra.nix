{ config, pkgs, ... }:

let
  melby-release = import "/home/l/prog/melby/package/build.nix";
in
{
  imports = [ ./minimal.nix ];

  environment.systemPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    babashka
    binutils
    bmon
    bottom
    cachix
    dhall
    dhall-json
    difftastic
    dos2unix
    editorconfig-core-c
    emacs
    fd
    fzf
    gcal
    git
    git-filter-repo
    gnumake
    htop
    jjui
    jless
    jq
    jujutsu
    lsof
    melby-release.melby-client-rust
    melby-release.melby-daemon
    meld
    ncdu
    neovim
    niv
    nix-diff
    parallel
    pciutils # lspci
    protobuf
    ripgrep
    shellcheck
    sqlite # emacs' org-roam needs it
    tig
    tmux
    tokei
    tree
    unixtools.xxd
    util-linux
    wezterm
    zsh

    p7zip
    unzip
    unrar
    xz
    zip

    notmuch
    lieer

    imagemagick
    inkscape

    ffmpeg
    gnupg
    graphviz
    pass
    zathura

    # Web
    firefox
    qutebrowser

    # X server utils
    xmodmap
    xev
    xdotool
    xdpyinfo
    xkill
    xclip
    xwininfo
    xcompmgr
    xsel
    xscreensaver

    # Cryptography
    openssl
    pinentry-curses
    cryptsetup

    # Net
    bind # dig
    rsync
    wget
    curl
    inetutils # ping
    dhcpcd
    rtorrent

    # Docs
    man-pages

    # Filesystem
    inotify-tools

    # Image
    gimp

    # Audio/Video
    mpv
    flac
    mpc
    vimpc
    mpd
    r128gain
    pavucontrol
    pulseaudio

    # Document processing
    pandoc

    # Data analysis
    gnuplot

    # Containerization
    docker
    kubectl
  ];

  nixpkgs.overlays = [ (import ./overlay.nix) ];

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

  services.displayManager = {
    defaultSession = "l_xmonad";
    autoLogin.enable = true;
    autoLogin.user = "l";
  };

  services.xserver = {
    enable = true;
    xkb.layout = "us";
    xkb.variant = "altgr-intl";
    xkb.options = "terminate:ctrl_alt_bksp";
    # See https://unix.stackexchange.com/questions/597358/nixos-how-to-configure-custom-desktop-session/597359#597359.
    displayManager.session = [
      {
        manage = "desktop";
        name = "l_xmonad";
        start = "exec $HOME/.xsession";
      }
    ];
    # We rely on ~/.xsession to start XMonad, instead of NixOS automagically
    # doing it for us. This way, we can use our xmonad binary compiled by Stack.
    windowManager.xmonad.enable = false;
    windowManager.xmonad.enableContribAndExtras = false;
  };

  # Enable X screen saver (xscreensaver).
  services.xscreensaver.enable = true;

  virtualisation.virtualbox.host.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "overlay2";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  users.extraUsers.l.extraGroups = [
    "wheel"
    "docker"
    "vboxusers"
  ];
}
