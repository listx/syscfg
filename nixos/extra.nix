{ config, pkgs, ... }:

let
  melby-release = import "/home/l/prog/melby/package/build.nix";
in
{
  imports = [ ./minimal.nix ];

  environment.systemPackages = with pkgs; [
    # Essentials
    wezterm
    tmux
    zsh
    git
    git-filter-repo
    difftastic
    tig
    jujutsu
    jjui
    meld
    emacs
    neovim
    editorconfig-core-c
    ripgrep
    fzf
    fd
    gnumake
    sqlite # emacs' org-roam needs it
    noweb # literate emacs configuration
    gcal
    melby-release.melby-daemon
    melby-release.melby-client-rust
    babashka

    # Dev tools
    parallel
    unixtools.xxd
    dos2unix
    jless
    jq
    dhall
    dhall-json
    protobuf
    tokei
    util-linux
    binutils

    # Nix
    niv

    # Shell
    shellcheck

    # ----------

    # Email
    notmuch
    lieer

    # Web
    firefox
    chromium
    qutebrowser

    # X server utils
    xorg.xmodmap
    xorg.xev
    xdotool
    xorg.xdpyinfo
    xorg.xkill
    xclip
    xorg.xwininfo
    xcompmgr
    xsel
    xscreensaver

    # Monitoring/Inspection
    htop
    bottom
    bmon
    ncdu
    tree
    pciutils # lspci
    lsof

    # Cryptography
    gnupg
    openssl
    pinentry-curses
    cryptsetup
    pass

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
    stdman
    stdmanpages

    # Archiving
    p7zip
    unzip
    unrar
    zip

    # Filesystem
    fuse
    parted
    gparted
    ntfs3g
    gptfdisk
    sshfs-fuse
    woeusb
    inotify-tools

    # Image
    scrot
    geeqie
    gimp
    imagemagick
    inkscape
    pdf2svg

    # Audio/Video
    mpv
    ffmpeg
    flac
    mpc
    vimpc
    mpd
    r128gain
    pavucontrol
    pulseaudio

    # Document processing
    pdftk
    poppler-utils # pdftotext
    zathura
    #haskellPackages.auca
    texlive.combined.scheme-full
    pandoc

    # Fonts
    raleway
    noto-fonts

    # Data analysis
    graphviz
    gnuplot

    # Containerization
    docker
    kubectl
    kind
    ko
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
