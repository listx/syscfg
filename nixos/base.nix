let
  HEAD = import /home/l/prog/foreign/nixpkgs {};
in

{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # Basic console tools
    wget
    curl
    zsh
    xlibs.xmodmap
    xlibs.xev
    rxvt_unicode
    rxvt_unicode.terminfo
    tmux
    emacs
    xsel
    vim
    silver-searcher
    colordiff
    gnupg
    mutt
    htop
    dhcpcd
    utillinuxCurses
    lsof

    # Docs
    manpages
    pthreadmanpages
    stdmanpages

    # Archiving
    p7zip
    unzip
    lzma
    unrar

    # Programming tools
    git
    tig
    mercurial
    clang
    gcc
    gdb
    gnumake
    ruby
    bundler_HEAD
    bundix
    # For haskell development via `nix-shell`; run `sudo nix-channel --add
    # <nixpkgs-unstable>`, then do `sudo nix-channel --update`.
    # <nixpkgs-unstable> is located at
    # https://nixos.org/channels/nixpkgs-unstable. The `haskellngPackages` set
    # is only in nixpkgs-unstable as of 2014-02-06.
    #
    # It's OK to have multple NixOS channels at the same time. Nix takes care of
    # dependencies without issue.
    haskellngPackages.cabal2nix
    # For invoking, e.g., `cabal2nix cabal://some-package`, because cabal2nix
    # depends on `cabal update`. We also get `cabal repl` and other commands
    # from the `cabal` binary with this package. The package
    # 'haskellPackages.cabalInstall' has been renamed to
    # 'haskellngPackages.cabal-install'.
    haskellngPackages.cabal-install
    # android development
    androidsdk_4_4
    android-udev-rules

    # Programming libraries
    boehmgc
    glfw
    glxinfo
    pcg_c

    # Browsers and multimedia
    firefoxWrapper
    chromium
    aria2
    rtorrent
    geeqie
    mpv
    ffmpeg
    flac
    shntool
    vlc
    higan
    cmus
    evince
    pidgin
    links
    gimp
    scrot
    texLiveFull
  ];

  # Fonts
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      baekmuk-ttf
      corefonts
      dejavu_fonts
      ipafont
      libertine
      source-serif-pro
      terminus_font
      ubuntu_font_family
    ];
  };

  hardware.pulseaudio.enable = true;

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    extraHosts = ''
      192.168.0.100 forest
      192.168.0.110 k0
      192.168.0.114 k1
      192.168.0.116 k3
      192.168.0.120 ocean
      192.168.0.130 mac
      192.168.0.132 w0
      74.207.246.114 l0
    '';
    networkmanager.enable = true;
  };

# allow installation of 'ati_unfree' video driver and also Firefox with Flash
  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.firefox = {
    enableAdobeFlash = true;
  };

  # Create a /etc/zshenv and other things to make Zsh work properly. Among
  # other things, this allows us to perform a "git pull <this machine's IP>"
  # from a remote machine; without this, the login shell cannot find the
  # git-upload-pack command, and the git pull operation will fail.
  programs.zsh.enable = true;

  # Delete things that come built-in by nix that customizes Zsh. For one thing,
  # disable the prompt settings because they interfere with our own
  # customizations.
  programs.zsh.promptInit = "";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "terminate:ctrl_alt_bksp";
    desktopManager.default = "none";
    # workaround for disabling xterm
    # see https://github.com/NixOS/nixpkgs/issues/4300
    desktopManager.xterm.enable = false;
    displayManager.sessionCommands = ''
      # Disable Caps_Lock from behaving like a "Lock" key, convert it to behave
      # as a Hyper_L key, and then add Hyper_L to its own unique modifer group,
      # mod3 (mod3 is unused by default). We use mod3 as our XMonad key as
      # "mod3Mask" in xmonad.hs.
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "remove Lock = Caps_Lock"
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "keysym Caps_Lock = Hyper_L"
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "remove mod4 = Hyper_L"
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "add mod3 = Hyper_L"
      # set keyboard press repeat delay/rate
      ${pkgs.xlibs.xset}/bin/xset r rate 250 80
      # disable mouse acceleration
      ${pkgs.xlibs.xset}/bin/xset m 0 0
      # use arrow, not "X" symbol, for default mouse pointer
      ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
    '';
    displayManager.slim.defaultUser = "l";
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  time.timeZone = "America/Los_Angeles";

  uim.enable = true;

  users.extraGroups.l = { };
  # Set password for extra users with 'passwd' command as root.
  users.extraUsers.l = {
    group = "l";
    description = "Linus Arver";
    createHome = true;
    home = "/home/l";
    extraGroups = [ "wheel" ] ;
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };

}
