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
    rxvt_unicode
    rxvt_unicode.terminfo
    tmux
    emacs
    xsel
    vim
    silver-searcher
    gnupg
    mutt
    htop
    dhcpcd

    # Programming tools
    git
    tig
    mercurial
    gcc
    gdb
    gnumake
    ruby
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
    glfw
    glxinfo

    # Browsers and multimedia
    firefoxWrapper
    chromium
    aria2
    rtorrent
    geeqie
    mpv
    vlc
    higan
    cmus
    evince
    pidgin
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
    displayManager.sessionCommands = ''
      # remap Caps_Lock key to xmonad's own, exclusive 'mod' key (no "sharing"
      # with ALT or any other combination)
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "remove Lock = Caps_Lock"
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "add mod3 = Caps_Lock"
      ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.xmodmap
      ${pkgs.xlibs.xset}/bin/xset r rate 250 80
    '';
    displayManager.slim.defaultUser = "l";
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";
  };

  # Enable CUPS to print documents.
  #services.printing.enable = true;

  time.timeZone = "America/Los_Angeles";

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
