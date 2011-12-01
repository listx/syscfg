{config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    gcc
    gitAndTools.gitFull
    gnumake
    haskellPackages.ghc
    haskellPackages.xmonad
    htop
    libertine
    manpages
    rxvt_unicode
    terminus_font
    tmux
    vim
    xorg.xmodmap
    weechat
    wget
    zsh
  ];

  # this is unused for now
  environment.shellInit = ''
    export GEM_PATH=/var/run/current-system/sw/${pkgs.ruby.gemPath}
    export RUBYLIB=/var/run/current-system/sw/lib
    export RUBYOPT=rubygems
  '';

    #firefoxWrapper
    #chromeWrapper
    #openoffice # shouldn't it be libreoffice???
    #adobeReader
  environment.x11Packages = with pkgs; [
    MPlayer
    scrot
    haskellPackages.xmonad
    haskellPackages.xmonadContrib
    haskellPackages.xmonadExtras
  ];

  nix.useChroot = true;

  time.timeZone = "America/Los_Angeles";

  boot.initrd.kernelModules = [
    # Specify all kernel modules that are necessary for mounting the root
    # file system.
    #
    "ext4"
  ];

  boot.loader.grub = {
    # Use grub 2 as boot loader.
    enable = true;
    version = 2;
  };

  users.defaultUserShell = "/bin/sh";

  networking.interfaceMonitor.enable = true; # Watch for plugged cable.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Add XServer (default if you have used a graphical iso)
  services.xserver = {
    enable = true;
    layout = "us";
    windowManager.xmonad.enable = true;
    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    startOpenSSHAgent = true;
    # Automatic login as regular user (no need to put up with entering
    # password)
    displayManager = {
        auto = {
            enable = true;
            user = "l"; # login as "l"
        };
    };
  };

  # OpenSSH daemon
  services.openssh.enable = true;

  networking.extraHosts = "
    192.168.0.100 forest
    192.168.0.110 exelion
    192.168.0.111 luxion.e
    192.168.0.112 luxion.w
    192.168.0.113 aether.e
    192.168.0.114 aether.w
    192.168.0.120 ocean
  ";

  # Add CUPS to print documents.
  services.printing.enable = true;

  # Add the NixOS Manual on virtual console 8
  services.nixosManual.showManual = true;
}

