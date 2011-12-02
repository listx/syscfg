{config, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    ack
    colordiff
    cpufrequtils
    emacs
    flac
    gcc
    gitAndTools.gitFull
    gnumake
    gnupg
    haskellPackages.ghc
    haskellPackages.cmdargs
    htop
    iftop
    libertine
    manpages
    mutt
    msmtp
    pinentry
    p7zip
    rtorrent
    rxvt_unicode
    terminus_font
    timidity
    tmux
    vim
    weechat
    wget
    unrar
    unzip
    xz
    zip
    zsh
    (let myTexLive =
        texLiveAggregationFun {
            paths = [
                texLive
                texLiveExtra
                texLiveBeamer
                texLiveLatexXColor
                texLivePGF
                texLive
            ];
        };
        in myTexLive)
  ];

    #firefoxWrapper
    #chromeWrapper
    #openoffice # shouldn't it be libreoffice???
  environment.x11Packages = with pkgs; [
    evince
    geeqie
    haskellPackages.xmonad
    haskellPackages.xmonadContrib
    haskellPackages.xmonadExtras
    MPlayer
    scrot
    xchm
    xorg.xmodmap
    xsel
  ];

  nix.useChroot = true;

  time.timeZone = "America/Los_Angeles";

  boot.initrd.kernelModules = [
    # Specify all kernel modules that are necessary for mounting the root
    # file system.
    #
    "ext4"
  ];

  # use linux 3.1.2
  boot.kernelPackages = pkgs.linuxPackages_3_1;

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
    192.168.0.110 k0
    192.168.0.111 k2
    192.168.0.112 k2w
    192.168.0.113 k1
    192.168.0.114 k1w
    192.168.0.120 ocean
  ";

  # allow use of "sudo" command
  security.sudo.enable = true;
  # set the sudoers file
  security.sudo.configFile = ''
        # Don't edit this file. Set nixos option security.sudo.configFile instead

        # "root" is allowed to do anything.
        root        ALL=(ALL) SETENV: ALL

        # Users in the "wheel" group can do anything.
        %wheel      ALL=(ALL) SETENV: ALL

        # shutdown
        l ALL=NOPASSWD: /var/run/current-system/sw/sbin/shutdown
        l ALL=NOPASSWD: /var/run/current-system/sw/bin/cpufreq-set

        # mounting
        l ALL=NOPASSWD: /var/run/current-system/sw/bin/mount
        l ALL=NOPASSWD: /var/run/current-system/sw/bin/umount
        l ALL=NOPASSWD: /var/run/current-system/sw/sbin/blkid

        # networking
        l ALL=NOPASSWD: /var/run/current-system/sw/sbin/iftop
        l ALL=NOPASSWD: /var/run/current-system/sw/sbin/ifconfig

        # printing
        l ALL=NOPASSWD: /var/run/current-system/sw/bin/cancel
        l ALL=NOPASSWD: /var/run/current-system/sw/bin/lprm
  '';

  # Add CUPS to print documents.
  services.printing.enable = true;

  # Add the NixOS Manual on virtual console 8
  services.nixosManual.showManual = true;
}

