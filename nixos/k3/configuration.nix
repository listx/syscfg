# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/disk/by-id/ata-ST9500420AS_5VJ59T8M";
  boot.loader.grub.extraEntries = ''
    menuentry "Windows7" {
      title Windows7
      insmod ntfs
      set root='(hd1,2)'
      chainloader +1
    }
  '';
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/1414ec8e-89fb-476a-9796-13b158fa1019";
      preLVM = true;
    }
  ];

  networking = {
    extraHosts = ''
      192.168.1.110 k0
    '';
    hostName = "k3"; # Define your hostname.
    hostId = "518ab295";
    wireless.enable = true;  # Enables wireless.
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

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

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # basic console/coding tools
    wget
    curl
    zsh
    xlibs.xmodmap
    rxvt_unicode
    tmux
    emacs
    xsel
    vim
    silver-searcher
    gnupg
    htop

    # source control
    git
    tig
    mercurial

    # c/c++
    gcc
    gdb
    gnumake
    ruby

    # browsers and multimedia
    firefox
    chromium
    aria2
    rtorrent
    mplayer2
    smplayer
    vlc
    higan
  ];

  # Create a /etc/zshenv and other things to make Zsh work properly. Among
  # other things, this allows us to perform a "git pull <this machine's IP>"
  # from a remote machine; without this, the login shell cannot find the
  # git-upload-pack command, and the git pull operation will fail.
  programs.zsh.enable = true;

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

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  #services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = [ "ati" ];
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "terminate:ctrl_alt_bksp";
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };
    config = ''
      Section "InputClass"
        Identifier "touchpad catchall"
        Driver "synaptics"
        MatchIsTouchpad "on"
        MatchDevicePath "/dev/input/event*"
        Option "TapButton1" "0"
        Option "TapButton2" "0"
        Option "TapButton3" "0"
      EndSection
    '';
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";
    desktopManager.default = "none";
    displayManager.sessionCommands = ''
      ${pkgs.xlibs.xset}/bin/xset r rate 250 80
    '';
  };

}
#device                          fs_type         label            mount point         UUID
#-------------------------------------------------------------------------------------------------------------------------
#/dev/root                       iso9660         NIXOS_ISO        /iso                2015-01-13-11-30-46-00
#/dev/loop0                      squashfs                         /nix/.ro-store      
#/dev/sda1                       vfat            RECOVERY         (not mounted)       3C98-AC5D
#/dev/sda2                       ntfs            OS               (not mounted)       5EF0D2BAF0D2981B
#/dev/sda3                       ext2            boot             /mnt/boot           184c55ec-db57-4a37-b997-72ec57f89a0f
#/dev/sda4                       crypto_LUKS                      (in use)            1414ec8e-89fb-476a-9796-13b158fa1019
#/dev/sdb1                       ext4                             (not mounted)       66d852d9-b62d-43aa-9897-b8a8243c9a12
#/dev/sdb2                       crypto_LUKS                      (not mounted)       b27b5ca9-8104-4425-8a8e-95693ce4d1a1
#/dev/mapper/luks                LVM2_member                      (in use)            dpGfxA-J5Bu-Ejl1-aO2G-G0Vm-f4cs-UKwhav
#/dev/mapper/vg0-root            ext4                             /mnt                e30c7293-51b1-4786-b6e7-adcb8492caf9
#/dev/mapper/vg0-swap            swap                             <swap>              e98031b6-649f-41a3-8ae5-40041855b0e3
#/dev/mapper/vg0-home            ext4                             /mnt/home           876ddf6a-1c9c-42e2-b4c4-bebf27b3261e
