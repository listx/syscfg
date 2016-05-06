{ config, pkgs, ... }:

{
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
      192.168.0.131 w1
      192.168.0.132 w0
      74.207.246.114 l0
    '';
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

  # We're going to use `gpg-agent` with SSH support --- so to avoid conflict,
  # disable OpenSSH's ssh-agent.
  programs.ssh.startAgent = false;

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

  # NTP for automated system clock adjustments.
  services.ntp.enable = true;

  services.udev.extraRules = ''
    # For Yubikey, enable 'ykinfo -v' and 'gpg2 --card-status' commands as normal (non-root) user.
    # From http://stafwag.github.io/blog/blog/2015/06/16/using-yubikey-neo-as-gpg-smartcard-for-ssh-authentication/
    ACTION!="add|change", GOTO="yubico_end"

    # Udev rules for letting the console user access the Yubikey USB
    # device node, needed for challenge/response to work correctly.

    # Yubico Yubikey II
    ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0010|0110|0111|0114|0116|0401|0403|0405|0407|0410", OWNER="l", MODE="0600"

    LABEL="yubico_end"
  '';

  virtualisation.virtualbox.host.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "devicemapper";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  time.timeZone = "America/Los_Angeles";

  i18n.inputMethod.enabled = "uim";

  users.extraGroups.l = {
    gid = 1000;
  };
  # Set password for extra users with 'passwd' command as root.
  users.extraUsers.l = {
    group = "l";
    description = "Linus Arver";
    createHome = true;
    home = "/home/l";
    extraGroups = [ "wheel" "docker" "vboxusers" ] ;
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };
}
