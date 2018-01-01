{ config, pkgs, ... }:

{
  imports = [
    ./bare.nix
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

  i18n.consoleFont = "lat9w-16";


  nixpkgs.config.firefox = {
    enableAdobeFlash = true;
  };

  services.openvpn.servers = {
    # Unless `autoStart = false;', all entries here start automatically as a
    # systemd service. To stop the `home' OpenVPN client service, run `sudo
    # systemctl stop openvpn-home'.
    home = {
      config = builtins.readFile ../openvpn/home.ovpn;
      up = "echo nameserver $nameserver | ${pkgs.openresolv}/sbin/resolvconf -m 0 -a $dev";
      down = "${pkgs.openresolv}/sbin/resolvconf -d $dev";
    };
  };

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
      # use circle, not "X" symbol, for default mouse pointer
      ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name circle
    '';
    displayManager.slim.defaultUser = "l";
    displayManager.slim.autoLogin = true;
    # We rely on ~/.xsession to start XMonad, instead of NixOS automagically
    # doing it for us. This way, we can use our xmonad binary compiled by Stack.
    windowManager.xmonad.enable = false;
    windowManager.xmonad.enableContribAndExtras = false;
  };

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

  i18n.inputMethod.enabled = "uim";

  users.extraUsers.l.extraGroups = [ "wheel" "docker" "vboxusers" ] ;
}
