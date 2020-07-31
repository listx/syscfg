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

  # Prettify the virtual console font early on with Terminus.
  console = {
    font = "ter-114n";
    packages = with pkgs; [ terminus_font ];
    earlySetup = true;
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
    displayManager.defaultSession = "l_xmonad";
    # See https://unix.stackexchange.com/questions/597358/nixos-how-to-configure-custom-desktop-session/597359#597359.
    displayManager.session = [
      {
        manage = "desktop";
        name = "l_xmonad";
        start = ''exec $HOME/.xsession'';
      }
    ];
    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = "l";
    };
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
