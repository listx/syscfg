# See configuration.nix(5) manpage.
{ config, pkgs, ... }:

{
  imports =
    [
      ../base.nix
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/disk/by-id/ata-VBOX_HARDDISK_VBf13324d7-d759fdcb";

  networking = {
    hostName = "w0";
    hostId =  "2650ce1d";
    extraHosts = ''
      192.168.0.100 forest
      192.168.0.110 k0
      192.168.0.114 k1
      192.168.0.116 k3
      192.168.0.120 ocean
      192.168.0.130 mac
      192.168.0.132 w0
      74.207.246.114 l0

      127.0.0.1 dashboard.twinprime.dev
      127.0.0.1 web-api.twinprime.dev
    '';
    networkmanager.enable = true;
  };

  virtualisation.docker.enable = true;

  services.xserver = {
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
    displayManager.sessionCommands = ''
      # We are running NixOS as a guest from Mac OSX host through VirtualBox. On
      # the Mac, make the CapsLock key do nothing under the Keyboard -> Modifier
      # Keys settings. Then install "Seil" and make CapsLock behave as the
      # *right* Command key (Super_R from X11's pov). The final trick is to use
      # xmodmap to make Super_R become part of the mod3 modifer group, which is
      # what we're doing below.
      #
      # The only reason we are doing this is because the extra Mac OSX and
      # VirtualBox layers mess up our keys before they are even sent into the
      # VM. Whereas the base configuration handles removing Caps_Lock with
      # xmodmap, we take care of that step from the host OS (Mac + Seil), and
      # simply define a key into the mod3 group (which our XMonad config uses.)
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "remove mod4 = Super_R"
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "add mod3 = Super_R"
      # set keyboard press repeat delay/rate
      ${pkgs.xlibs.xset}/bin/xset r rate 250 80
      # disable mouse acceleration
      ${pkgs.xlibs.xset}/bin/xset m 0 0
      # use arrow, not "X" symbol, for default mouse pointer
      ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
    '';
  };
}
