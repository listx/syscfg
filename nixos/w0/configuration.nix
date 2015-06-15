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

  # When we import from `base.nix`, we get the values defined there as well.
  # `base.nix` defines "wheel" for us, so we add any other extra groups we
  # should belong to below."
  users.extraUsers.l.extraGroups = [
    "docker"
  ];

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
      # Scroll Lock key (Scroll_Lock from X11's pov), by assigning it the Mac
      # keycode 107 (listed in the menu as F14). There is no way to make Seil
      # make capslock behave as Hyper_L, so we make it behave as Scroll_Lock
      # instead.

      # Apparently, importing from `base.nix` results in still reading those
      # options, at least for `sessionCommands`. I.e., the setting below is
      # appended along (before or after, it is not clear) with the
      # `sessionCommands` config option in `base.nix`. We piggyback along on top
      # of the existing Hyper_L settings defined there.

      # First prevent Scroll Lock from behaving like a toggled key.
      ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "keysym Scroll_Lock = Hyper_L"
    '';
  };
}
