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
  boot.loader.grub.device = "/dev/disk/by-id/ata-ST9500420AS_5VJ59T8M";
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/1414ec8e-89fb-476a-9796-13b158fa1019";
      preLVM = true;
    }
  ];
  boot.loader.grub.extraEntries = ''
    menuentry "Windows7" {
      insmod ntfs
      set root='(hd0,2)'
      chainloader +1
    }
  '';

  # KNOWN ISSUES
  #
  # - If we suspend with `systemctl suspend`, and then resume, the wireless
  # connection won't work. The command `ping www.google.com` won't work. To fix
  # this, manually invoke `systemctl restart network-addresses-wlp3s0.service`.
  networking = {
    hostName = "k3";
    hostId = "518ab295";
    wireless.enable = true;
    interfaces = {
      wlp3s0 = {
        ipAddress = "192.168.1.116";
        prefixLength = 24;
      };
    };
  };

  services.xserver = {
    videoDrivers = [ "ati" ];
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
  };
}
