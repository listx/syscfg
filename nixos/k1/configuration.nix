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
  boot.loader.grub.device = "/dev/disk/by-id/ata-FUJITSU_MHZ2320BH_G2_K623T922A38A";
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-id/ata-FUJITSU_MHZ2320BH_G2_K623T922A38A-part3";
      preLVM = true;
    }
  ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  #
  # The ones below help to bootstrap the system upon an initial NixOS install.
  environment.systemPackages = with pkgs; [
    dhcpcd
    git
    gnumake
    vim
    wpa_supplicant
  ];

  # KNOWN ISSUES
  #
  # - If we suspend with `systemctl suspend`, and then resume, the wireless
  # connection won't work. The command `ping www.google.com` won't work. To fix
  # this, manually invoke `systemctl restart network-addresses-wlp3s0.service`.
  networking = {
    hostName = "k1";
    hostId = "518ab295";
    # Enables wireless support via wpa_supplicant.
    wireless.enable = true;

    # Assign static IP manually. This is a workaround the erratic DHCP IP lease
    # reservation behavior of the Motorola SBG6782-AC cable modem/wifi router
    # (that router actively deletes DHCP reservation rules). The other half of
    # this trick to make it work is to tell the Motorola router to start its
    # DHCP lease assignment address from 192.168.0.10+, and then use the
    # addresses 192.168.0.2-192.168.0.9 statically (192.168.0.1 is reserved for
    # the router itself).
    interfaces.wlp24s0.ip4 = [ { address = "192.168.0.5"; prefixLength = 24; } ];
    defaultGateway = "192.168.0.1";
    nameservers = [ "8.8.8.8" ];
  };

  services.xserver = {
    videoDrivers = [ "intel" ];
    synaptics = {
      enable = true;
      vertEdgeScroll = true;
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

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

}
