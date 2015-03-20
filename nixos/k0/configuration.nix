{ config, pkgs, ... }:

{
  imports =
    [
      ../base.nix
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/disk/by-id/ata-Samsung_SSD_840_EVO_250GB_S1DBNSAF368573R";

  boot.initrd.luks.devices = [
    {
      name = "luksroot";
      device = "/dev/disk/by-id/ata-Samsung_SSD_840_EVO_250GB_S1DBNSAF368573R-part2";
      preLVM = true;
    }
  ];

  networking = {
    hostName = "k0";
    interfaces = {
      eno1 = {
        ipAddress = "192.168.1.110";
        prefixLength = 24;
      };
    };
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };
}
