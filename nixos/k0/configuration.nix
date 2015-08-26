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
    extraHosts = ''
      192.168.0.132 dashboard.twinprime.dev
      192.168.0.132 web-api.twinprime.dev
    '';
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    # export finalized xorg.conf to /etc/X11/xorg.conf
    exportConfiguration = true;
    config = pkgs.lib.mkOverride 50 (builtins.readFile ./quadmon.conf);
 };
}
