{ config, pkgs, ... }:

{
  imports =
    [
      ../bare.nix
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/disk/by-id/ata-KINGSTON_SVP100S296G_X0NA40R9K0KK";

  boot.initrd.luks.devices = [
    {
      name = "luksroot";
      device = "/dev/disk/by-id/ata-KINGSTON_SVP100S296G_X0NA40R9K0KK-part2";
      preLVM = true;
    }
  ];

  # Mount our secondary luks (RAID1) device. Actual path is just
  # "/root/keyfile", but we are in "stage 1" where the root filesystem is
  # mounted at "/mnt-root", not "/".
  boot.initrd.postMountCommands = "cryptsetup luksOpen --key-file /mnt-root/root/keyfile /dev/md127 craid";

  boot.kernel.sysctl = {
    # Make the kernel reluctant to use swap.
    "vm.swappiness" = 5;
  };

  networking = {
    hostName = "m0";
    extraHosts = ''
      127.0.0.1 dashboard.twinprime.dev
      127.0.0.1 web-api.twinprime.dev
    '';
  };
}
