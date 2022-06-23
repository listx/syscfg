{ config, pkgs, ... }:

{
  imports = [ ../extra.nix ./hardware-configuration.nix ../k8s-node.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device =
    "/dev/disk/by-id/ata-KINGSTON_SVP100S296G_X0NA40R9K0KK";

  boot.initrd.luks.devices = {
    luksroot = {
      device = "/dev/disk/by-id/ata-KINGSTON_SVP100S296G_X0NA40R9K0KK-part2";
      preLVM = true;
    };
  };

  # Mount our secondary luks (RAID1) device. Actual path is just
  # "/root/keyfile", but we are in "stage 1" where the root filesystem is
  # mounted at "/mnt-root", not "/".
  boot.initrd.postMountCommands =
    "cryptsetup luksOpen --key-file /mnt-root/root/keyfile /dev/md127 craid";

  boot.kernel.sysctl = {
    # Make the kernel reluctant to use swap.
    "vm.swappiness" = 5;
  };

  networking = {
    hostName = "m0";

    # Static IP.
    interfaces.enp2s0.ipv4.addresses = [{
      address = "192.168.0.3";
      prefixLength = 24;
    }];
    defaultGateway = "192.168.0.1";
    nameservers = [ "8.8.8.8" ];
  };
}
