{ config, pkgs, ... }:

{
  imports =
    [
      ../base.nix
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # See https://askubuntu.com/a/863301 (this fixes flooding of the kernel logs
  # with "printk messages dropped".
  boot.kernelParams = ["pcie_aspm=off"];

  boot.initrd.luks.devices = [
    {
      name = "luksroot";
      device = "/dev/disk/by-id/nvme-SAMSUNG_MZVKW512HMJP-000H1_S34CNA0J100907-part2";
      preLVM = true;
    }
  ];

  boot.kernel.sysctl = {
    # Make the kernel reluctant to use swap.
    "vm.swappiness" = 5;
  };

  networking = {
    hostName = "k0";
    extraHosts = ''
    '';

    defaultGateway = "192.168.0.1";
    nameservers = [ "8.8.8.8" ];
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    # export finalized xorg.conf to /etc/X11/xorg.conf
    exportConfiguration = true;
    config = pkgs.lib.mkOverride 50 (builtins.readFile ./quadmon.conf);
  };

  system.stateVersion = "18.03";
}
