{ config, pkgs, ... }:

{
  imports =
    [
      ../extra.nix
      ./hardware-configuration.nix
      ../k8s-master.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # See https://askubuntu.com/a/863301 (this fixes flooding of the kernel logs
  # with "printk messages dropped".
  boot.kernelParams = ["pcie_aspm=off"];

  boot.initrd.luks.devices = {
    luksroot = {
      device = "/dev/disk/by-id/nvme-SAMSUNG_MZVKW512HMJP-000H1_S34CNA0J100907-part2";
      preLVM = true;
    };
  };

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

  # Enable cron service
  services.cron = {
    enable = true;
    systemCronJobs = [
      "* * * * *      l     zsh -l -c ~/syscfg/script/export-agenda.sh"
    ];
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    # export finalized xorg.conf to /etc/X11/xorg.conf
    exportConfiguration = true;
    config = pkgs.lib.mkOverride 50 (builtins.readFile ./quadmon.conf);
  };

  # Steps to add printer: Go to localhost:631 to access the CUPS admin page.
  # Then add a printer by specifying http://<PRINTER_IP_ADDRESS>:631/ipp.
  # Specify the correct driver for the make and model. As of 2020-01-03 the
  # settings are:
  #
  # Driver:       HP LaserJet Pro MFP m125nw, hpcups 3.20.5 (color)
  # Connection:   http://192.168.0.2:631/ipp
  #
  # Use the sudo username and password for adding the printer (last step).
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };
}
