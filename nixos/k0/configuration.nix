{ config, pkgs, ... }:

{
  imports = [ ../extra.nix ./hardware-configuration.nix ../k8s-master.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # See https://askubuntu.com/a/863301 (this fixes flooding of the kernel logs
  # with "printk messages dropped".
  boot.kernelParams = [ "pcie_aspm=off" ];

  boot.initrd.luks.devices = {
    luksroot = {
      device =
        "/dev/disk/by-id/nvme-SAMSUNG_MZVKW512HMJP-000H1_S34CNA0J100907-part2";
      preLVM = true;
    };
  };

  boot.kernel.sysctl = {
    # Make the kernel reluctant to use swap.
    "vm.swappiness" = 5;
  };

  networking = {
    hostName = "k0";
    extraHosts = "";

    defaultGateway = "192.168.0.1";
    nameservers = [ "8.8.8.8" ];

    # Use the USB Wifi adapter "ALFA AWUS036ACHM". See
    # https://github.com/morrownr/USB-WiFi/blame/73bf16e0621daafdbc9c852836ea0473616ec626/README.md#L61.
    # This enables wireless support via /etc/wpa_supplicant.conf.
    wireless.enable = true;
    # Assign static IP manually. The other half of this trick to make it work
    # is to tell the Motorola router to start its DHCP lease assignment address
    # from 192.168.0.240+, and then use the addresses 192.168.0.2-192.168.0.239
    # statically (192.168.0.1 is reserved for the router itself).
    interfaces.wlp0s20u4u3.ipv4.addresses = [{
      address = "192.168.0.4";
      prefixLength = 24;
    }];
  };

  # Enable cron service
  services.cron = {
    enable = true;
    systemCronJobs = [
      "* * * * *      l     2>&1 zsh -l -c ~/syscfg/script/export-agenda.sh | tee -a ~/agenda.cron.log.txt"
      "0 * * * *      l     rm -f ~/agenda.cron.log.txt"
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

  # Binary Cache for Haskell.nix
  nix.binaryCachePublicKeys =
    [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  nix.binaryCaches = [ "https://hydra.iohk.io" ];
}
