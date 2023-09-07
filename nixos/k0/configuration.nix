{ config, pkgs, ... }:

{
  imports =
    [
      ../extra.nix
      ./hardware-configuration.nix
    ];

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

    defaultGateway = "10.0.0.1";
    nameservers = [ "8.8.8.8" ];

    # Use the USB Wifi adapter "ALFA AWUS036ACHM". See
    # https://github.com/morrownr/USB-WiFi/blame/73bf16e0621daafdbc9c852836ea0473616ec626/README.md#L61.
    # This enables wireless support via /etc/wpa_supplicant.conf.
    wireless.enable = true;
    # Assign static IP manually. The other half of this trick to make it work
    # is to tell the router to start its DHCP lease assignment address
    # from 10.0.0.240+, and then use the addresses 10.0.0.2-10.0.0.239
    # statically (10.0.0.1 is reserved for the router itself).
    interfaces.wlp0s20u4u3.ipv4.addresses = [{
      address = "10.0.0.4";
      prefixLength = 24;
    }];
  };

  # Enable cron service
  services.cron = {
    enable = true;
    systemCronJobs = [
      "* * * * *      l     2>&1 zsh -l -c ~/syscfg/script/export-agenda.sh | tee -a ~/agenda.cron.log.txt"
      "5 0 * * *      l     rm -f ~/agenda.cron.log.txt"
    ];
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    # export finalized xorg.conf to /etc/X11/xorg.conf
    exportConfiguration = true;
    config = pkgs.lib.mkOverride 50 (builtins.readFile ./quadmon.conf);
  };

  # Unfortunately, upgrading to nixos 21.11 led to us getting the new 495.44
  # driver, which breaks our configuration (boo NVIDIA). So, we are forced to
  # use the older version (which works perfectly!). The configuration here is
  # taken from this commit
  # https://github.com/NixOS/nixpkgs/commit/f8d38db8d7c995e0e20ab6b4e48cac26c2ef0dfa.
  # The instructions at https://nixos.wiki/wiki/Nvidia led me to this commit.
  hardware.nvidia.package =
    config.boot.kernelPackages.nvidiaPackages.legacy_470;

  # Steps to add printer: Go to localhost:631 to access the CUPS admin page.
  # Then add a printer by specifying http://<PRINTER_IP_ADDRESS>:631/ipp.
  # Specify the correct driver for the make and model. As of 2020-01-03 the
  # settings are:
  #
  # Driver:       HP LaserJet Pro MFP m125nw, hpcups 3.20.5 (color)
  # Connection:   http://10.0.0.2:631/ipp
  #
  # Use the sudo username and password for adding the printer (last step).
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };

  # Binary Cache for Haskell.nix
  nix.settings.trusted-public-keys =
    [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  nix.settings.substituters = [ "https://cache.iog.io" ];
}
