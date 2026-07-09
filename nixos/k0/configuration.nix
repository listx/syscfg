{ config, pkgs, ... }:

{
  imports = [
    ../extra.nix #
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # See https://askubuntu.com/a/863301 (this fixes flooding of the kernel logs
  # with "printk messages dropped".
  boot.kernelParams = [ "pcie_aspm=off" ];

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
    extraHosts = "";

    defaultGateway = "192.168.1.1";
    nameservers = [ "8.8.8.8" ];

    # Use the USB Wifi adapter "ALFA AWUS036ACHM". See
    # https://github.com/morrownr/USB-WiFi/blame/73bf16e0621daafdbc9c852836ea0473616ec626/README.md#L61.
    wireless.enable = true;
    # This enables wireless support via /etc/wpa_supplicant/imperative.conf.
    wireless.allowAuxiliaryImperativeNetworks = true;
    wireless.interfaces = [
      "wlp0s20u6"
    ];
  };

  systemd.user.services.notmuch-sync = {
    description = "Sync Gmail with lieer and update Notmuch index";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "/home/l/syscfg/script/mail-sync.sh";
      Environment = "PATH=/run/current-system/sw/bin:/usr/bin:/bin"; #
    };
  };

  systemd.user.timers.notmuch-sync = {
    description = "Run mail sync every 15 minutes";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "2min";
      OnUnitActiveSec = "15min";
      Persistent = true;
    };
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    # export finalized xorg.conf to /etc/X11/xorg.conf
    exportConfiguration = true;
    config = pkgs.lib.mkOverride 50 (builtins.readFile ./xorg.conf);
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  # Unfortunately, upgrading to nixos 21.11 led to us getting the new 495.44
  # driver, which breaks our configuration (boo NVIDIA). So, we are forced to
  # use the older version (which works perfectly!). The configuration here is
  # taken from this commit
  # https://github.com/NixOS/nixpkgs/commit/f8d38db8d7c995e0e20ab6b4e48cac26c2ef0dfa.
  # The instructions at https://nixos.wiki/wiki/Nvidia led me to this commit.
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;

  # Audio
  services.pipewire.pulse.enable = true;

  # Binary Cache for Haskell.nix
  nix.settings.trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  nix.settings.substituters = [ "https://cache.iog.io" ];
}
