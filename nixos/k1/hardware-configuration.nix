# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ata_piix" "usb_storage" "ums_realtek" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/3adb795c-f155-4a9b-a53c-609bbcd28238";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/4d0d83c3-dcfb-4c06-95f9-3b29c755d3ef";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/8a51a7f0-124b-4ce7-b7d4-86a09602e64e";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/5d435cd0-059d-4c42-880a-2dca398cef2a"; }
    ];

  nix.maxJobs = lib.mkDefault 2;
  powerManagement.cpuFreqGovernor = "ondemand";
}
