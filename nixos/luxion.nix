# Edit this configuration file which defines what would be installed on the
# system.  To Help while choosing option value, you can watch at the manual
# page of configuration.nix or at the last chapter of the manual available
# on the virtual console 8 (Alt+F8).

{config, pkgs, modulesPath, ...}:

{
    # Include the configuration for part of your system which have been
    # detected automatically.
    require = [
        ./base.nix
        "${modulesPath}/installer/scan/not-detected.nix" # FIXME: is this necessary?
    ];

    # Specify all kernel modules that are necessary for mounting the root file
    # system.
    boot.initrd.kernelModules = [
        "ata_piix" "uhci_hcd" "ehci_hcd" "ohci1394"
    ];
    boot.kernelModules = [ "acpi-cpufreq" ];
    boot.extraModulePackages = [ ];
    nix.maxJobs = 1; # i.e., there is only 1 CPU on this system

    boot.loader.grub = {
        device = "/dev/sda";
    };

    networking = {
        hostName = "luxion"; # Define your hostname.
        interfaceMonitor.enable = true; # Watch for plugged cable.
        enableWLAN = true;  # Enables Wireless (hopefully... it doesn't work on luxion right now)
    };

    # Add file system entries for each partition that you want to see mounted
    # at boot time.  You can add filesystems which are not mounted at boot by
    # adding the noauto option.
    fileSystems = [
        {
            mountPoint = "/";
            device = "/dev/disk/by-label/sys";
            fsType = "ext4";
        }
        {
            mountPoint = "/home";
            device = "/dev/disk/by-label/home";
            fsType = "ext4";
        }
    ];

    swapDevices = [{ device = "/dev/sda1"; }];

    # add 'l' user (remember, this file is always read as if we're installing
    # the system from scratch! so that's why we _add_ a user here)
    users.extraUsers = [
        {
            name = "l";
            uid = 1000;
            group = "users";
            extraGroups = [ "wheel" "share" ];
            description = "Linus";
            home = "/home/l";
            # environmen.shellInit unfortunately only exists for bash, so this is broken if we don't start up bash first (which happens when X starts, but not when we login from virtual console, or ssh)
            #shell = pkgs.zsh + "/bin/zsh";
            shell = "/bin/sh"; # it's actually symlinked by NixOS to bash
        }
    ];
    users.extraGroups = [
        {
            name = "share";
            gid = 1001;
        }
    ];
}
