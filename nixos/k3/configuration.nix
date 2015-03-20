# See configuration.nix(5) manpage.
{ config, pkgs, ... }:

{
  imports =
    [
      ../base.nix
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/disk/by-id/ata-ST9500420AS_5VJ59T8M";
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/1414ec8e-89fb-476a-9796-13b158fa1019";
      preLVM = true;
    }
  ];
  boot.loader.grub.extraEntries = ''
    menuentry "Windows7" {
      insmod ntfs
      set root='(hd0,2)'
      chainloader +1
    }
  '';

  # KNOWN ISSUES
  #
  # - If we suspend with `systemctl suspend`, and then resume, the wireless
  # connection won't work. The command `ping www.google.com` won't work. To fix
  # this, manually invoke `systemctl restart network-addresses-wlp3s0.service`.
  networking = {
    hostName = "k3";
    hostId = "518ab295";
    wireless.enable = true;
    interfaces = {
      wlp3s0 = {
        ipAddress = "192.168.1.116";
        prefixLength = 24;
      };
    };
    useDHCP = false;
    defaultGateway = "192.168.1.254";
    nameservers = [ "192.168.1.254" ];
  };

  services.xserver = {
    videoDrivers = [ "ati" ];
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };
    config = ''
      Section "InputClass"
        Identifier "touchpad catchall"
        Driver "synaptics"
        MatchIsTouchpad "on"
        MatchDevicePath "/dev/input/event*"
        Option "TapButton1" "0"
        Option "TapButton2" "0"
        Option "TapButton3" "0"
      EndSection
    '';
  };

}
#device                          fs_type         label            mount point         UUID
#-------------------------------------------------------------------------------------------------------------------------
#/dev/root                       iso9660         NIXOS_ISO        /iso                2015-01-13-11-30-46-00
#/dev/loop0                      squashfs                         /nix/.ro-store
#/dev/sda1                       vfat            RECOVERY         (not mounted)       3C98-AC5D
#/dev/sda2                       ntfs            OS               (not mounted)       5EF0D2BAF0D2981B
#/dev/sda3                       ext2            boot             /mnt/boot           184c55ec-db57-4a37-b997-72ec57f89a0f
#/dev/sda4                       crypto_LUKS                      (in use)            1414ec8e-89fb-476a-9796-13b158fa1019
#/dev/sdb1                       ext4                             (not mounted)       66d852d9-b62d-43aa-9897-b8a8243c9a12
#/dev/sdb2                       crypto_LUKS                      (not mounted)       b27b5ca9-8104-4425-8a8e-95693ce4d1a1
#/dev/mapper/luks                LVM2_member                      (in use)            dpGfxA-J5Bu-Ejl1-aO2G-G0Vm-f4cs-UKwhav
#/dev/mapper/vg0-root            ext4                             /mnt                e30c7293-51b1-4786-b6e7-adcb8492caf9
#/dev/mapper/vg0-swap            swap                             <swap>              e98031b6-649f-41a3-8ae5-40041855b0e3
#/dev/mapper/vg0-home            ext4                             /mnt/home           876ddf6a-1c9c-42e2-b4c4-bebf27b3261e
