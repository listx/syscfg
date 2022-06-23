{ config, pkgs, ... }:

{
  imports = [ ./minimal.nix ];

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "terminate:ctrl_alt_bksp";
    displayManager.defaultSession = "l_xmonad";
    # See https://unix.stackexchange.com/questions/597358/nixos-how-to-configure-custom-desktop-session/597359#597359.
    displayManager.session = [{
      manage = "desktop";
      name = "l_xmonad";
      start = "exec $HOME/.xsession";
    }];
    displayManager = {
      autoLogin.enable = true;
      autoLogin.user = "l";
    };
    # We rely on ~/.xsession to start XMonad, instead of NixOS automagically
    # doing it for us. This way, we can use our xmonad binary compiled by Stack.
    windowManager.xmonad.enable = false;
    windowManager.xmonad.enableContribAndExtras = false;
  };

  virtualisation.virtualbox.host.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "devicemapper";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  users.extraUsers.l.extraGroups = [ "wheel" "docker" "vboxusers" ];
}
