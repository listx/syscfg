{ config, pkgs, ... }:

{
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    extraHosts = ''
      192.168.0.3 m0
      192.168.0.4 k0
      192.168.0.5 k1
      192.168.0.7 linusa-macbookpro
    '';
    # Port 22 is opened automatically if SSH daemon is enabled (no need to specify it here).
    firewall.allowedTCPPorts = [
      8000 # hledger -R web -- --serve --host=192.168.0.4 --port=8000
      8001 # hledger    web -- --serve --host=192.168.0.4 --port=8001
      8010 # org-roam-server-mode
      8020 # stack exec -- blog watch --host=192.168.0.4 --port=8020
    ];
  };

  # allow installation of 'ati_unfree' video driver and also Firefox with Flash
  nixpkgs.config.allowUnfree = true;

  # Avoid "lacks a valid signature" error from using nix-copy-closures from
  # another machine on the local LAN. See
  # https://github.com/NixOS/nix/issues/2330#issuecomment-451650296.
  nix.trustedUsers = [ "root" "@wheel" ];

  # Create a /etc/zshenv and other things to make Zsh work properly. Among
  # other things, this allows us to perform a "git pull <this machine's IP>"
  # from a remote machine; without this, the login shell cannot find the
  # git-upload-pack command, and the git pull operation will fail.
  programs.zsh.enable = true;

  # Delete things that come built-in by nix that customizes Zsh. For one thing,
  # disable the prompt settings because they interfere with our own
  # customizations.
  programs.zsh.promptInit = "";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # We're going to use `gpg-agent` with SSH support --- so to avoid conflict,
  # disable OpenSSH's ssh-agent.
  programs.ssh.startAgent = false;

  # NTP for automated system clock adjustments.
  services.ntp.enable = true;

  time.timeZone = "America/Los_Angeles";

  users.extraGroups.l = {
    gid = 1000;
  };

  # Set password for extra users with 'passwd' command as root.
  users.extraUsers.l = {
    group = "l";
    description = "Linus Arver";
    createHome = true;
    home = "/home/l";
    extraGroups = [ "wheel" ] ;
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };
}
