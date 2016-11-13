{ config, pkgs, ... }:

{
  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    extraHosts = ''
      192.168.0.100 m0
      192.168.0.110 k0
      192.168.0.112 k1
      74.207.246.114 l0
    '';
    # Port 22 is opened automatically if SSH daemon is enabled (no need to specify it here).
    # 8001 is for the nix-serve HTTP daemon.
    firewall.allowedTCPPorts = [ 8001 ];
  };

  # allow installation of 'ati_unfree' video driver and also Firefox with Flash
  nixpkgs.config.allowUnfree = true;

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
