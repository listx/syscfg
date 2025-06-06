{ config, pkgs, ... }:

{
  system.stateVersion = "23.11";
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    # 10.0.0.1 router
    # 10.0.0.2 printer
    extraHosts = ''
      10.0.0.3 m0
      10.0.0.4 k0
      10.0.0.5 k1
      10.0.0.6 w0
      10.0.0.7 macp
    '';
    # Port 22 is opened automatically if SSH daemon is enabled (no need to specify it here).
    firewall.allowedTCPPorts = [
      8010 # org-roam-server-mode
      8020 # stack exec -- blog watch --host=10.0.0.4 --port=8020
      8030 # mpd HTTP stream
    ];
  };

  # allow installation of 'ati_unfree' video driver and also Firefox with Flash
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  # Avoid "lacks a valid signature" error from using nix-copy-closures from
  # another machine on the local LAN. See
  # https://github.com/NixOS/nix/issues/2330#issuecomment-451650296.
  nix.settings.trusted-users = [ "root" "@wheel" ];

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

  i18n.inputMethod.enabled = "uim";

  # Fonts
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      baekmuk-ttf
      corefonts
      dejavu_fonts
      ipafont
      libertine
      source-code-pro
      source-sans
      source-serif
      source-han-sans
      source-han-serif
      source-han-mono
      terminus_font
      ubuntu_font_family
    ];
  };

  # Prettify the virtual console font early on with Terminus.
  console = {
    font = "ter-114n";
    packages = with pkgs; [ terminus_font ];
    earlySetup = true;
  };

  services.openvpn.servers = {
    # Unless `autoStart = false;', all entries here start automatically as a
    # systemd service. To stop the `home' OpenVPN client service, run `sudo
    # systemctl stop openvpn-home'.
    home = { config = builtins.readFile ../openvpn/home.ovpn; };
  };

  # PipeWire
  # rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  users.groups.l = { gid = 1000; };

  # Set password for extra users with 'passwd' command as root.
  users.users.l = {
    isNormalUser = true;
    group = "l";
    description = "Linus Arver";
    createHome = true;
    home = "/home/l";
    extraGroups = [ "wheel" ];
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };
}
