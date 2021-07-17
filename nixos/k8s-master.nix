{ config, pkgs, ... }:
let
  kubeMasterIP = "192.168.0.4";
  kubeMasterHostname = "k0";
  kubeMasterAPIServerPort = 6443;
in
{
  services.kubernetes = {
    roles = ["master" "node"];
    masterAddress = kubeMasterHostname;
    apiserverAddress = "https://${kubeMasterHostname}:${builtins.toString kubeMasterAPIServerPort}";
    apiserver = {
      securePort = kubeMasterAPIServerPort;
      advertiseAddress = kubeMasterIP;
    };

    addons.dns.enable = true;

    # Allow usage of swap (by default, Kubernetes nodes fail if swap is turned
    # on, because it hampers predictable scheduling of pods when resources are
    # constrained --- but we are not running heavy tasks in heavy load, so this
    # default setting is OK to ignore).
    kubelet.extraOpts = "--fail-swap-on=false";
  };
}
