{ config, pkgs, ... }:
let
  kubeMasterIP = "192.168.0.4";
  kubeMasterHostname = "k0";
  kubeMasterAPIServerPort = 6443;
in
{
  services.kubernetes = let
    api = "https://${kubeMasterHostname}:${builtins.toString kubeMasterAPIServerPort}";
  in
  {
    roles = ["node"];
    masterAddress = kubeMasterHostname;
    apiserverAddress = api;
    apiserver = {
      securePort = kubeMasterAPIServerPort;
      advertiseAddress = kubeMasterIP;
    };

    addons.dns.enable = true;

    kubelet.extraOpts = "--fail-swap-on=false";

    # point kubelet and other services to kube-apiserver
    kubelet.kubeconfig.server = api;
  };
}
