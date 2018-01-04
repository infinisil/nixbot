{ config, ... }:

{
  systemd.services.nixbot = {
    description = "Nix bot";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${(import ./. {}).nixbot}/bin/nixbot";
      Restart = "on-failure";
      RestartSec = 1;
    };
  };
}
