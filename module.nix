{ config, pkgs, ... }:

{

  users.users.nixbot = {
    description = "User for nixbot";
    home = "/var/lib/nixbot";
    createHome = true;
    group = "users";
  };

  systemd.services.nixbot = {
    description = "Nix bot";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      User = "nixbot";
      ExecStart = "${(import ./stack2nix.nix { inherit pkgs; }).nixbot}/bin/nixbot /var/lib/nixbot/state ${builtins.readFile ./auth}";
      Restart = "on-failure";
      RestartSec = 1;
    };
  };
}
