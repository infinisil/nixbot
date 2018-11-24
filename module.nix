{ lib, config, pkgs, ... }:

{

  users.users.nixbot = {
    description = "User for nixbot";
    home = "/var/lib/nixbot";
    createHome = true;
    group = "users";
  };

  environment.systemPackages = [ pkgs.nix-index ];

  systemd.services.nixbot = {
    description = "Nix bot";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      User = "nixbot";
      ExecStart = "${import ./default.nix {}}/bin/nixbot ${./release.nix}";
      Restart = "always";
      RestartSec = 1;
      MemoryMax = "100M";
      CPUQuota = "50%";
      WorkingDirectory = "/var/lib/nixbot/state/nixpkgs";
    };
  };
}
