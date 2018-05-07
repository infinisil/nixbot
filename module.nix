{ lib, config, pkgs, ... }:

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
    script = ''
      export PATH="${lib.makeBinPath (with pkgs; [ gnutar gzip ])}:$PATH"
      ${(import ./stack2nix.nix { inherit pkgs; }).nixbot}/bin/nixbot ${./release.nix}
    '';
    serviceConfig = {
      User = "nixbot";
      Restart = "on-failure";
      RestartSec = 1;
      MemoryMax = "200M";
      CPUQuota = "50%";
    };
  };
}
