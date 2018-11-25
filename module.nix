{ lib, config, pkgs, ... }:

with lib;

let

  cfg = config.services.nixbot;

  filteredConfig = filterAttrsRecursive (name: value: name != "_module") cfg.config;
  configFile = pkgs.writeText "nixbot-config.json" (builtins.toJSON filteredConfig);

  nixbot = import ./default.nix {};

in

{

  options.services.nixbot = {

    enable = mkEnableOption "Nixbot";

    config = mkOption {
      type = types.submodule (import ./nix/options.nix);
      default = {};
      description = "Nixbot configuration";
    };

    configFile = mkOption {
      type = types.path;
      description = "JSON configuration file";
    };

  };

  config = mkIf cfg.enable {

    services.nixbot.configFile = mkDefault configFile;

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
        ExecStart = "${nixbot}/bin/nixbot ${cfg.configFile}";
        Restart = "always";
        RestartSec = 1;
        MemoryMax = "100M";
        CPUQuota = "50%";
        WorkingDirectory = "/var/lib/nixbot/state/nixpkgs";
      };
    };

  };
}
