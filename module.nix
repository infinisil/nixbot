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

    services.nixbot.config.nixpkgsPath = "/var/lib/nixbot/nixpkgs/master/repo";

    users.users.nixbot = {
      description = "User for nixbot";
      home = "/var/lib/nixbot";
      createHome = true;
      group = "users";
    };

    systemd.timers.nixbot-master-updater = {
      wantedBy = [ "timers.target" ];
      timerConfig.OnUnitInactiveSec = 60;
    };

    systemd.services.nixbot-master-updater = {
      description = "Nix bot master updater";
      path = [ pkgs.git ];
      script = ''
        if [ -d nixpkgs/master/repo ]; then
          git -C nixpkgs/master/repo pull --rebase --autostash origin master
        else
          mkdir -p nixpkgs/master
          git clone https://github.com/NixOS/nixpkgs nixpkgs/master/repo
        fi
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "nixbot";
        WorkingDirectory = "/var/lib/nixbot";
      };
    };

    systemd.services.nixbot = {
      description = "Nix bot";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      requires = [ "nixbot-master-updater.service" ];
      path = [ pkgs.nix-index ];
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
