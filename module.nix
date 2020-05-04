{ lib, config, pkgs, ... }:

with lib;

let

  cfg = config.services.nixbot;

  filteredConfig = filterAttrsRecursive (name: value: name != "_module") cfg.config;
  configFile = pkgs.writeText "nixbot-config.json" (builtins.toJSON filteredConfig);

  nixbot = import ./default.nix (optionalAttrs (! cfg.pinned) { inherit pkgs; });

  dataDir = "/var/lib/nixbot";

  defaultNixPath = [
    "nixpkgs=${dataDir}/nixpkgs/master/repo"
    "nixos-config=${pkgs.writeText "configuration.nix" ''
      {
        boot.loader.grub.device = "nodev";
        fileSystems."/".device = "/dev/sda1";
      }
    ''}"
  ] ++ map (channel: "${channel}=${dataDir}/nixpkgs/${channel}/repo") cfg.channels;

in

{

  options.services.nixbot = {

    enable = mkEnableOption "Nixbot";

    pinned = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to use the pinned nixpkgs for nixbot. Enabling this guarantees
        that there will be no build problems, but dependencies can't be
        shared with the rest of the system.
      '';
    };

    channels = mkOption {
      type = types.listOf types.str;
      default = [ "nixpkgs-unstable" ];
      description = ''
        Channels which should be added to NIX_PATH, tracking
        https://github.com/NixOS/nixpkgs-channels.
      '';
    };

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

    services.nixbot.config.users.nixrepl.nixPath = defaultNixPath;
    services.nixbot.config.channelDefaults.nixrepl.nixPath = defaultNixPath;

    users.users.nixbot = {
      description = "User for nixbot";
      home = "${dataDir}";
      createHome = true;
      group = "nixbot";
    };
    users.groups.nixbot = {};

    systemd.timers.nixbot-master-updater = {
      wantedBy = [ "timers.target" ];
      timerConfig.OnUnitInactiveSec = 60;
    };

    systemd.timers.nixbot-channel-updater = {
      wantedBy = [ "timers.target" ];
      timerConfig.OnUnitInactiveSec = 60;
    };

    systemd.services.nixbot-master-updater = {
      description = "Nix bot master updater";
      path = [ pkgs.git ];
      script = ''
        git -C repo config gc.autoDetach false
        if [ -d repo ]; then
          git -C repo fetch
          old=$(git -C repo rev-parse @)
          new=$(git -C repo rev-parse @{u})
          if [ $old != $new ]; then
            git -C repo rebase --autostash
            echo "Updated from $old to $new"
          fi
        else
          git clone https://github.com/NixOS/nixpkgs repo
          git -C repo remote add channels https://github.com/NixOS/nixpkgs-channels
          echo "Initialized at $(git -C repo rev-parse @)"
        fi
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "nixbot";
        WorkingDirectory = "${dataDir}/nixpkgs/master";
      };
    };

    systemd.services.nixbot-channel-updater = {
      description = "Nix bot channel updater";
      path = [ pkgs.git ];
      after = [ "nixbot-master-updater.service" ];
      script = ''
        git -C master/repo worktree prune
        git -C master/repo fetch channels
        ${flip (concatMapStringsSep "\n") cfg.channels (channel: ''
          if [ -d ${channel}/repo ]; then
            old=$(git -C ${channel}/repo rev-parse @)
            new=$(git -C ${channel}/repo rev-parse @{u})
            if [ $old != $new ]; then
              git -C ${channel}/repo rebase --autostash
              echo "Updated ${channel} from $old to $new"
            fi
          else
            git -C master/repo branch -D ${channel} || true
            git -C master/repo worktree add -B ${channel} $PWD/${channel}/repo remotes/channels/${channel}
            echo "Initialized ${channel} at $(git -C ${channel}/repo rev-parse @)"
          fi
        '')}
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "nixbot";
        WorkingDirectory = "${dataDir}/nixpkgs";
      };
    };

    systemd.services.nixbot = {
      description = "Nix bot";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      wants = [ "nixbot-master-updater.service" "nixbot-channel-updater.service" ];
      path = [ pkgs.nix-index ];
      unitConfig.StartLimitIntervalSec = 0;
      serviceConfig = {
        User = "nixbot";
        Group = "nixbot";
        ExecStart = "${nixbot}/bin/nixbot ${cfg.configFile}";
        Restart = "always";
        RestartSec = 1;
        MemoryMax = "100M";
        CPUQuota = "50%";
        WorkingDirectory = "${dataDir}/state/nixpkgs";
      };
    };

    services.nginx = {
      enable = true;
      virtualHosts."nixbot.${config.networking.domain}" = {
        root = "${dataDir}/state/new";
        locations."/global/commands/".extraConfig = ''
          autoindex on;
          # https://stackoverflow.com/q/28166131/6605742
          index "you cant assign this";

          location ~ /global/commands/commands/.+$ {
            add_header Access-Control-Allow-Origin "*";
            add_header Access-Control-Request-Method "GET";
            add_header Content-Security-Policy "default-src 'none'; sandbox;";
            add_header Content-Type "text/plain; charset=utf-8";
            add_header X-Content-Type-Options "nosniff";
            add_header X-Frame-Options "deny";
            add_header X-XSS-Protection "1; mode=block";
          }
        '';
      };
    };

    users.users.nginx.extraGroups = [ "nixbot" ];

  };
}
