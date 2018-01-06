{ pkgs, config, lib, ... }:

{
  containers.nixbot = {
    config = {
      systemd.services.nixbot = {
        description = "Nix bot";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${(import ./. {}).nixbot}/bin/nixbot ${builtins.readFile ./auth}";
          User = "nixbot";
          Restart = "on-failure";
          RestartSec = 1;
          Environment="NIX_PATH=nixpkgs=${lib.cleanSource pkgs.path}";
        };
      };

      environment.systemPackages = with pkgs; [
        nix-repl
      ];

      nix = {
        extraOptions = ''
          build-timeout = 1
          timeout = 1
          max-silent-time = 1
          allow-import-from-derivation = false
        '';
        useSandbox = true;
        package = pkgs.nixUnstable;
        binaryCaches = [];
      };

      users.extraUsers.admin =
        { isNormalUser = true;
          description = "Admin account";
          extraGroups = [ "wheel" ];
          password = "foobar";
        };

      users.extraUsers.nixbot =
        { isNormalUser = true;
          description = "nixbot account";
        };

      security.sudo.wheelNeedsPassword = false;
    };


    autoStart = true;
  };
}
