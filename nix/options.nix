{ lib, ... }:

with lib;

{
  options = {

    password = mkOption {
      type = types.str;
      description = "Password";
    };

    user = mkOption {
      type = types.str;
      description = "Name";
      default = "ircbot-infinisil";
    };

    stateDir = mkOption {
      type = types.path;
      description = "State dir";
      default = "/var/lib/nixbot/state";
    };

    argsPath = mkOption {
      type = types.path;
      description = "Arguments to nixpkgs import";
      default = builtins.toFile "nixbot-args" ''
        {
          config = {};
          overlays = [];
        }
      '';
    };

  };
}
