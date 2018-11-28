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
    };

    stateDir = mkOption {
      type = types.path;
      description = "State dir";
      default = "/var/lib/nixbot/state";
    };

    nixPath' = mkOption {
      type = types.listOf types.str;
      description = "NIX_PATH";
    };

  };
}
