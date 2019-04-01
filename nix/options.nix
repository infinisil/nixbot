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

    karmaBlacklist = mkOption {
      type = types.listOf types.str;
      description = "Blacklisted karma receivers";
    };

    plugins.pr = {

      ignoreStandaloneUnder = mkOption {
        type = types.ints.unsigned;
        default = 10;
        description = ''
          Ignore #<number> where number < ignoreStandaloneUnder. This is such that when
          people say things like `My #1 person is you`, nothing gets triggered.
          This does not apply to the <repo>#<number> or <owner>/<repo>#<number> syntax.
        '';
      };

      defaultRepo = mkOption {
        type = types.str;
        default = "nixpkgs";
        description = ''
          The repository that will be looked up when somebody uses #<number>.
          This then goes through `defaultOwners` to look up the owner for it,
          or uses fallbackOwner if there's no defaultOwner for this repo.
        '';
      };

      defaultOwners = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = ''
          Default owners for repositories. When somebody uses <repo>#<number>,
          this option will be used to look up the owner, or fallbackOwner if
          there's no definition here.
        '';
        example = literalExample ''
          {
            idris-dev = "idris";
            nixbot = "infinisil";
            home-manager = "rycee";
          }
        '';
      };

      fallbackOwner = mkOption {
        type = types.str;
        default = "NixOS";
        description = ''
          The default owner for repositories. If somebody uses #<number> or <repo>#<number>,
          this owner is used as a fallback if defaultOwners doesn't define one.
        '';
      };

    };

    debugMode = mkOption {
      type = types.bool;
      default = false;
      description = "Enable debug mode: Only accepts messages in #bottest";
    };

  };
}
