{ lib, options, config, ... }:

with lib;

let

  # Takes a recursive set of options and returns all their values with a mkDefault
  takeValuesAsDefault = opts: mapAttrs (name: value:
    if isOption value
    then mkDefault value.value
    else takeValuesAsDefault value)
    (filterAttrs (name: value: ! hasPrefix "_" name) opts);

  channelOptions = {
    pr = {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the PR plugin.";
      };

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

    commands = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the command plugin.";
      };
      randomPr.token = mkOption {
        type = types.str;
        description = ''
          GitHub API token from https://help.github.com/en/github/authenticating-to-github/creating-a-personal-access-token-for-the-command-line
        '';
      };
    };

    nixrepl = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the nixrepl plugin.";
      };

      nixPath = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "The NIX_PATH to use for nix evaluation.";
      };
    };

    leaked.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the leaked plugin.";
    };

    karma = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the karma plugin.";
      };

      blacklist = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Blacklisted karma receivers";
      };
    };

    unreg.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the unreg plugin.";
    };
  };

in

{
  options = {

    host = mkOption {
      type = types.str;
      description = "AMQP host";
    };

    port = mkOption {
      type = types.port;
      description = "AMQP host port";
    };

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

    channels = mkOption {
      type = types.attrsOf (types.submodule [
        { options = channelOptions; }
        (takeValuesAsDefault options.channelDefaults)
      ]);
      default = {};
      description = "Channel-specific plugin configuration.";
    };

    channelDefaults = channelOptions;

    users = channelOptions;

    debugMode = mkOption {
      type = types.bool;
      default = false;
      description = "Enable debug mode: Only accepts messages in #bottest";
    };

  };
}
