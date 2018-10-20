{ lib, ... }:

with lib;

# TODO: Add examples and descriptions
{

  options = {

    primarySessionFile = mkOption {
      type = types.path;
      default = toString ~/.local/share/nix-session/session;
      description = "Primary session";
    };

    secondarySessionFiles = mkOption {
      type = types.attrsOf types.path;
      default = {};
    };

    nixPath = mkOption {
      type = types.nullOr (types.listOf types.str);
      default = null;
    };

    nixOptions = mkOption {
      type = types.attrsOf types.str;
      default = {};
    };

    sessionDefaults = mkOption {
      type = types.submodule {
        options = {

          selfName = mkOption {
            type = types.str;
            default = "self";
            description = "Name for self argument";
          };

          metaName = mkOption {
            type = types.str;
            default = "meta";
          };

          fixedDefs = mkOption {
            type = types.attrsOf types.str;
            default = {};
            description = ''
              Fixed definitions, overriding all dynamically defined ones.
              Note: While this is an attrset and therefore doesn't have a fixed
              order, this doesn't matter, as the `let in` statement to be used for
              these fixed definitions works like a rec set.
            '';
          };

        };
      };
      default = {};
    };

  };
}
