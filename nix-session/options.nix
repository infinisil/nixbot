{ config }:

with import <nixpkgs/lib>;

builtins.removeAttrs (evalModules {
  modules = [
    ({ config, lib, ... }: {
      options = {

        selfName = mkOption {
          type = types.str;
          default = "self";
          description = "Name for self argument";
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
    })
    config
  ];
}).config [ "_module" ]
