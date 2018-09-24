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
          description = "Fixed definitions";
        };

      };
    })
    config
  ];
}).config [ "_module" ]
