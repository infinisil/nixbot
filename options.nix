{ cfg ? {} }:

with import <nixpkgs/lib>;

builtins.removeAttrs (evalModules {
  modules = [
    ({ config, lib, ... }: {
      options = {

        password = mkOption {
          type = types.str;
          description = "Password";
        };

        name = mkOption {
          type = types.str;
          description = "Name";
          default = "ircbot-infinisil";
        };

        stateDir = mkOption {
          type = types.path;
          description = "State dir";
          default = "/var/lib/nixbot/state";
        };

      };
    })
    cfg
  ];
}).config [ "_module" ]
