with import <nixpkgs/lib>;

{ config }: let

  result = evalModules {
    modules = [ ./options.nix config ];
  };

in filterAttrsRecursive (name: value: name != "_module") result.config
