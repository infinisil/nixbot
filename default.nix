{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
}:

let
  nixbot = (import ./stack2nix.nix { inherit pkgs; }).nixbot;

  src = builtins.filterSource (absName: type: let
    name = lib.removePrefix "${toString ./.}/" absName;
  in ! (lib.hasSuffix ".stack-work" name
    || lib.hasSuffix ".git" name
    || name == "state"
    || name == "result"
    || (lib.hasSuffix ".nix" name && ! lib.hasPrefix "hnix" name && ! (name == "options.nix")))
  ) ./.;

in
  nixbot.overrideAttrs (old: {
    inherit src;
  })
