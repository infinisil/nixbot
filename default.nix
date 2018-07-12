{ nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/dae9cf6106da19f79a39714f183ed253c62b32c5.tar.gz";
    sha256 = "0r3c00m96ldb9z81ay7vj8gnpk4bf8gjcdiad7mgxvwxr9ndskjx";
  }
, pkgs ? import nixpkgs {}
, lib ? pkgs.lib
}:

let

  hpkgs = (import ./stack2nix.nix { inherit pkgs; }).override {
    overrides = self: super: {
      hnix = pkgs.haskell.lib.overrideCabal super.hnix (oldAttrs: {
        libraryHaskellDepends = oldAttrs.libraryHaskellDepends ++ [
          self.compact
        ];
      });
    };
  };

  src = builtins.filterSource (absName: type: let
    name = lib.removePrefix "${toString ./.}/" absName;
  in ! (lib.hasSuffix ".stack-work" name
    || lib.hasSuffix ".git" name
    || name == "state"
    || name == "result"
    || (lib.hasSuffix ".nix" name && ! lib.hasPrefix "hnix" name && ! (name == "options.nix")))
  ) ./.;

  nixbot = hpkgs.nixbot.overrideAttrs (oldAttrs: {
    inherit src;
  });

in
  nixbot
