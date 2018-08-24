{ nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/dae9cf6106da19f79a39714f183ed253c62b32c5.tar.gz";
    sha256 = "0r3c00m96ldb9z81ay7vj8gnpk4bf8gjcdiad7mgxvwxr9ndskjx";
  }
, pkgs ? import nixpkgs {}
}:

let

  src = fetchGit ./.;

  hpkgs = pkgs.haskellPackages;

  nixbot = hpkgs.callCabal2nix "nixbot" src {};

  # Once developPackage allows the use of a source filter, this can be used instead
  nixbot' = hpkgs.developPackage {
    root = ./.;
  };

  shellDrv = nixbot.env.overrideAttrs (oldAttrs: {
    buildInputs = [ pkgs.cabal-install ];
  });

in
  if pkgs.lib.inNixShell then shellDrv else nixbot
