{ nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/7db611f2af869bac6e31ba814a5593c52d54ec19.tar.gz";
    sha256 = "0yp97ayg3bbi2bm2sgvjhrrmc73hqpv4cymm7gb49mmqjwg5fzws";
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
    buildInputs = [ hpkgs.cabal-install ];
  });

in
  if pkgs.lib.inNixShell then shellDrv else nixbot
