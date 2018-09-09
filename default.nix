{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ca2ba44cab47767c8127d1c8633e2b581644eb8f.tar.gz";
    sha256 = "1jg7g6cfpw8qvma0y19kwyp549k1qyf11a5sg6hvn6awvmkny47v";
  }) {}
}:
pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  nixbot = ./.;
  nix-session = ./nix-session;
})
