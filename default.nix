{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/0a7e258012b60cbe530a756f09a4f2516786d370.tar.gz";
    sha256 = "1qcnxkqkw7bffyc17mqifcwjfqwbvn0vs0xgxnjvh9w0ssl2s036";
  }) {}
}:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = (pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
    nixbot = ./.;
    nix-session = ./nix-session;
  })).extend (self: super: {
    hnix = pkgs.haskell.lib.overrideSrc super.hnix {
      # https://github.com/haskell-nix/hnix/pull/363
      src = fetchTarball {
        url = "https://github.com/infinisil/hnix/archive/c723e73a415fa68d12baac88140d685424adac4f.tar.gz";
        sha256 = "15fnylxm29izxvf7mf7f4y60spi2hgs72lkf53an8wk7is5v434f";
      };
    };
  });
in hpkgs.nixbot // {
  inherit hpkgs pkgs;
}
