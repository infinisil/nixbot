let
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ca2ba44cab47767c8127d1c8633e2b581644eb8f.tar.gz";
    sha256 = "1jg7g6cfpw8qvma0y19kwyp549k1qyf11a5sg6hvn6awvmkny47v";
  };

  nixpkgs = (import nixpkgsSrc {}).srcOnly {
    name = "nixpkgs-patched";
    src = nixpkgsSrc;
    patches = [
      # https://github.com/NixOS/nixpkgs/pull/46453
      (builtins.fetchurl {
        url = "https://github.com/NixOS/nixpkgs/commit/e6dd03d554e65badd9cdc8b9c137a5998a642e42.patch";
        sha256 = "0aisra3arv6x6z59dfw4bfxyj40pm6liixgkwpj1rjrr0ql4yc9s";
      })
    ];
  };
in
{ pkgs ? import nixpkgs {}
}:

(pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  nixbot = fetchGit ./.;
  nix-session = ./nix-session;
})).extend (self: super: {
  hnix = pkgs.haskell.lib.overrideSrc super.hnix {
    # https://github.com/haskell-nix/hnix/pull/363
    src = fetchTarball {
      url = "https://github.com/infinisil/hnix/archive/c723e73a415fa68d12baac88140d685424adac4f.tar.gz";
      sha256 = "15fnylxm29izxvf7mf7f4y60spi2hgs72lkf53an8wk7is5v434f";
    };
  };
}) // { inherit pkgs; }
