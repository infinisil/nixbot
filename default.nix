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
  nixbot = ./.;
  nix-session = ./nix-session;
})).extend (self: super: {
  hnix = pkgs.haskell.lib.overrideSrc super.hnix {
    # https://github.com/haskell-nix/hnix/pull/360
    src = fetchTarball {
      url = "https://github.com/infinisil/hnix/archive/9a74e0cd99065533a878f6442b8391904a9b53b1.tar.gz";
      sha256 = "13qnc1fmizkayfck1ym0xl6d013z5097p1lifb40vc4awcmm3xg2";
    };
  };
}) // { inherit pkgs; }
