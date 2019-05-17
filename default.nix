{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/d73f16d6767e99675682f822dac3017bf9af1e83.tar.gz";
    sha256 = "1b5wix9kr5s3hscpl425si0zw00zzijc9xrcph6l2myh4n5nvcm0";
  }) {}
}:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc864.extend (self: super: {
    nixbot = (self.callCabal2nix "nixbot" (lib.sourceByRegex ./. [
      "^src.*$"
      "^.*\\.cabal$"
      "^LICENSE$"
      "^nix.*$"
    ]) {}).overrideAttrs (drv: {
      nativeBuildInputs = drv.nativeBuildInputs or [] ++ [ pkgs.makeWrapper ];
      postInstall = drv.postInstall or "" + ''
        wrapProgram $out/bin/nixbot \
          --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.gnutar pkgs.gzip ]}"
      '';
    });
  });
in hpkgs.nixbot // {
  inherit hpkgs pkgs;
}
