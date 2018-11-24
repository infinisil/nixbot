{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/80738ed9dc0ce48d7796baed5364eef8072c794d.tar.gz";
    sha256 = "0anmvr6b47gbbyl9v2fn86mfkcwgpbd5lf0yf3drgm8pbv57c1dc";
  }) {}
}:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskellPackages.extend (self: super: {
    nixbot = (self.callCabal2nix "nixbot" (lib.sourceByRegex ./. [
      "^src.*$"
      "^.*\\.cabal$"
      "^LICENSE$"
      "^options.nix$"
    ]) {}).overrideAttrs (drv: {
      nativeBuildInputs = drv.nativeBuildInputs or [] ++ [ pkgs.makeWrapper ];
      postInstall = drv.postInstall or "" + ''
        wrapProgram $out/bin/nixbot \
          --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.gnutar pkgs.gzip ]}"
      '';
    });
    nix-session = self.callCabal2nix "nix-session" (lib.sourceByRegex ./nix-session [
      "^app.*$"
      "^src.*$"
      "^nix.*$"
      "^.*\\.cabal$"
      "^LICENSE$"
    ]) {};

    hnix = import (pkgs.fetchgit {
      # https://github.com/haskell-nix/hnix/pull/416
      url = "https://github.com/infinisil/hnix";
      rev = "8ef09e6efa9ec7b5ec13519cac7241f0b3323df3";
      sha256 = "0wz5z4p5lvqjhsxamn3xk9daidga68g71vgkwd3w2pvajprvd4xf";
    }) {
      inherit pkgs;
      # Needed for some reason, not sure why
      returnShellEnv = false;
    };
  });
in hpkgs.nixbot // {
  inherit hpkgs pkgs;
}
