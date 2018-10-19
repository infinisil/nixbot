with import ./. {};

hpkgs.shellFor {
  packages = p: with hpkgs; [ nixbot nix-session ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
