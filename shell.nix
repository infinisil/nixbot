with import ./. {};

hpkgs.shellFor {
  packages = p: with hpkgs; [ nixbot ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
