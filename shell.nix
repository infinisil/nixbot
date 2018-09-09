with import ./. {};

shellFor {
  packages = p: with p; [ nixbot nix-session ];
  withHoogle = true;
  nativeBuildInputs = [ cabal-install ];
}
