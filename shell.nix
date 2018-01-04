with import <nixpkgs> {};

haskell.lib.buildStackProject {
  name = "nixbot";
  src = lib.cleanSource ./.;
  buildInputs = [ zlib ];
}
