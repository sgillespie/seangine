{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8107"
}:

(import ./default.nix {
  inherit nixpkgs compiler;
}).env
