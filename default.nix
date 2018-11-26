{ pkgs ? import <nixpkgs> {}}:

with pkgs;

let
  hsPkgs = pkgs.haskell.packages.ghc822.override ({
    overrides = self: super: {
      circlehs = self.callPackage ./circlehs.nix { inherit fetchFromGitHub; };
      ghc-ci = super.callPackage ./ghc-ci.nix {};
    };
  });
in hsPkgs.ghc-ci
