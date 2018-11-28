{ haskellPackages, fetchFromGitHub }:

let
  hsPkgs = haskellPackages.override ({
    overrides = self: super: {
      circlehs = self.callPackage ./circlehs.nix { inherit fetchFromGitHub; };
      ghc-ci = super.callPackage ./ghc-ci.nix {};
    };
  });
in hsPkgs.ghc-ci
