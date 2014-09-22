# Usage
#
#     $ nix-build
#     $ ./result/bin/load-env-nanompd-dev

let
  pkgs = import <nixpkgs>{};
  haskell = pkgs.haskellPackages_ghc783;
in
pkgs.myEnvFun {
  name = "nanompd-dev";
  buildInputs = [
    (haskell.ghcWithPackages(self: with self; [
      cabalInstall attoparsec either exceptions mtl
      network text unorderedContainers
      hspec hspecExpectations QuickCheck
      criterion
    ]))
  ];
}
