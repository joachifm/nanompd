# A Nix expression that defines a working environment for our library.
#
# Run `nix-build` to build the library and `nix-shell` to enter
# the environment (with `cabal-install` provided for convenience).
#
# The expression is parameterised over the set of Haskell packages
# to build against.
# By default, build against the latest GHC with profiling enabled.
#
# The body comprises a call to `cabal.mkDerivation`, which is passed a
# recursive (in this case) set of attributes that affect the cabal
# build process.
# The set is made recursive so that we can refer to `buildDepends` in
# the definition of `testDepends`.

{ haskellPackages ? (import <nixpkgs>{}).haskellPackages_ghc783_profiling }:
with haskellPackages; cabal.mkDerivation (self: rec {
  pname = "mpd";
  version = "0.0.0.0";
  src = ./.;
  propagatedBuildDepends = [ cabalInstall ];
  buildDepends = [
    cabalInstall deepseq mtl network text unorderedContainers
  ];
  testDepends = buildDepends;
  doCheck = false;
})
