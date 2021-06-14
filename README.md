## Prerequisites

* Install GHC and Cabal via GHCup.
* Install nix.
* Install GHCJS 8.10.4 via nix:
```
nix-env --install \
    --file https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz \
    --attr pkgs-unstable.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc8104
```
