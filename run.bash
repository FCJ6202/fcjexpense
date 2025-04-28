#!/usr/bin/env bash

if [ "$1" = "build" ]; then
  nix-shell -p cabal-install ghc --run "cabal build"
elif [ "$1" = "run" ]; then
  nix-shell -p cabal-install ghc --run "cabal run"
else
  echo "Usage: $0 [build|run]"
fi