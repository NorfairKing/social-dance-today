#!/usr/bin/env bash

export DEVELOPMENT=True

stack install salsa-party-web-server salsa-party-web-server-e2e \
  --file-watch --watch-all \
  --fast \
  --no-nix-pure \
  --ghc-options="-freverse-errors -j4 +RTS -A128M -n2m -RTS" \
  --exec="./scripts/restart-e2e.sh $@"

