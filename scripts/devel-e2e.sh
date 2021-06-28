#!/usr/bin/env bash

export DEVELOPMENT=True

stack install salsa-party-web-server \
  --file-watch --watch-all \
  --no-nix-pure \
  --exec="./scripts/restart-e2e.sh $@"

