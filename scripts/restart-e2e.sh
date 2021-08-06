#!/usr/bin/env bash


cd salsa-party-web-server

killall salsa-party-web-server || true

PORT=8000

export SALSA_PARTY_WEB_SERVER_EVENTS_INFO_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_LOG_LEVEL=LevelDebug
export SALSA_PARTY_WEB_SERVER_STATIC_DIR=static
export SALSA_PARTY_WEB_SERVER_PORT="${PORT}"
salsa-party-web-server & 

sleep 0.5

export SALSA_PARTY_SERVER_URL="http://localhost:${PORT}"
salsa-party-web-server-e2e $@
