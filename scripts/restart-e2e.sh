#!/usr/bin/env bash


cd salsa-party-web-server

killall salsa-party-web-server || true


export SALSA_PARTY_WEB_SERVER_EVENTS_INFO_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_LOG_LEVEL=LevelDebug
salsa-party-web-server &

export SALSA_PARTY_SERVER_URL=http://localhost:8000
salsa-party-web-server-e2e
