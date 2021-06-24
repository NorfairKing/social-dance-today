#!/usr/bin/env bash


cd salsa-party-web-server

killall salsa-party-web-server || true


export SALSA_PARTY_WEB_SERVER_EVENTS_INFO_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_LOG_LEVEL=LevelDebug
export SALSA_PARTY_WEB_SERVER_ADMIN=syd@cs-syd.eu
salsa-party-web-server &
