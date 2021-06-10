#!/usr/bin/env bash


cd salsa-party-web-server

killall salsa-party-web-server || true


export SALSA_PARTY_WEB_SERVER_LOG_LEVEL=LevelDebug
salsa-party-web-server &
