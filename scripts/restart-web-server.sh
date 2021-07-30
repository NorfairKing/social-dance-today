#!/usr/bin/env bash


cd salsa-party-web-server

killall salsa-party-web-server || true


export SALSA_PARTY_WEB_SERVER_SEND_EMAILS=False
export SALSA_PARTY_WEB_SERVER_IMAGE_GARBAGE_COLLECTOR_ENABLED=False
export SALSA_PARTY_WEB_SERVER_IMAGE_GARBAGE_COLLECTOR_PHASE=0
export SALSA_PARTY_WEB_SERVER_IMAGE_GARBAGE_COLLECTOR_PERIOD=60
export SALSA_PARTY_WEB_SERVER_ORGANISER_REMINDER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_ORGANISER_REMINDER_PHASE=0
export SALSA_PARTY_WEB_SERVER_ORGANISER_REMINDER_PERIOD=10

export SALSA_PARTY_WEB_SERVER_IMPORTER_INTERVAL=1
export SALSA_PARTY_WEB_SERVER_EVENTS_INFO_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_EVENTS_INFO_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_EVENTS_INFO_IMPORTER_PERIOD=60
export SALSA_PARTY_WEB_SERVER_GOLATINDANCE_COM_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_GOLATINDANCE_COM_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_GOLATINDANCE_COM_IMPORTER_PERIOD=1
export SALSA_PARTY_WEB_SERVER_DANCEPLACE_COM_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_DANCEPLACE_COM_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_DANCEPLACE_COM_IMPORTER_PERIOD=1
export SALSA_PARTY_WEB_SERVER_MAPDANCE_COM_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_MAPDANCE_COM_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_MAPDANCE_COM_IMPORTER_PERIOD=1
export SALSA_PARTY_WEB_SERVER_SALSACHICAGO_COM_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_SALSACHICAGO_COM_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_SALSACHICAGO_COM_IMPORTER_PERIOD=1

export SALSA_PARTY_WEB_SERVER_SENTRY_RELEASE=manual


export SALSA_PARTY_WEB_SERVER_LOG_LEVEL=LevelDebug
export SALSA_PARTY_WEB_SERVER_ADMIN=syd@cs-syd.eu
salsa-party-web-server &
