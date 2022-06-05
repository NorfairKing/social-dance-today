#!/usr/bin/env bash


cd salsa-party-web-server

killall salsa-party-web-server || true

export SALSA_PARTY_WEB_SERVER_SEND_EMAILS=False
export SALSA_PARTY_WEB_SERVER_SEND_ADDRESS=noreply@social-dance.today
export SALSA_PARTY_WEB_SERVER_SEARCH_CACHE_POPULATOR_ENABLED=False
export SALSA_PARTY_WEB_SERVER_SEARCH_CACHE_POPULATOR_PHASE=0
export SALSA_PARTY_WEB_SERVER_SEARCH_CACHE_POPULATOR_PERIOD=600
export SALSA_PARTY_WEB_SERVER_EXPLORE_CACHE_POPULATOR_ENABLED=False
export SALSA_PARTY_WEB_SERVER_EXPLORE_CACHE_POPULATOR_PHASE=0
export SALSA_PARTY_WEB_SERVER_EXPLORE_CACHE_POPULATOR_PERIOD=600
export SALSA_PARTY_WEB_SERVER_ORGANISER_REMINDER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_ORGANISER_REMINDER_PHASE=0
export SALSA_PARTY_WEB_SERVER_ORGANISER_REMINDER_PERIOD=10
export SALSA_PARTY_WEB_SERVER_PARTY_GARBAGE_COLLECTOR_ENABLED=False
export SALSA_PARTY_WEB_SERVER_PARTY_GARBAGE_COLLECTOR_PHASE=1
export SALSA_PARTY_WEB_SERVER_PARTY_GARBAGE_COLLECTOR_PERIOD=60
export SALSA_PARTY_WEB_SERVER_IMAGE_GARBAGE_COLLECTOR_ENABLED=False
export SALSA_PARTY_WEB_SERVER_IMAGE_GARBAGE_COLLECTOR_PHASE=0
export SALSA_PARTY_WEB_SERVER_IMAGE_GARBAGE_COLLECTOR_PERIOD=60
export SALSA_PARTY_WEB_SERVER_PARTY_SCHEDULER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_PARTY_SCHEDULER_PHASE=0
export SALSA_PARTY_WEB_SERVER_PARTY_SCHEDULER_PERIOD=10

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
export SALSA_PARTY_WEB_SERVER_DANCEFLOORFINDER_COM_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_DANCEFLOORFINDER_COM_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_DANCEFLOORFINDER_COM_IMPORTER_PERIOD=1
export SALSA_PARTY_WEB_SERVER_SENSUAL_DANCE_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_SENSUAL_DANCE_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_SENSUAL_DANCE_IMPORTER_PERIOD=1
export SALSA_PARTY_WEB_SERVER_SALSA_BE_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_SALSA_BE_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_SALSA_BE_IMPORTER_PERIOD=10
export SALSA_PARTY_WEB_SERVER_LATINWORLD_NL_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_LATINWORLD_NL_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_LATINWORLD_NL_IMPORTER_PERIOD=10
export SALSA_PARTY_WEB_SERVER_TANZAGENDA_CH_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_TANZAGENDA_CH_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_TANZAGENDA_CH_IMPORTER_PERIOD=10
export SALSA_PARTY_WEB_SERVER_STAYHAPPENING_COM_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_STAYHAPPENING_COM_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_STAYHAPPENING_COM_IMPORTER_PERIOD=10
export SALSA_PARTY_WEB_SERVER_LONDONSALSAEVENTS_COM_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_LONDONSALSAEVENTS_COM_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_LONDONSALSAEVENTS_COM_IMPORTER_PERIOD=10
export SALSA_PARTY_WEB_SERVER_SALSALOVERS_BE_IMPORTER_ENABLED=False
export SALSA_PARTY_WEB_SERVER_SALSALOVERS_BE_IMPORTER_PHASE=0
export SALSA_PARTY_WEB_SERVER_SALSALOVERS_BE_IMPORTER_PERIOD=10

export SALSA_PARTY_WEB_SERVER_SENTRY_RELEASE=manual


export SALSA_PARTY_WEB_SERVER_LOG_LEVEL=Debug
export SALSA_PARTY_WEB_SERVER_ADMIN=syd@cs-syd.eu
export SALSA_PARTY_WEB_SERVER_STATIC_DIR=static
salsa-party-web-server &
