#!/usr/bin/env bash


cd salsa-party-web-server


killall salsa-party-web-server || true

salsa-party-web-server &
