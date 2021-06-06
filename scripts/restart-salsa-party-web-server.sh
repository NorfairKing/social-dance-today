#!/usr/bin/env bash


killall salsa-party-web-server || true

salsa-party-web-server &
