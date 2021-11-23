#!/bin/bash
# shellcheck disable=SC2164
cd /opt/app
echo Stopping application
source ./shutdown.sh
# sleep 2
echo Starting application
source ./start.sh
