#!/bin/sh

cd xenopy && python xenobot.py "$@" || echo "run error code: $?"
