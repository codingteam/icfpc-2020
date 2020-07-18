#!/bin/sh

python xenopy/xenobot.py "$@" || echo "run error code: $?"
