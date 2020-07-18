#!/bin/sh

cd xenopy && python xenopy/xenobot.py "$@" || echo "run error code: $?"
