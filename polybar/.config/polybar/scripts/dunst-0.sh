#!/usr/bin/env bash

if ! notifs=$(dunstctl count history); then
    notifs=0
else
    # Ensure the count is valid (in case dunstctl returns something unexpected)
    notifs="${notifs//[^0-9]/}"
fi

if [ "$notifs" -gt 0 ]; then
    echo "$notifs"
else
    echo ""
fi
