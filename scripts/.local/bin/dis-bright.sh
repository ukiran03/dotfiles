#!/usr/bin/env bash

if [[ -f /home/ukiran/.config/my_display/brightness ]]; then
    awk '{print $1 * 100}' /home/ukiran/.config/my_display/brightness
else
    echo ""
fi
