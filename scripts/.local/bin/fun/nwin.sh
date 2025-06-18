#!/usr/bin/env bash

# Get the current workspace number
CURR_WS=$(wmctrl -d | grep '*' | awk '{print $1}')

# Count the number of windows in the current workspace
NWIN=$(wmctrl -l | awk -v curr_ws="$CURR_WS" '$2 == curr_ws {count++} END {print count}')

# Print the number of windows
echo $NWIN
