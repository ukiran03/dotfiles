#!/usr/bin/env bash

# 1. Kill existing bar instances
pkill xmobar

# 2. Restart XMonad
# We use & so the script continues even if xmonad takes a moment
# xmonad --recompile &&
xmonad --restart

# 3. Refresh peripherals/status indicators
# changevolume > /dev/null 2>&1
# uk-bright > /dev/null 2>&1
laptop-brightness.sh > /dev/null 2>&1
