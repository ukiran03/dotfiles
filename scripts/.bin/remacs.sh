#!/bin/sh

# Kill all existing Emacs clients and daemons
pkill -u "$USER" -f "/usr/bin/emacs --daemon"

# Start a new Emacs daemon
/usr/bin/emacs --bg-daemon
