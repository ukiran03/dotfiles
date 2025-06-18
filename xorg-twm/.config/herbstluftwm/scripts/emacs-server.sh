#!/usr/bin/env bash

hc() {
    herbstclient "$@"
}
# https://alessandroferrari.live/minimize-windows-bspwm/
hc rule title='emacs-server' tag=9 focus=false

# Define the hide function
hide() {
    # Get the client ID of the emacs server
    id=$(hc list_clients | grep "emacs-server" | awk '{print $1}')

    # Set the client to minimized
    if [[ -n "$id" ]]; then
        hc set_attr clients.$id.minimized true
    else
        echo "Emacs server not found."
    fi
}

# Start the emacs server in the background and then call hide
hc spawn emacs --title=emacs-server && hide
