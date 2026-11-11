#!/usr/bin/env bash
exec 2>&1

export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export GNUPGHOME="$HOME/.gnupg"

# Ensure gpg-agent is running and has an active socket
if ! gpg-connect-agent /bye >/dev/null 2>&1; then
    gpgconf --launch gpg-agent
fi

if ! error_log=$(mbsync -a 2>&1); then
    dunstify -r 34560 -u critical -i emblem-mail \
         "Mail Sync Failed" \
         "mbsync encountered an error. Check logs."

    echo "ALERT: mbsync failed with: $error_log"
    exit 1
else
    notmuch new
fi
