#!/usr/bin/env bash
exec 2>&1

if ! error_log=$(mbsync -a 2>&1); then
        # Send a critical desktop alert that stays on screen until dismissed
    dunstify -r 34560 -u critical -i emblem-mail \
             "Mail Sync Failed" \
             "mbsync encountered an error. Check logs."

    echo "ALERT: mbsync failed with: $error_log"
    exit 1
else
    # If successful, run notmuch indexing and exit
    notmuch new
fi
