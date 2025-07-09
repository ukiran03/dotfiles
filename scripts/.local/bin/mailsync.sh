#!/usr/bin/env bash

# Run mbsync and check for success
if mbsync -a; then
  # If mbsync is successful, print a colored message
  echo -e "\033[32mSyncing completed, indexing started...\033[0m"  # Green text

  # Only run notmuch new if mbsync succeeds
  notmuch new
else
  echo "mbsync failed, not running notmuch."
  exit 1
fi
