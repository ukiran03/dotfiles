#!/usr/bin/env bash

for vid in *; do
    [[ "$vid" =~ \.(mkv|webm|mp4)$ ]] || continue
    echo "$vid"
    mediainfo "$vid" | grep -i purl | awk '{print $3, "\n"}'
done
