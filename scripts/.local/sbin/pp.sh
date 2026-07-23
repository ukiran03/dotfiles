#!/usr/bin/env bash

# Check if an argument was provided
if [ $# -eq 0 ]; then
    echo "Error: No file specified." >&2
    echo "Usage: pp <filename>" >&2
    exit 1
fi

FILE="$1"

# Check if the file exists
if [ ! -e "$FILE" ]; then
    echo "Error: File '$FILE' does not exist." >&2
    exit 1
fi

# Check if it's a regular file (not a directory)
if [ ! -f "$FILE" ]; then
    echo "Error: '$FILE' is not a regular file." >&2
    exit 1
fi

# Print the header with the filename, followed by its contents
echo "=== File: $FILE ==="
cat "$FILE"
