#!/usr/bin/env bash
set -euo pipefail

# Check if a target tag was provided
if [ -z "${1:-}" ]; then
	echo "Usage: $0 <target_tag>"
	exit 1
fi

TARGET_TAG="$1"
CURRENT_TAG=$(herbstclient attr tags.focus.name)

if [ "$CURRENT_TAG" = "$TARGET_TAG" ]; then
	echo "Error: Cannot swap a tag with itself."
	exit 1
fi

# Verify target tag exists
if ! herbstclient silent pad "$TARGET_TAG" 2>/dev/null && ! herbstclient tag_status | grep -q "\b$TARGET_TAG\b"; then
	echo "Error: Tag '$TARGET_TAG' does not exist."
	exit 1
fi

# 1. Dump layouts for both tags
LAYOUT_CURRENT=$(herbstclient dump "$CURRENT_TAG")
LAYOUT_TARGET=$(herbstclient dump "$TARGET_TAG")

# 2. Apply layouts inversely to swap structures and windows cleanly
# Note: herbstclient load <tag> <layout-string> sets the layout for that tag
herbstclient load "$CURRENT_TAG" "$LAYOUT_TARGET"
herbstclient load "$TARGET_TAG" "$LAYOUT_CURRENT"

echo "Successfully swapped contents and layouts between '$CURRENT_TAG' and '$TARGET_TAG'."
