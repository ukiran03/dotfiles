#!/usr/bin/env bash

# Shows list of manually installed packages with sizes, sorted by name (default) or size (-s)
# Generates a local cache file and displays a beautifully color-graded terminal table.

FILE="pkgsizelist2.txt"

# echo "Step 1/2: Gathering live package sizes in parallel..."

# 1. Gather clean, unversioned package names
manual_pkgs=()
while read -r pkg; do
	manual_pkgs+=("$(xbps-uhelper getpkgname "$pkg")")
done < <(xbps-query -m)

# 2. Get sizes in parallel using xargs and dump directly to the file
printf '%s\n' "${manual_pkgs[@]}" | xargs -P 4 -I {} sh -c '
    size=$(xbps-query -p installed_size "{}")
    if [ -n "$size" ]; then
        # Clean up trailing "B" from suffixes (e.g., 0B, 883KB)
        clean_size=$(echo "$size" | sed "s/\([0-9KMGT]\)B\$/\1/")
        echo "$clean_size {}"
    fi
' >"$FILE"

# echo "Step 2/2: Formatting data into a colorful table..."
# echo ""

# 3. Determine sort order based on the flag
if [[ $1 == "-s" ]]; then
	# Sort by size (largest first). Standardize to raw numbers first for
	# accurate sorting.
	SORTED_DATA=$(numfmt --from=auto --field=1 <"$FILE" | sort -n -r | numfmt --to=iec --field=1)
else
	# Default: Sort by package name alphabetically
	SORTED_DATA=$(sort -k2 "$FILE")
fi

# 4. Process sorted data and render the color-graded table via awk
echo "$SORTED_DATA" | awk '
BEGIN {
    # Print a beautiful bold header
    printf "\033[1;34m%-12s\033[1;36m%-30s\033[0m\n", "SIZE", "PACKAGE NAME";
    print "\033[1;30m------------------------------------------\033[0m";
}
{
    size_str = $1;
    pkg_name = $2;

    # Convert human-readable size to raw bytes inside awk to determine color thresholds
    val = size_str;
    unit = "";

    if (val ~ /[KMGT]$/) {
        unit = substr(val, length(val));
        val = substr(val, 1, length(val)-1);
    }

    bytes = val + 0; # coerce to number
    if (unit == "K") bytes *= 1024;
    else if (unit == "M") bytes *= 1024 * 1024;
    else if (unit == "G") bytes *= 1024 * 1024 * 1024;
    else if (unit == "T") bytes *= 1024 * 1024 * 1024 * 1024;

    # Color Threshold Brackets:
    # > 500MB = Bright Red (Highest)
    # > 50MB  = Yellow/Orange (Medium-Large)
    # > 5MB   = Cyan/Green (Normal)
    # < 5MB   = Dim Gray/Black (Lowest)
    if (bytes >= 524288000) {
        color = "\033[1;31m"; # Bright Red
    } else if (bytes >= 52428800) {
        color = "\033[0;34m"; # Blue
    } else if (bytes >= 5242880) {
        color = "\033[0;35m"; # Mage
    } else {
        color = "\033[1;36m"; # Cyan
    }

    # Print the aligned columns with applied color
    printf "%s%-12s\033[0m%-30s\n", color, size_str, pkg_name;
}'
