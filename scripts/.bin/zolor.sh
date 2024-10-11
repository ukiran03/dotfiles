#!/bin/bash

DIR="$XDG_CONFIG_HOME/zathura"
# DIR="/home/ukiran/tmp/test"

for file in "$DIR"/*; do
    # Check of the .wt file exists
    if [[ "$file" == *.wt ]]; then
	# Get the base name of the file without extension
	base_name="${file%.wt}"

	# Check if the base file (without extension) exists
	if [ -f "$base_name" ]; then
	    # Move the base file to base.gr
	    mv "$base_name" "$base_name.gr"

	    # Move the .wt file to the base file name
	    mv "$file" "$base_name"
	fi

    elif [[ "$file" = *.gr ]]; then
	base_name="${file%.gr}"

	# Check if the base file (without extension) exists
	if [ -f "$base_name" ]; then
	    # Move the base file to base.gr
	    mv "$base_name" "$base_name.wt"
	    
	    # Move the .gr file to the base file name
	    mv "$file" "$base_name"
	fi
    fi
    
done

