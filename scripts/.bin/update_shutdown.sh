#!/usr/bin/env bash

# Update the system
echo "Updating the system..."
sudo pacman -Syyu --noconfirm

# Check if the update was successful
if [ $? -eq 0 ]; then
    echo "Update completed successfully."
    echo "Shutting down the system..."
    sudo shutdown now
else
    echo "Update failed. Please check the output above."
fi
