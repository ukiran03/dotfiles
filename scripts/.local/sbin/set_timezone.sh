#!/usr/bin/env bash

# Exit immediately if any command fails (excluding the reboot prompt)
set -e

echo "Updating system timezone to Asia/Kolkata..."

# Remove old localtime
sudo rm -f /etc/localtime

# Create symbolic link for the new timezone
sudo ln -sf /usr/share/zoneinfo/Asia/Kolkata /etc/localtime

# Sync the hardware clock with the system clock
sudo hwclock --systohc

echo "Timezone updated successfully."

# Ask for confirmation before rebooting
read -p "Do you want to reboot the system now? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	echo "Rebooting system..."
	sudo reboot
else
	echo "Reboot skipped. Please remember to reboot later for changes to fully take effect."
fi
