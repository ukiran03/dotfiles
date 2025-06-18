#!/bin/sh
#
notify-send -i "Ncmpcpp status" "Music" "Ncmpcpp $(mpc status '%state%')" -t 2000 -r 8645
