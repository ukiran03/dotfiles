#!/usr/bin/env bash

# Commentry: kill the High Demanding Process using rofi menu

menu=$(ps -U $USER -o pid,%mem,%cpu,comm | sort -b -k2 -r | sed -n '1!p' | rofi -dmenu)
pid=$(echo $menu | awk '{print $1}')
command=$(echo $menu | awk '{for (i=4; i<=NF; i++) printf "%s ", $i; printf "\n"}')

kill -15 $pid 2>/dev/null

dunstify -a "ignore" -i computer-fail $command "Process Killed" -r 3242
