#!/usr/bin/env bash

# Commentry: kill the High Demanding Process using rofi menu

menu=$(ps -U $USER --no-headers --sort=-pcpu -o comm,%mem,%cpu,pid | rofi -dmenu -p "kill proc")
pid=$(echo $menu | awk '{print $NF}')
command=$(ps --no-headers -o comm -p $pid)

kill -15 $pid 2>/dev/null && dunstify -a "ignore" -i computer-fail $command "Process Killed" -r 3242
