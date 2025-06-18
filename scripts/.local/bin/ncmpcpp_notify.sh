#!/bin/sh

# Send notification with album art when ncmpcpp plays a new song
# execute_on_song_change must be set in ncmpcpp config

# readonly MUSIC_DIR="${HOME}/Music/Library"
# readonly MUSIC_DIR="${HOME}/Music"
# readonly SONG_PATH="$(mpc --format '%file%' current)"
# readonly SONG_DIR="$(dirname "${SONG_PATH}")"
# readonly ALBUM_ART_PATH="${MUSIC_DIR}/${SONG_DIR}/cover.jpg"

# DUNST_ICON=""

# if [[ ! -f "${ALBUM_ART_PATH}" ]]; then # file doesn't exist
# 	ALBUM_ART_PATH="" # TODO: insert path to generic icon here
# fi

dunstify -a "ignore" -i mpi-symbolic "$(mpc --format '%title% - %album%' current)" "$(mpc --format '%artist%' current)" -t 2000 -r 9036
