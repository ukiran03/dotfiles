#!/bin/bash

# Video 1 details
width1=852
height1=460
bitrate1=823

# Video 2 details
width2=640
height2=352
bitrate2=1144

# Video 3 details
width3=640
height3=352
bitrate3=1144

# Function to calculate a score based on criteria
calculate_score() {
	resolution_score=$(( $1 * $2 ))
	bitrate_score=$3
	echo $(( resolution_score * bitrate_score ))
}

# Calculate scores for each video
score1=$(calculate_score $width1 $height1 $bitrate1)
score2=$(calculate_score $width2 $height2 $bitrate2)
score3=$(calculate_score $width3 $height3 $bitrate3)

# Determine the video with the highest score
if [ $score1 -ge $score2 ] && [ $score1 -ge $score3 ]; then
	echo "Video 1 is the best."
	# Remove Video 2 and Video 3
	# rm video2.mp4 video3.mp4
elif [ $score2 -ge $score1 ] && [ $score2 -ge $score3 ]; then
	echo "Video 2 is the best."
	# Remove Video 1 and Video 3
	# rm video1.mp4 video3.mp4
else
	echo "Video 3 is the best."
	# Remove Video 1 and Video 2
	# rm video1.mp4 video2.mp4
fi
