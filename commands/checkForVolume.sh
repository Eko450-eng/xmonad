#!/bin/bash
COUT=$( pactl -- get-default-sink )
OUTPUT=$( pactl list sinks | awk '$1=="Volume:" {print $5}' )
RAZER="alsa_output.usb-Razer_Razer_Nari_Essential-00.analog-stereo"
if [ "$COUT" == "$RAZER" ]; then
    echo "$OUTPUT" | sed -n '2 p'
else
    echo "$OUTPUT" | sed -n '1 p'
fi
