#!/bin/bash
COUT=$( pactl -- get-default-sink )
RAZER="alsa_output.usb-Razer_Razer_Nari_Essential-00.analog-stereo"
if [ "$COUT" == "$RAZER" ]; then
    echo "Headset"
else
    echo "Speakers"
fi
