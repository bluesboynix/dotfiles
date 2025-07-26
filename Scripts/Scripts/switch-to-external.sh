#!/bin/bash

# Name: switch-to-external.sh

# Wait a bit to allow hotplug to finish
sleep 1

# External monitor name
EXT="DP-1"
INTERNAL="eDP-1"

# Check if external monitor is connected
if hyprctl monitors | grep -q "$EXT"; then
    # Set external monitor to primary, disable internal
    hyprctl dispatch dpms off "$INTERNAL"
    hyprctl keyword monitor "$INTERNAL,disable"
    hyprctl keyword monitor "$EXT,preferred,auto,1"
else
    # External not connected, fallback to internal only
    hyprctl keyword monitor "$EXT,disable"
    hyprctl keyword monitor "$INTERNAL,preferred,auto,1"
    hyprctl dispatch dpms on "$INTERNAL"
fi

