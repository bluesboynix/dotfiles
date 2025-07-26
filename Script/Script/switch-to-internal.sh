#!/bin/bash

# Name: switch-to-internal.sh

EXT="DP-1"
INTERNAL="eDP-1"

# Disable external, enable internal
hyprctl keyword monitor "$EXT,disable"
hyprctl keyword monitor "$INTERNAL,preferred,auto,1"
hyprctl dispatch dpms on "$INTERNAL"

