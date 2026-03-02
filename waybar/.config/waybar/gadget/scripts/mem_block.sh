#!/bin/bash

BLOCKS=("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█")

usage=$(free | awk '/Mem:/ {printf "%.0f", $3/$2*100}')

level=$((usage/12))
[ $level -gt 7 ] && level=7

bar="${BLOCKS[$level]}"

text="──────────\nMEMORY\n$bar\n$usage%"

printf '{"text":"%s","tooltip":"RAM %s%%"}\n' "$text" "$usage"
