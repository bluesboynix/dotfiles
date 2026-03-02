#!/bin/bash

HIST_FILE="/tmp/waybar_cpu_hist"
PREV_FILE="/tmp/waybar_cpu_prev"
MAX=24
BLOCKS=("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█")

read cpu user nice system idle rest < /proc/stat
total=$((user+nice+system+idle))

if [ -f "$PREV_FILE" ]; then
    read ptotal pidle < "$PREV_FILE"
    diff_total=$((total-ptotal))
    diff_idle=$((idle-pidle))
    usage=$((100*(diff_total-diff_idle)/diff_total))
else
    usage=0
fi

echo "$total $idle" > "$PREV_FILE"

level=$((usage/12))
[ $level -gt 7 ] && level=7

echo -n "${BLOCKS[$level]}" >> "$HIST_FILE"
hist=$(tail -c $MAX "$HIST_FILE")
echo -n "$hist" > "$HIST_FILE"

text="──────────\nCPU\n$hist"

printf '{"text":"%s","tooltip":"CPU %s%%"}\n' "$text" "$usage"
