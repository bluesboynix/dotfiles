#!/bin/bash
# Kill any existing Conky instances
killall conky 2>/dev/null

# Wait a moment
sleep 1

# Launch each module (adjust config paths as needed)
# top left
conky -c ~/.config/conky/modules/clock.conf &
conky -c ~/.config/conky/modules/calendar.conf &
conky -c ~/.config/conky/modules/weather.conf &
conky -c ~/.config/conky/modules/todo.conf &

#top right
conky -c ~/.config/conky/modules/cpu.conf &
conky -c ~/.config/conky/modules/battery.conf &
conky -c ~/.config/conky/modules/memory.conf &
conky -c ~/.config/conky/modules/disk.conf &
conky -c ~/.config/conky/modules/network.conf &
conky -c ~/.config/conky/modules/player.conf &
#top center

#bottom center
conky -c ~/.config/conky/modules/log.conf &

echo "All Conky modules started."
