#!/usr/bin/env bash

# Add this script to your wm startup file.

DIR="$HOME/.config/polybar/hack"

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

desktop=$(echo $DESKTOP_SESSION)

case $desktop in
i3)
    if type "xrandr" >/dev/null; then
        for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
            if [ $m == 'eDP1' ]; then
                MONITOR=$m polybar --reload -q top -c "$DIR"/config.ini &
                MONITOR=$m polybar --reload -q bottom -c "$DIR"/config.ini &
            elif [ $m == 'HDMI1' ]; then
                MONITOR=$m polybar --reload -q top -c "$DIR"/config.ini &
                MONITOR=$m polybar --reload -q bottom -c "$DIR"/config.ini &
            else
                MONITOR=$m polybar --reload -q top -c "$DIR"/config.ini &
                MONITOR=$m polybar --reload -q bottom -c "$DIR"/config.ini &
            fi
        done
    else
        MONITOR=$m polybar --reload -q top -c "$DIR"/config.ini &
        MONITOR=$m polybar --reload -q bottom -c "$DIR"/config.ini &
    fi
    ;;
openbox)
    if type "xrandr" >/dev/null; then
        for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
            MONITOR=$m polybar --reload mainbar-openbox -c ~/.config/polybar/config &
        done
    else
        polybar --reload mainbar-openbox -c ~/.config/polybar/config &
    fi
    ;;
bspwm)
    if type "xrandr" >/dev/null; then
        for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
            MONITOR=$m polybar --reload mainbar-bspwm -c ~/.config/polybar/config &
        done
    else
        polybar --reload mainbar-bspwm -c ~/.config/polybar/config &
    fi
    ;;
esac
