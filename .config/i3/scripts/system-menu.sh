#!/usr/bin/env bash

# Catppuccin Colors
BG="#1e1e2e"
TEXT="#cdd6f4"
ACCENT="#cba6f7"

# Menu with icons
options=(
    "  Lock"
    "  Logout"
    "  Suspend"
    "  Hibernate"
    "  Reboot"
    "  Shutdown"
)

# Show menu
choice=$(printf '%s\n' "${options[@]}" | rofi -dmenu \
    -p "  Menu" \
    -theme-str "
        * {
            font: \"DejaVu Sans 18\";
            background: $BG;
            text-color: $TEXT;
        }
        window {
            width: 300px;
            height: 300px;
            border: 4px solid;
            border-radius: 10px;
        }
        element {
            padding: 4px;
        }
        element selected {
            background-color: $ACCENT;
            text-color: $BG;
        }
    ")

# Handle choice
case "$choice" in
"  Lock") ~/.config/i3/scripts/lock ;;
"  Logout") i3-msg exit ;;
"  Suspend") ~/.config/i3/scripts/lock && systemctl suspend ;;
"  Hibernate") ~/.config/i3/scripts/lock && systemctl hibernate ;;
"  Reboot") systemctl reboot ;;
"  Shutdown") systemctl poweroff ;;
esac
