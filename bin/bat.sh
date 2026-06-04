#!/bin/bash

# Battery monitor script
# Notifies when battery is below 20%, then every 5 minutes below 10% until death

CHECK_INTERVAL=60
LAST_20_NOTIFY=0
MINUTE_MARKER=0
NOTIFY_WHEN_START=0

# i wish this worked... fk mac?
get_battery_percentage() {
  if command -v upower &>/dev/null; then
    upower -b 2>/dev/null | grep "percentage" | awk '{print $3}' | tr -d '%'
  else    # Fallback to acpi
    acpi -b 2>/dev/null | awk '{print $4}' | tr -d "%,"
  fi
}

send_notification() {
  local title="$1"
  local message="$2"

  if command -v notify-send &>/dev/null; then
    notify-send "$title" "$message" -u normal
  else
    echo "[$(date)] $title: $message" >>/tmp/battery_notifications.log
  fi
}

case "$(pidof dunst | wc -l)" in
0)
  dunst &
  ;;
1)
  echo "restarting dunst"
  pkill dunst
  dunst &
  ;;
esac

# Main monitoring loop
while true; do
  # Get current battery percentage
  is_charging=$(head -c 1 /sys/class/power_supply/BAT0/status)

  if [ "$is_charging" = "C" ]; then
    continue
  fi

  PERCENT=$(acpi -b | awk '{print $4}' | tr -d '%,')


  if [ -z "$PERCENT" ] || ! [[ "$PERCENT" =~ ^[0-9]+$ ]]; then
    echo "[$(date)] Warning: Could not determine battery level" >>/tmp/battery_monitor.log
    # echo "[$PERCENT] Warning: Could not determine battery level" >>/tmp/battery_monitor.log
    sleep $CHECK_INTERVAL
    continue
  fi

  if [ $NOTIFY_WHEN_START -eq 0 ]; then
      send_notification "Battery" "Battery is at ${PERCENT}%"
      NOTIFY_WHEN_START=1
  fi

  if [ "$PERCENT" -lt 20 ]; then
    if [ $LAST_20_NOTIFY -eq 0 ]; then
      send_notification "Battery Low" "Battery is at ${PERCENT}%. Please connect your charger."
      LAST_20_NOTIFY=1
    fi
  else
    LAST_20_NOTIFY=0
  fi

  if [ "$PERCENT" -lt 10 ]; then
    MINUTE_MARKER=$((MINUTE_MARKER + (CHECK_INTERVAL / 60)))

    if [ $((MINUTE_MARKER % 5)) -eq 0 ]; then
      send_notification "Critical Battery" "Battery is at ${PERCENT}%. You have approximately $(echo "scale=1; $PERCENT * 2 / 10" | bc) minutes left. Save your work!"
    fi
  else
    MINUTE_MARKER=0
  fi

  sleep $CHECK_INTERVAL
done
