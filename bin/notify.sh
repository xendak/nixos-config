#!/usr/bin/env bash
# Usage: ./notify.sh {time|bat}

case "$1" in
time)
  # Fetch current time components using date
  TIME=$(date +%H:%M)
  DAY=$(date +%d%a)
  MONTH=$(date +%m)
  MSG="${TIME} ${DAY} ${MONTH}月"

  notify-send -i clock -u normal "Time" "$MSG"
  ;;

bat)
  # Charging status check (exactly as you specified)
  if [[ -f /sys/class/power_supply/BAT0/status ]]; then
    is_charging=$(head -c 1 /sys/class/power_supply/BAT0/status)
  else
    is_charging="D" # Fallback if file doesn't exist
  fi

  if [[ "$is_charging" == "C" ]]; then
    BAT_ICON="battery-charging"
    STATUS="Charging"
  else
    BAT_ICON="battery"
    STATUS="Discharging"
  fi

  # Get percentage (exactly as you specified)
  PERCENT=$(acpi -b | awk '{print $4}' | tr -d '%,')
  # Safety fallback if acpi returns empty/invalid
  [[ ! "$PERCENT" =~ ^[0-9]+$ ]] && PERCENT=0

  # Build 5-segment battery bar
  BLOCKS=$(((PERCENT + 19) / 20))
  [[ $BLOCKS -gt 5 ]] && BLOCKS=5
  BAR=""
  for ((i = 1; i <= 5; i++)); do
    if ((i <= BLOCKS)); then
      BAR+="█"
    else
      BAR+="░"
    fi
  done

  MSG="${PERCENT}% ${BAR}"
  notify-send -i "$BAT_ICON" -u normal "$STATUS" "$MSG"
  ;;

*)
  echo "Usage: $0 {time|bat}"
  exit 1
  ;;
esac
