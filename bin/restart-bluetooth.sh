#!/usr/bin/env bash
set -e
set -u
PS4=" $ "

ID_VENDOR="8087"
ID_PRODUCT="0a2a"

_reset_paths() {
  for p in "$@"; do
    echo 0 >"$p"/authorized
    sleep 1
    echo 1 >"$p"/authorized
  done
}

# Main function for the script.
main() {
  echo "Resetting USB bluetooth devices."
  # Not strictly needed, but stops bluetooth.
  # It will, in any way, be started at the end.
  systemctl stop bluetooth
  sleep 4

  # Using a function allows use of local and declare.
  local p
  local found_vnd
  local found_prd
  declare -a paths

  # For all usb devices,
  for p in /sys/bus/usb/devices/*; do
    # Try to check vendor/product IDs
    found_vnd="$(cat "$p/idVendor" 2>/dev/null)" || :
    found_prd="$(cat "$p/idProduct" 2>/dev/null)" || :

    # When both match
    if [[ "$found_vnd" == "$ID_VENDOR" && "$found_prd" == "$ID_PRODUCT" ]]; then
      # Add to valid paths
      paths+=("$p")
    fi
  done

  # Reset all paths
  _reset_paths "${paths[@]}"
  sleep 1
  # Twice for good luck
  _reset_paths "${paths[@]}"

  # Waits for everything to settle down
  sleep 2

  # Restarts bluetooth.
  systemctl restart bluetooth
  echo "Done resetting USB bluetooth devices."
}

# Calls up main.
main "$@"
