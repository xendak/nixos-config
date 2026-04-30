#!/bin/bash

# Usage: ./zej.sh [float|ver|hor]

COMMAND=$1

get_pane_id() {
  # zellij action list-panes | grep "\[$1\]" | awk '{print $1}' | tr -dc '0-9'
  zellij action list-panes | grep "$1" | awk '{print $1}' | cut -d '_' -f 2
}

case $COMMAND in
float)
  PANE_ID=$(get_pane_id "Float")
  if [ -n "$PANE_ID" ]; then
    zellij action close-pane -p "$PANE_ID"
  else
    zellij action new-pane --floating --name "Float" --width 90% --height 90% --x 5% --y 5%
  fi
  ;;

hor)
  PANE_ID=$(get_pane_id "Quake")
  if [ -n "$PANE_ID" ]; then
    zellij action close-pane -p "$PANE_ID"
  else
    zellij action new-pane --direction right --name "Quake" --width 30%
  fi
  ;;

ver)
  PANE_ID=$(get_pane_id "Scratch")
  if [ -n "$PANE_ID" ]; then
    zellij action close-pane -p "$PANE_ID"
  else
    zellij action new-pane --direction down --name "Scratch"
  fi
  ;;
esac
