#!/usr/bin/env bash
KAK_SESSION=$1

# Look for an existing "Kak" pane
TARGET_PANE_ID=$(zellij action list-panes | grep "Kak" | awk '{print $1}' | cut -d '_' -f 2)
ORIGINAL_PANE_ID=$(zellij action list-panes | grep "Editor" | awk '{print $1}' | cut -d '_' -f 2)

if [ -z "$TARGET_PANE_ID" ]; then
  # No Kak pane exists, spawn one
  zellij run --name "Kak" --direction right --close-on-exit -- \
    kak -c "$KAK_SESSION" -e 'buffer *make*'
  zellij action focus-pane-id "$ORIGINAL_PANE_ID"
else
  # Reuse existing pane — tell that kak client to switch to *make*
  zellij action focus-pane-id "$TARGET_PANE_ID"
  zellij action write-chars ": buffer *make*" --pane-id "$TARGET_PANE_ID"
  zellij action write 13 --pane-id "$TARGET_PANE_ID"
  zellij action focus-pane-id "$ORIGINAL_PANE_ID"
fi
