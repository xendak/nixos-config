#!/bin/sh

ACTIVE_WORKSPACE=$(niri msg -j workspaces | jq -r '.[] | select(.is_active) | .name')
if [ "$ACTIVE_WORKSPACE" = "5" ]; then
 exit 0
fi

if niri msg -j windows | jq -e 'any(.[]?; (.app_id // "") | test("steam_app_"))' >/dev/null 2>&1; then
 exit 0
fi

exec "$@"
