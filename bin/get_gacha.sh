#!/bin/sh
nix-shell -p nixpkgs#bintool -c strings $HOME/.local/share/honkers-railway-launcher/HSR/StarRail_Data/webCaches/Cache/Cache_Data/data_2 | grep -oe 'https://.*/e.*gacha-v2/.*' | tail -1 | wl-copy
