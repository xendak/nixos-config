#!/bin/sh

cat ~/.config/discordcanary/settings.json >> ~/.config/discordcanary/t.json
rm ~/.config/discordcanary/settings.json

discordcanary &

# Wait for Discord Canary to finish
while pgrep -x discordcanary > /dev/null; do
    sleep 1
done

rm ~/.config/discordcanary/settings.json
mv ~/.config/discordcanary/t.json ~/.config/discordcanary/settings.json
